############################################################################## #
# Filename
#    32_balancing_prediction.R
#
# Description
#   Balancing of Prediction period
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      20xx-xx-xx
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #
source("00_init.R")
source("30_GCE_algorithm.R")
print("start")
load(file.path(wd_data_work, "all_municipalities_population.RData"))

allowed_deviation_pred <- all_munip_pop %>%
  filter(year %in% 2013:2024) %>%
  select(municipality_code, year, population) %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population, na.rm = T)) %>%
  mutate(# Get the population in 2024 for the current group
    population_2024 = population[year == 2024],
    # Calculate the percentage change compared to 2024
    percentage_change = ((population_2024 - population) / population) * 100) %>%
  ungroup() %>%
  mutate(
    population_size_group = case_when(
      population_2024 < 500 ~ "< 500",
      population_2024 >= 500 & population_2024 < 1000 ~ "500-1000",
      population_2024 >= 1000 &
        population_2024 <= 2000 ~ "1000-2000",
      population_2024 > 2000 &
        population_2024 <= 5000 ~ "2000-5000",
      population_2024 > 5000 &
        population_2024 <= 20000 ~ "5000-20000",
      population_2024 > 20000 &
        population_2024 <= 50000 ~ "20000-50000", 
      population_2024 > 50000 ~ "> 50000",
      TRUE ~ NA_character_
    )
  )


# export size group mapping ----------------------------------------------------
 municipality_size_group_mapping_2024 <- allowed_deviation_pred %>%
   select(municipality_code, population_size_group) %>%
   distinct(municipality_code, .keep_all = TRUE)
#
# save(municipality_size_group_mapping_2021,
#      file = file.path(wd_data_work, "munip_size_group_mapping_2021.RData"))
# ------------------------------------------------------------------------------
allowed_deviation_pred <- allowed_deviation_pred %>%
  #filter(year != 2021) %>%
  ungroup() %>% group_by(year, population_size_group) %>%
  summarise(
    min_percentage_change = min(percentage_change, na.rm = TRUE),
    max_percentage_change = max(percentage_change, na.rm = TRUE)
  ) %>%
  mutate(prediction_period = paste("pred_period", 2024 - as.numeric(year), "y", sep = "_")) %>%
  ungroup() %>%
  relocate(prediction_period, .before = population_size_group)

rm(all_munip_pop)



# GCE balancing ----------------------------------------------------------------

## function that calls GCE -----------------------------------------------------
#' Balance Population Predictions Using Generalized Cross Entropy
#'
#' Applies a Generalized Cross Entropy (GCE) balancing procedure to a wide-format
#' prediction matrix, enforcing row constraints (e.g., regional age-sex population totals)
#' and column bounds (e.g., municipality-level uncertainty ranges). The function returns
#' the original dataset augmented with a new column of balanced predictions.
#'
#' @param data A data frame containing demographic prediction data. and grouped by `reg_code`, `year`.
#'      Must include the following columns:
#'   - `municipality_code`: identifier for municipalities (columns in the prediction matrix),
#'   - `sex_age_cohort`: identifier for age-sex cohort (rows in the matrix),
#'   - `PRED_tuned_LINEXP`: predicted population values,
#'   - `projected_population`: projected population values used for row constraints,
#'   - `min_percentage_change` / `max_percentage_change`: bounds used for column constraints.
#'
#' @param M Integer (default = 3). Number of support vectors to use in the GCE optimization.
#'
#' @param prior Character string indicating the type of prior to use in GCE. Default is `"spike"`.
#'
#' @return A data frame identical to the input `data`, but with an additional column:
#'   - `balanced_pred`: the GCE-balanced population prediction.
#'
#' @details
#' The function constructs a wide matrix of population predictions, generates support vectors,
#' and computes row and column constraints. The `balance_matrix()` function is then used to
#' perform the balancing using GCE. The final balanced predictions are merged back into the
#' original dataset in long format.
balance_prediction <- function(data, M = 3, prior = "spike", pred_col_name) {
  pred_col <-  ensym(pred_col_name)
  
  init_matrix <- data %>%
    ungroup() %>%
    select(municipality_code, sex_age_cohort, !!pred_col) %>%
    pivot_wider(id_cols = sex_age_cohort,
                names_from = municipality_code,
                values_from = !!pred_col) %>%
    column_to_rownames(var = "sex_age_cohort") %>%
    as.matrix()
  
  print(colnames(init_matrix)[1])
  
  support_vectors <- generate_support_vectors(init_matrix, M = M)
  
  
  row_names <- rownames(init_matrix)
  col_names <- colnames(init_matrix)
  
  get_bounds <- function(data, pred_col) {
    bounds_df <- data %>%
      group_by(municipality_code,
               min_percentage_change,
               max_percentage_change) %>%
      summarise(!!pred_col := sum(!!pred_col),
                .groups = 'drop') %>%
      mutate(
        lower_bound = !!pred_col * (1 + min_percentage_change / 100),
        upper_bound = !!pred_col * (1 + max_percentage_change / 100)
      ) %>%
      group_by(municipality_code) %>%
      summarise(
        lower_bound = sum(lower_bound),
        upper_bound = sum(upper_bound),
        .groups = 'drop'
      )
    
    lower_bounds_named <- bounds_df$lower_bound
    names(lower_bounds_named) <- bounds_df$municipality_code
    
    upper_bounds_named <- bounds_df$upper_bound
    names(upper_bounds_named) <- bounds_df$municipality_code
    
    return_list <- list(lower_bounds = lower_bounds_named, upper_bounds = upper_bounds_named)
    return(return_list)
  }
  
  
  bounds <- get_bounds(data, pred_col)
  lower_bounds <- bounds$lower_bounds[col_names]
  upper_bounds <- bounds$upper_bounds[col_names]
  
  
  get_row_constraints <- function(data) {
    data <- data %>% group_by(sex_age_cohort) %>%
      distinct(projected_population, .keep_all = TRUE) %>%
      summarise(population = sum(projected_population))
    
    result_names <- data %>% pull(sex_age_cohort)
    result_values <- data %>% pull(population)
    names(result_values) <- result_names
    
    return(result_values)
  }
  
  row_constraints <- get_row_constraints(data)[row_names]
  
  
  out_matrix <- balance_matrix(
    init_matrix,
    u = row_constraints,
    v_lower = lower_bounds,
    v_upper = upper_bounds,
    support_vectors = support_vectors,
    M = M,
    prior = prior
  )
  
  balanced_matrix <- out_matrix$X_estimated
  rownames(balanced_matrix) <- row_names
  colnames(balanced_matrix) <- col_names
  out_matrix_long <- as_tibble(balanced_matrix, rownames = "sex_age_cohort") %>%
    pivot_longer(
      cols = -sex_age_cohort,
      names_to = "municipality_code",
      values_to = "balanced_pred"
    )
  
  data <-  data %>%
    left_join(out_matrix_long, by = join_by(municipality_code, sex_age_cohort))
  return(data)
}



#' Prepare LINEXP Predictions for Balancing
#'
#' This function transforms tuned LINEXP prediction data by filtering for the selected years,
#' merging in regional and size mappings, applying join logic for deviation and district projections,
#' and grouping the data in preparation for balancing. It also unites the sex and age group columns
#' into a new column called "sex_age_cohort".
#'
#' @param prediction_data A data frame containing the tuned LINEXP predictions.
#' @param municipality_reg_mapping A data frame mapping municipalities to regions.
#' @param municipality_size_group_mapping A data frame mapping municipalities to size groups.
#' @param allowed_deviation A data frame with allowed deviation information.
#' @param district_projection A data frame with district-level projections.
#' @param filter_years Numeric vector of years to filter on (default: 2022:2024).
#'
#' @return A data frame with the prepared predictions for balancing.
prepare_prediction_for_balancing <- function(prediction_data,
                                             municipality_reg_mapping,
                                             municipality_size_group_mapping,
                                             allowed_deviation,
                                             district_projection,
                                             prediction_years = 2022:2024) {
  result <- prediction_data %>%
    filter(year %in% prediction_years) %>%
    left_join(municipality_reg_mapping, by = "municipality_code") %>%
    left_join(municipality_size_group_mapping, by = "municipality_code") %>%
    select(-population) %>%
    mutate(join_year = jump_off_year - (year - jump_off_year)) %>%
    left_join(allowed_deviation,
              by = join_by(join_year == year, population_size_group)) %>%
    select(-join_year, -population_size_group, -prediction_period) %>%
    mutate(sex = as.numeric(sex)) %>%
    left_join(
      district_projection,
      by = join_by(
        reg_code == district_identifier,
        sex == sex,
        age_group == coarse_age_group,
        year == year
      )
    ) %>%
    ungroup() %>%
    group_by(year, reg_code) %>%
    unite("sex_age_cohort", c("sex", "age_group"), remove = FALSE)
  
  return(result)
}




## LINEXP prediction -----------------------------------------------------------
load(file.path(wd_res, "25-35_LINEXP_prediction.RData"))
load(file.path(wd_data_work, "municipality_code_reg_code_mapping.RData"))
#load(file.path(wd_data_work, "munip_size_group_mapping_2021.RData"))
load(file.path(wd_data_work, "district_projection.RData"))

jump_off_year <- 2024

# data on which the function is applied
LINEXP_pred_for_balancing <- prepare_prediction_for_balancing(
  tuned_LINEXP_pred_export,
  municipality_reg_mapping,
  municipality_size_group_mapping_2024,
  allowed_deviation_pred,
  district_projection,
  prediction_years = 2025:2035
)

balanced_LINEXP_pred <- LINEXP_pred_for_balancing %>%
  group_by(year, reg_code) %>%
  group_modify(~ balance_prediction(.x, pred_col_name = "PRED_tuned_LINEXP")) %>%
  select(municipality_code, reg_code, sex, age_group, year, PRED_tuned_LINEXP, balanced_pred) 



save(balanced_LINEXP_pred, file = file.path(wd_res, "2025-2035_LINEXP_balanced.RData"))
print("LINEXP finished")
## hamilton-perry --------------------------------------------------------------
load(file.path(wd_res, "25-35_HP_prediction.RData"))

hp_pred_for_balancing <- prepare_prediction_for_balancing(
  hp_pred_export,
  municipality_reg_mapping,
  municipality_size_group_mapping_2024,
  allowed_deviation_pred,
  district_projection,
  prediction_years = 2025:2035
)

balanced_hp_pred <- hp_pred_for_balancing %>%
  group_by(year, reg_code) %>%
  group_modify(~ balance_prediction(.x, pred_col_name = "PRED_hamilton_perry")) %>%
  select(municipality_code, reg_code, sex, age_group, year, PRED_hamilton_perry, balanced_pred)

save(balanced_hp_pred, file = file.path(wd_res, "2025-2035_HP_balanced.RData"))
print("HP finished")