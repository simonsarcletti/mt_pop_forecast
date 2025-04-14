############################################################################## #
# Filename
#    01_balancing_framework.R
#
# Description
#   Matrix balancing with GCE framework
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      20xx-xx-xx
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #

# allowed deviation ------------------------------------------------------------
# calculate max/min allowed deviation from historical data
# What is the max/min allowed population change over 1-10 years?
load(file.path(wd_data_work, "all_municipalities_population.RData"))
load(file.path(wd_data_work, "district_projection.RData"))

allowed_deviation <- all_munip_pop %>%
  filter(year %in% 2011:2021) %>%
  select(municipality_code, year, population) %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population, na.rm = T)) %>%
  mutate(# Get the population in 2024 for the current group
    population_2021 = population[year == 2021],
    # Calculate the percentage change compared to 2024
    percentage_change = ((population_2021 - population) / population) * 100) %>%
  ungroup() %>%
  mutate(
    population_size_group = case_when(
      population_2021 < 500 ~ "< 500",
      population_2021 >= 500 & population_2021 < 1000 ~ "500-1000",
      population_2021 >= 1000 &
        population_2021 <= 2000 ~ "1000-2000",
      population_2021 > 2000 &
        population_2021 <= 5000 ~ "2000-5000",
      population_2021 > 5000 &
        population_2021 <= 10000 ~ "5000-10000",
      population_2021 > 10000 ~ "> 10000",
      TRUE ~ NA_character_
    )
  )


# export size group mapping ----------------------------------------------------
#municipality_size_group_mapping_2021 <- allowed_deviation %>%
#  select(municipality_code, population_size_group) %>%
#  distinct(municipality_code, .keep_all = TRUE)

#save(municipality_size_group_mapping,
#     file = file.path(wd_data_work, "munip_size_group_mapping_2021.RData"))
# ------------------------------------------------------------------------------



allowed_deviation <- allowed_deviation %>%
  filter(year != 2021) %>%
  ungroup() %>% group_by(year, population_size_group) %>%
  summarise(
    min_percentage_change = min(percentage_change, na.rm = TRUE),
    max_percentage_change = max(percentage_change, na.rm = TRUE)
  ) %>%
  mutate(prediction_period = paste("pred_period", 2021 - as.numeric(year), "y", sep = "_")) %>%
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
balance_prediction <- function(data, M = 3, prior = "spike") {
  init_matrix <- data %>%
    ungroup() %>%
    select(municipality_code, sex_age_cohort, PRED_tuned_LINEXP) %>%
    pivot_wider(id_cols = sex_age_cohort,
                names_from = municipality_code,
                values_from = PRED_tuned_LINEXP) %>%
    column_to_rownames(var = "sex_age_cohort") %>%
    as.matrix()
  
  support_vectors <- generate_support_vectors(init_matrix, M = M)
  
  
  row_names <- rownames(init_matrix)
  col_names <- colnames(init_matrix)
  
  get_bounds <- function(data) {
    bounds_df <- data %>%
      group_by(municipality_code,
               min_percentage_change,
               max_percentage_change) %>%
      summarise(PRED_tuned_LINEXP = sum(PRED_tuned_LINEXP),
                .groups = 'drop') %>%
      mutate(
        lower_bound = PRED_tuned_LINEXP * (1 + min_percentage_change / 100),
        upper_bound = PRED_tuned_LINEXP * (1 + max_percentage_change / 100)
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
  
  
  bounds <- get_bounds(data)
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



## data frame preparation ------------------------------------------------------
# start with LINEXP prediction
load(file.path(wd_res, "final_LINEXP_prediction.RData"))
load(file.path(wd_data_work, "municipality_code_reg_code_mapping.RData"))
municipality_reg_mapping$reg_code <- ifelse(
  municipality_reg_mapping$reg_code <= 1000,
  municipality_reg_mapping$reg_code * 10,
  municipality_reg_mapping$reg_code
)


load(file.path(wd_data_work, "munip_size_group_mapping_2021.RData"))



if (colnames(tuned_LINEXP_pred_export)[1] == "index") {
  LINEXP_pred_for_balancing <- tuned_LINEXP_pred_export %>%
    separate(index,
             into = c("municipality_code", "sex", "age_group"),
             sep = "_")
}

# data on which the function is applied
LINEXP_pred_for_balancing <- LINEXP_pred_for_balancing %>%
  filter(year %in% 2022:2024) %>%
  left_join(municipality_reg_mapping) %>%
  left_join(municipality_size_group_mapping_2021) %>%
  select(-c(population, smoothed_population))

LINEXP_pred_for_balancing <- LINEXP_pred_for_balancing %>%
  mutate(join_year = as.character(as.numeric(year) - 2)) %>%
  left_join(allowed_deviation,
            by = join_by(join_year == year, population_size_group)) %>%
  select(-c(join_year, population_size_group, prediction_period)) %>%
  #mutate(sex = case_when(sex == "männlich" ~ 1, sex == "weiblich" ~ 2)) %>%
  left_join(
    district_projection,
    by = join_by(
      reg_code == district_identifier,
      sex,
      age_group == coarse_age_group,
      year
    )
  ) %>%
  ungroup() %>% group_by(year, reg_code) %>%
  unite("sex_age_cohort", c("sex", "age_group"), remove = FALSE)





balanced_LINEXP_pred_ <- LINEXP_pred_for_balancing %>%
  group_by(year, reg_code) %>%
  group_modify( ~ balance_prediction(.x))
