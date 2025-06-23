############################################################################## #
# Filename
#    33_balancing_HP_for_bl.R
#
# Description
#   Balance HP prediction with bl projection
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      20xx-xx-xx
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #

linux <- TRUE

#source("00_init.R")
source("30_GCE_algorithm.R")

if (!require("nloptr")) {
  install.packages("nloptr")
}
if (!require("tidyr")) {
  install.packages("tidyr")
}
if (!require("dplyr")) {
  install.packages("dplyr")
}
if (!require("rlang")) {
  install.packages("rlang")
}
if (!require("tibble")) {
  install.packages("tibble")
}

if (linux) {
  load("/data/simon/all_municipalities_population_2025.RData")
} else {
  load(file.path(wd_data_work, "all_municipalities_population_2025.RData"))
}
all_munip_pop <- all_munip_pop_2025
rm(all_munip_pop_2025)
if (linux) {
  load("/data/simon/municipality_code_reg_code_mapping.RData")
} else {
  load(file.path(wd_data_work, "municipality_code_reg_code_mapping.RData"))
}


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
balance_prediction <- function(data,
                               M = 3,
                               prior = "uniform",
                               pred_col_name) {
  pred_col <-  ensym(pred_col_name)
  
  init_matrix <- data %>%
    ungroup() %>%
    select(municipality_code, sex_age_cohort, !!pred_col) %>%
    pivot_wider(
      id_cols = sex_age_cohort,
      names_from = municipality_code,
      values_from = !!pred_col
    ) %>%
    column_to_rownames(var = "sex_age_cohort") %>%
    as.matrix()
  
  print(colnames(init_matrix)[1])
  print(data$year[1])
  
  support_vectors <- generate_support_vectors(init_matrix,
                                              M = M,
                                              years_passed = data$year[1] - jump_off_year)
  
  
  row_names <- rownames(init_matrix)
  col_names <- colnames(init_matrix)
  
  get_bounds <- function(data, pred_col) {
    bounds_df <- data %>%
      group_by(municipality_code,
               min_percentage_change,
               max_percentage_change) %>%
      summarise(!!pred_col := sum(!!pred_col), .groups = 'drop') %>%
      mutate(
        lower_bound = !!pred_col * (1 + min_percentage_change * 2 / 100),
        upper_bound = !!pred_col * (1 + max_percentage_change * 2 / 100)
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
  
  print(sum(lower_bounds))
  print(sum(row_constraints))
  print(sum(upper_bounds))
  
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
  
  data <- select(data, -bl_code, -year) %>%
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
                                             prediction_years = 2026:2035) {
  result <- prediction_data %>%
    filter(year %in% prediction_years) %>%
    mutate(bl_code = substr(municipality_code, 1, 1)) %>%
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
        bl_code == bundesland_code,
        sex == sex,
        age_group == age_group,
        year == year
      )
    ) %>%
    ungroup() %>%
    group_by(year, bl_code) %>%
    unite("sex_age_cohort", c("sex", "age_group"), remove = FALSE)
  
  return(result)
}

# Prepare allowed deviation data -----------------------------------------------
allowed_deviation_pred <- all_munip_pop %>%
  filter(year %in% 2015:2025) %>%
  select(municipality_code, year, population) %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population, na.rm = T)) %>%
  mutate(
    # Get the population in 2024 for the current group
    population_2025 = population[year == 2025],
    # Calculate the percentage change compared to 2024
    percentage_change = ((population_2025 - population) / population) * 100
  ) %>%
  ungroup() %>%
  mutate(
    population_size_group = case_when(
      population_2025 < 500 ~ "< 500",
      population_2025 >= 500 & population_2025 < 1000 ~ "500-1000",
      population_2025 >= 1000 &
        population_2025 <= 2000 ~ "1000-2000",
      population_2025 > 2000 &
        population_2025 <= 5000 ~ "2000-5000",
      population_2025 > 5000 &
        population_2025 <= 20000 ~ "5000-20000",
      population_2025 > 20000 &
        population_2025 <= 50000 ~ "20000-50000",
      population_2025 > 50000 ~ "> 50000",
      TRUE ~ NA_character_
    )
  )


# export size group mapping ----------------------------------------------------
municipality_size_group_mapping_2025 <- allowed_deviation_pred %>%
  select(municipality_code, population_size_group) %>%
  distinct(municipality_code, .keep_all = TRUE)
#
# save(municipality_size_group_mapping_2024,
#     file = file.path(wd_data_work, "munip_size_group_mapping_2024.RData"))
# ------------------------------------------------------------------------------
allowed_deviation_pred <- allowed_deviation_pred %>%
  #filter(year != 2021) %>%
  ungroup() %>% group_by(year, population_size_group) %>%
  summarise(
    min_percentage_change = min(percentage_change, na.rm = TRUE),
    max_percentage_change = max(percentage_change, na.rm = TRUE)
  ) %>%
  mutate(prediction_period = paste("pred_period", 2025 - as.numeric(year), "y", sep = "_")) %>%
  ungroup() %>%
  relocate(prediction_period, .before = population_size_group)

rm(all_munip_pop)



if (linux) {
  bl_projection <- read.csv2("/data/simon/file_balancing_bl.csv", sep = ";") %>%
    mutate(bundesland_code = as.character(bundesland_code))
} else {
  bl_projection <- read.csv2(file.path(wd_data_work, "file_balancing_bl.csv"), sep = ";") %>%
    mutate(bundesland_code = as.character(bundesland_code))
}

jump_off_year <- 2025
#  hamilton-perry --------------------------------------------------------------
if (linux) {
  load("/data/simon/2026-2035_HP_expanding_forecast_smoothed.RData")
} else {
  load(file.path(wd_res, "2026-2035_HP_expanding_forecast_smoothed.RData"))
}

hp_pred_for_balancing <- prepare_prediction_for_balancing(
  hp_pred_forecast_smoothed_export,
  municipality_reg_mapping,
  municipality_size_group_mapping_2025,
  allowed_deviation_pred,
  bl_projection,
  prediction_years = 2035
)

balanced_hp_pred <- hp_pred_for_balancing %>%
  #filter(!reg_code %in% regs_to_not_balance) %>%
  filter(bl_code == 1) %>%
  group_by(year, bl_code) %>%
  group_modify( ~ balance_prediction(.x, pred_col_name = "PRED_hamilton_perry"),
                .keep = TRUE) %>%
  select(
    municipality_code,
    bl_code,
    sex,
    age_group,
    year,
    PRED_hamilton_perry,
    balanced_pred
  )


#balanced_hp_pred <- balanced_hp_pred %>%
#  bind_rows(hp_pred_for_balancing %>%
#              filter(reg_code %in% regs_to_not_balance) %>%
#              select(municipality_code, reg_code, sex, age_group, year, PRED_hamilton_perry, projected_population) %>%
#              rename(balanced_pred = projected_population))


#save(balanced_hp_pred, file = file.path(wd_res, "2026-2035_exp_HP_balanced.RData"))
save(balanced_hp_pred, file = "2026-2035_exp_HP_balanced.RData")
print("HP finished")
