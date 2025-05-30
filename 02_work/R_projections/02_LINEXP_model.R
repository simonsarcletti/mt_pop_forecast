############################################################################## #
# Filename
#    01_LIN/EXP_model.R
#
# Description
#   Projection with LIN/EXP method such as in Wilson 2014
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      2025-02-25
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #

# load data and train test split------------------------------------------------

load(file.path(wd_data_work, "all_municipalities_population.RData"))






# LIN/EXP prediction -----------------------------------------------------------
return_LINEXP_prediction <- function(population,
                                     population_change,
                                     population_change_factor,
                                     base_period_length,
                                     n_prediction_periods) {
  # Print the last population value for debugging
  cat("Last population value:", population[length(population) - n_prediction_periods], "\n")
  
  base_period_begin <- length(population) - n_prediction_periods - base_period_length[1] + 1
  base_period_end <- length(population) - n_prediction_periods  
  
  annual_average_pop_growth <- mean(population_change[base_period_begin:base_period_end], na.rm = TRUE)
  annual_average_pop_growth_rate <- gm_mean(population_change_factor[base_period_begin:base_period_end], na.rm = TRUE) - 1
  
  h <- seq_len(n_prediction_periods)
  
  if (annual_average_pop_growth > 0) {
    new_vals <- population[base_period_end] + annual_average_pop_growth * h
  } else {
    new_vals <- population[base_period_end] * exp(annual_average_pop_growth_rate * h)
  }
  
  # return past + future
  c(population[1:base_period_end], new_vals)
}
#


# Hyperparameter tuning: Find optmal length of base period for each index
base_period_lengths <- 1:18
valid_prediction_periods <- 3
test_years <- 2022:2024


train_data <- all_munip_pop %>% filter(year %in% c(2002:2024)) %>%
  rename(age_group = coarse_age_group)


train_data_growth_rate <- train_data %>%
  unite("index", municipality_code, sex, age_group, sep = "_") %>%
  group_by(index) %>%
  arrange(year, .by_group = TRUE) %>%
  select(index, year, population, smoothed_population) %>%
  mutate(population_change = calculate_population_change(population)) %>%
  mutate(population_change_factor = calculate_population_change_factor(population)) %>%
  mutate(smoothed_population_change = calculate_population_change(smoothed_population)) %>%
  mutate(smoothed_population_change_factor = calculate_population_change_factor(smoothed_population))

LINEXP_validation <- train_data_growth_rate %>%
  group_by(index) %>%
  arrange(year)


for(bpl in base_period_lengths) {
  new_col_name <- paste0("LINEXP_prediction_", bpl, "bpl")
  
  LINEXP_validation <- LINEXP_validation %>%
    mutate(
      !!new_col_name := return_LINEXP_prediction(
        population = population,
        population_change = smoothed_population_change,
        population_change_factor = smoothed_population_change_factor,
        base_period_length = bpl,
        n_prediction_periods = valid_prediction_periods
      )
    )
}


LINEXP_MAE <- LINEXP_validation %>%
  group_by(index) %>%
  filter(year %in% test_years) %>%
  select(-c(population_change, population_change)) %>%
  summarise(across(ends_with("bpl"), ~mean_absolute_error(population, .x))) %>%
  rowwise() %>%
  mutate(
    best_bpl = which.min(c_across(ends_with("bpl")))
  ) %>%
  ungroup()



# test prediction --------------------------------------------------------------
prediction_periods <- 3 

test_data <- all_munip_pop %>%
  rename(age_group = coarse_age_group) %>%
  unite("index", municipality_code, sex, age_group, sep = "_") %>%
  group_by(index) %>%
  arrange(year, .by_group = TRUE) %>%
  select(index, year, reg_code, population, smoothed_population) %>%
  mutate(population_change = calculate_population_change(population)) %>%
  mutate(population_change_factor = calculate_population_change_factor(population)) %>%
  mutate(smoothed_population_change = calculate_population_change(smoothed_population)) %>%
  mutate(smoothed_population_change_factor = calculate_population_change_factor(smoothed_population))


# find best base period length
LINEXP_validation <- test_data %>%
  group_by(index) %>%
  arrange(year)


for(bpl in base_period_lengths) {
  new_col_name <- paste0("LINEXP_prediction_", bpl, "bpl")
  
  LINEXP_validation <- LINEXP_validation %>%
    mutate(
      !!new_col_name := return_LINEXP_prediction(
        population = population,
        population_change = smoothed_population_change,
        population_change_factor = smoothed_population_change_factor,
        base_period_length = bpl,
        n_prediction_periods = valid_prediction_periods
      )
    )
}


LINEXP_MAE <- LINEXP_validation %>%
  group_by(index) %>%
  filter(year %in% test_years) %>%
  select(-c(population_change, population_change)) %>%
  summarise(across(ends_with("bpl"), ~mean_absolute_error(population, .x))) %>%
  rowwise() %>%
  mutate(
    best_bpl = which.min(c_across(ends_with("bpl")))
  ) %>%
  ungroup()




# prediction
tuned_LINEXP_test <- test_data %>%
  left_join(select(LINEXP_MAE, index, best_bpl), by = join_by(index)) %>%
  group_by(index) %>%
  arrange(year) %>%
  mutate(PRED_tuned_LINEXP = return_LINEXP_prediction(population = smoothed_population,
                                                      population_change = smoothed_population_change,
                                                      population_change_factor = smoothed_population_change_factor,
                                                      base_period_length = best_bpl,
                                                      n_prediction_periods = prediction_periods))


tuned_LINEXP_test_export <- tuned_LINEXP_test %>% 
  select(index, year, population, PRED_tuned_LINEXP) %>%
  separate(index,
           into = c("municipality_code", "sex", "age_group"),
           sep = "_")
save(tuned_LINEXP_test_export,
     file= file.path(wd_res, "final_LINEXP_prediction.RData"))





# tuned (actual) prediction -------------------------------------------------------------
prediction_periods <- 11
forecast_years <- (2024 + 1):(2024+prediction_periods)



prediction_data <- all_munip_pop %>%
  rename(age_group = coarse_age_group) %>%
  unite("index", municipality_code, sex, age_group, sep = "_") %>%
  group_by(index) %>%
  arrange(year, .by_group = TRUE) %>%
  select(index, year, reg_code, population, smoothed_population) %>%
  mutate(population_change = calculate_population_change(population)) %>%
  mutate(population_change_factor = calculate_population_change_factor(population)) %>%
  mutate(smoothed_population_change = calculate_population_change(smoothed_population)) %>%
  mutate(smoothed_population_change_factor = calculate_population_change_factor(smoothed_population))

prediction_data <- prediction_data %>%
  group_by(index) %>%
  complete(year = union(year, forecast_years),
           # and for any brand‐new rows fill these cols with NA
           fill = list(
             population                         = NA_real_,
             smoothed_population                = NA_real_,
             population_change                  = NA_real_,
             population_change_factor           = NA_real_,
             smoothed_population_change         = NA_real_,
             smoothed_population_change_factor  = NA_real_
           )
  ) %>%
  arrange(index, year) %>%
  ungroup()


tuned_LINEXP_prediction <- prediction_data %>%
  left_join(select(LINEXP_MAE, index, best_bpl), by = join_by(index)) %>%
  group_by(index) %>%
  arrange(year) %>%
  mutate(PRED_tuned_LINEXP = return_LINEXP_prediction(population = smoothed_population,
                                                 population_change = smoothed_population_change,
                                                 population_change_factor = smoothed_population_change_factor,
                                                 base_period_length = best_bpl,
                                                 n_prediction_periods = prediction_periods))


# Export -----------------------------------------------------------------------
tuned_LINEXP_pred_export <- tuned_LINEXP_prediction %>% 
  select(index, year, population, PRED_tuned_LINEXP) %>%
  separate(index,
           into = c("municipality_code", "sex", "age_group"),
           sep = "_")

save(tuned_LINEXP_pred_export,
     file= file.path(wd_res, "25-35_LINEXP_prediction.RData"))



plot_prediction(train_data = tuned_LINEXP_pred_export %>% dplyr::filter(year %in% 2002:2021),
                test_data = tuned_LINEXP_pred_export %>% dplyr::filter(year %in% 2022:2024),
                prediction_data = tuned_LINEXP_pred_export %>% dplyr::filter(year %in% 2022:2024),
                train_col_name = "population",
                test_col_name = "population",
                prediction_col_name = "PRED_tuned_LINEXP",
                municipality_code = "10423",
                sex = 2,
                age_group = "75+",
                prediction_method = "LIN/EXP")

plot_prediction(train_data = tuned_LINEXP_prediction %>% dplyr::filter(year %in% 2002:2021),
                test_data = tuned_LINEXP_prediction %>% dplyr::filter(year %in% 2022:2024),
                prediction_data = tuned_LINEXP_prediction %>% dplyr::filter(year %in% 2022:2024),
                train_col_name = "population",
                test_col_name = "population",
                prediction_col_name = "PRED_tuned_LINEXP",
                cohort = "60624_2_20 - 29",
                prediction_method = "LIN/EXP")
