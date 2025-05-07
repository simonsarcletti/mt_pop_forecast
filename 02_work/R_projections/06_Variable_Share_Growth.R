############################################################################## #
# Filename
#    01_Variable_Share_Growth.R
#
# Description
#   Projection with Variable Share of Growth method such as in Wilson 2014
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      2025-05-07
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
# function for vsg on top of LIN/EXP: group_by(reg_code, sex, age_group)!!!
return_vsg_prediction <- function(data, jump_off_year){
  
  data <- data %>%
    group_by(municipality_code) %>%
    mutate(small_area_jump_off_pop = population[year == jump_off_year],
           district_jump_off_pop = district_pop[year == jump_off_year]) %>%
    ungroup() %>%
    mutate(sa_pop_change = PRED_tuned_LINEXP - small_area_jump_off_pop,
           district_pop_change = district_pop -district_jump_off_pop) %>%
    group_by(year, sex, age_group) %>%
    mutate(sum_abs_pop_change = sum(abs(sa_pop_change)),
           sum_pop_change = sum(sa_pop_change)) %>%
    ungroup() %>%
    mutate(pos_factor = (sum_abs_pop_change + district_pop_change - sum_pop_change) /  sum_abs_pop_change,
           neg_factor = (sum_abs_pop_change - district_pop_change - sum_pop_change) /  sum_abs_pop_change) %>%
    group_by(municipality_code) %>%
    mutate(PRED_vsg = case_when(small_area_jump_off_pop >= population[year == (jump_off_year-best_bpl)] ~ small_area_jump_off_pop + sa_pop_change * pos_factor,
                                small_area_jump_off_pop < population[year == (jump_off_year-best_bpl)] ~ small_area_jump_off_pop + sa_pop_change * neg_factor)) %>%
    select(municipality_code, year, population, PRED_vsg) %>%
    mutate(PRED_vsg = case_when(PRED_vsg < 0 ~ 0,
                                .default = PRED_vsg ))
  
  return(data)  
}

# Hyperparameter tuning: Find optmal length of base period for each index for TEST case
base_period_lengths <- 1:16
valid_prediction_periods <- 3
valid_years <- 2019:2021


train_data <- all_munip_pop %>% filter(year %in% c(2002:2021)) %>%
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
  filter(year %in% valid_years) %>%
  select(-c(population_change, population_change)) %>%
  summarise(across(ends_with("bpl"), ~mean_absolute_error(population, .x))) %>%
  rowwise() %>%
  mutate(
    best_bpl = which.min(c_across(ends_with("bpl")))
  ) %>%
  ungroup()

# test
test_prediction_periods <- 3


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

tuned_LINEXP_test <- test_data %>%
  left_join(select(LINEXP_MAE, index, best_bpl), by = join_by(index)) %>%
  group_by(index) %>%
  arrange(year) %>%
  mutate(PRED_tuned_LINEXP = return_LINEXP_prediction(population = smoothed_population,
                                                      population_change = smoothed_population_change,
                                                      population_change_factor = smoothed_population_change_factor,
                                                      base_period_length = best_bpl,
                                                      n_prediction_periods = test_prediction_periods))

tuned_LINEXP_test <- tuned_LINEXP_test %>% 
  select(index, year, population, PRED_tuned_LINEXP, best_bpl) %>%
  separate(index,
           into = c("municipality_code", "sex", "age_group"),
           sep = "_")


# implememtn VSG correction
load(file.path(wd_data_work, "district_projection.RData"))
load(file.path(wd_data_work, "municipality_code_reg_code_mapping.RData"))



vsg_data <- tuned_LINEXP_test %>%
  left_join(municipality_reg_mapping) %>%
  ungroup() %>%
  mutate(sex = as.numeric(sex)) %>%
  left_join(district_projection, 
            by = join_by(reg_code == district_identifier, sex == sex, age_group == coarse_age_group, year == year)) %>%
  group_by(reg_code, sex, age_group, year) %>%
  mutate(district_pop = case_when(is.na(projected_population) ~ sum(population),
                                  .default = projected_population)) %>%
  select(-projected_population) %>%
  arrange(municipality_code, year)



vsg_test <- vsg_data %>%
  group_by(reg_code, sex, age_group) %>%
  group_modify( ~ return_vsg_prediction(.x,
                                        jump_off_year = 2021),
                .keep = TRUE) %>%
  ungroup()

# export 
vsg_test_for_export <- vsg_test %>%
  select(-reg_code) %>%
  select(municipality_code,
         sex, 
         age_group,
         year,
         population,
         PRED_vsg)

save(vsg_test_for_export,
     file = file.path(wd_res, "final_vsg_test_pred_2022-2024.RData"))


plot_prediction(
  train_data = vsg_test_for_export %>% dplyr::filter(year %in% 2002:2021),
  test_data = vsg_test_for_export %>% dplyr::filter(year %in% 2022:2024),
  prediction_data = vsg_test_for_export %>% dplyr::filter(year %in% 2022:2024),
  train_col_name = "population",
  test_col_name = "population",
  prediction_col_name = "PRED_vsg",
  municipality_code = "10423",
  sex = "2",
  age_group = "75+",
  prediction_method = "LIN/EXP"
)

plot_prediction(
  train_data = vsg_test_for_export %>% dplyr::filter(year %in% 2002:2021),
  test_data = vsg_test_for_export %>% dplyr::filter(year %in% 2022:2024),
  prediction_data = vsg_test_for_export %>% dplyr::filter(year %in% 2022:2024),
  train_col_name = "population",
  test_col_name = "population",
  prediction_col_name = "PRED_vsg",
  municipality_code = "70728",
  sex = "1",
  age_group = "20 - 29",
  prediction_method = "LIN/EXP"
)


# prediction 2025-2035 ---------------------------------------------------------
# Hyperparameter tuning: Find optmal length of base period for each index for TEST case
base_period_lengths <- 1:18
valid_prediction_periods <- 5
valid_years <- 2020:2024


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
  filter(year %in% valid_years) %>%
  select(-c(population_change, population_change)) %>%
  summarise(across(ends_with("bpl"), ~mean_absolute_error(population, .x))) %>%
  rowwise() %>%
  mutate(
    best_bpl = which.min(c_across(ends_with("bpl")))
  ) %>%
  ungroup()


# prediction with found hyperparameters 
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

# VSG correction
load(file.path(wd_data_work, "district_projection.RData"))
load(file.path(wd_data_work, "municipality_code_reg_code_mapping.RData"))



vsg_data <- tuned_LINEXP_prediction %>%
  select(index, year, population, PRED_tuned_LINEXP, best_bpl) %>%
  separate(index,
           into = c("municipality_code", "sex", "age_group"),
           sep = "_") %>%
  left_join(municipality_reg_mapping) %>%
  ungroup() %>%
  mutate(sex = as.numeric(sex)) %>%
  left_join(district_projection, 
            by = join_by(reg_code == district_identifier, sex == sex, age_group == coarse_age_group, year == year)) %>%
  group_by(reg_code, sex, age_group, year) %>%
  mutate(district_pop = case_when(is.na(projected_population) ~ sum(population),
                                  .default = projected_population)) %>%
  select(-projected_population) %>%
  arrange(municipality_code, year)



vsg_prediction <- vsg_data %>%
  group_by(reg_code, sex, age_group) %>%
  group_modify( ~ return_vsg_prediction(.x,
                                        jump_off_year = 2024),
                .keep = TRUE) %>%
  ungroup()

# export 
vsg_pred_for_export <- vsg_prediction %>%
  select(-reg_code) %>%
  select(municipality_code,
         sex, 
         age_group,
         year,
         population,
         PRED_vsg)

save(vsg_pred_for_export,
     file = file.path(wd_res, "25-35_VSG_prediction.RData"))


plot_prediction(
  train_data = vsg_pred_for_export %>% dplyr::filter(year %in% 2002:2021),
  test_data = vsg_pred_for_export %>% dplyr::filter(year %in% 2022:2024),
  prediction_data = vsg_pred_for_export %>% dplyr::filter(year %in% 2025:2035),
  train_col_name = "population",
  test_col_name = "population",
  prediction_col_name = "PRED_vsg",
  municipality_code = "10423",
  sex = "2",
  age_group = "75+",
  prediction_method = "LIN/EXP"
)

plot_prediction(
  train_data = vsg_pred_for_export %>% dplyr::filter(year %in% 2002:2021),
  test_data = vsg_pred_for_export %>% dplyr::filter(year %in% 2022:2024),
  prediction_data = vsg_pred_for_export %>% dplyr::filter(year %in% 2025:2035),
  train_col_name = "population",
  test_col_name = "population",
  prediction_col_name = "PRED_vsg",
  municipality_code = "70728",
  sex = "1",
  age_group = "20 - 29",
  prediction_method = "LIN/EXP"
)


plot_prediction(
  train_data = vsg_pred_for_export %>% dplyr::filter(year %in% 2002:2021),
  test_data = vsg_pred_for_export %>% dplyr::filter(year %in% 2022:2024),
  prediction_data = vsg_pred_for_export %>% dplyr::filter(year %in% 2025:2035),
  train_col_name = "population",
  test_col_name = "population",
  prediction_col_name = "PRED_vsg",
  municipality_code = "41112",
  sex = "1",
  age_group = "45 - 54",
  prediction_method = "LIN/EXP"
)

