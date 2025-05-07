############################################################################## #
# Filename
#    01_Constant_Share_Growth.R
#
# Description
#   Projection with Constant Share of Growth method such as in Wilson 2014
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      2025-05-07
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #

load(file.path(wd_data_work, "all_municipalities_population.RData"))
load(file.path(wd_data_work, "district_projection.RData"))

# csg function -----------------------------------------------------------------
# must be applied to data frame arragned by year !
return_csg_projection <- function(population,
                                  district_projection,
                                  n_prediction_periods,
                                  base_period_length,
                                  jump_off_year) {
  jump_off_year_idx <- jump_off_year - 2002 + 1
  print(population[jump_off_year_idx])
  
  base_period_begin <- jump_off_year_idx - base_period_length[1] + 1
  base_period_end <- jump_off_year_idx
  
  district_growth <- district_projection[base_period_end] - district_projection[base_period_begin]
  cohort_growth <- population[base_period_end] - population[base_period_begin]
  
  if (district_growth == 0) {
    district_growth <- 0.1
  }
  
  share_growth <- cohort_growth / district_growth
  
  if (share_growth > 1) {
    share_growth <- 1
  }
  if (share_growth < -1) {
    share_growth <- -1
  }
  
  
  forecast_district_growth <-  district_projection[(base_period_end+1):(base_period_end+n_prediction_periods)] - district_projection[base_period_end]
  
  
  prediction <- population[base_period_end] + forecast_district_growth * share_growth
  
  prediction[prediction < 0] <- 0
  
  c(population[1:base_period_end], prediction)
}



# test ------------------------------------------------------------------------
# 1 hyperparameter tuning
n_prediction_periods <- 3
jump_off_year <- 2018
base_period_lengths <- 2:15
years_for_validation <- 2002:2021
validation_years <- 2019:2021

csg_data <- all_munip_pop %>%
  select(-population) %>%
  group_by(reg_code, year, sex, coarse_age_group) %>%
  mutate(district_pop = sum(smoothed_population)) %>%
  rename(population = smoothed_population) %>%
  ungroup()

csg_validation <- csg_data %>%
  group_by(municipality_code, sex, coarse_age_group) %>%
  arrange(year) %>%
  filter(year %in% years_for_validation)


for (bpl in base_period_lengths) {
  new_col_name <- paste0("csg_prediction_", bpl, "bpl")
  
  csg_validation <- csg_validation %>%
    mutate(
      !!new_col_name := return_csg_projection(
        population = population,
        district_projection = district_pop,
        n_prediction_periods = n_prediction_periods,
        base_period_length = bpl,
        jump_off_year = jump_off_year
      )
    )
}

csg_MAE <- csg_validation %>%
  group_by(municipality_code, sex, coarse_age_group) %>%
  filter(year %in% validation_years) %>%
  select(-c(municipality)) %>%
  summarise(across(ends_with("bpl"), ~ mean_absolute_error(population, .x))) %>%
  rowwise() %>%
  mutate(best_bpl = which.min(c_across(ends_with("bpl")))) %>%
  ungroup()


# test predition ---------------------------------------------------------------
test_jump_off <- 2021
test_n_pred_periods <- 3

csg_test <- csg_data %>%
  ungroup() %>%
  left_join(
    select(csg_MAE, municipality_code, sex, coarse_age_group, best_bpl),
    by = join_by(municipality_code, sex, coarse_age_group)
  ) %>%
  group_by(municipality_code, sex, coarse_age_group) %>%
  arrange(year) %>%
  mutate(
    PRED_csg_final = return_csg_projection(
      population = population,
      district_projection = district_pop,
      n_prediction_periods = test_n_pred_periods,
      base_period_length = best_bpl,
      # using different bpl for each cohort requires balancing (if wished)
      jump_off_year = test_jump_off
    )
  )

# Export -----------------------------------------------------------------------
csg_test_export <- csg_test %>%
  select(-c(district_pop, best_bpl, population)) %>%
  left_join(all_munip_pop, 
            by = join_by(municipality_code, reg_code, municipality, sex, coarse_age_group, year)) %>% 
  select(municipality_code,
         sex,
         coarse_age_group,
         year,
         population,
         PRED_csg_final) %>%
  rename(age_group = coarse_age_group)

save(csg_text_export,
     file = file.path(wd_res, "final_csg_test_pred_2022-2024.RData"))


length(csg_test_export$PRED_csg_final[csg_test_export$PRED_csg_final == 0 &
                                        csg_test_export$year > 2021])

plot_prediction(
  train_data = csg_test_export %>% dplyr::filter(year %in% 2002:2021),
  test_data = csg_test_export %>% dplyr::filter(year %in% 2022:2024),
  prediction_data = csg_test_export %>% dplyr::filter(year %in% 2022:2024),
  train_col_name = "population",
  test_col_name = "population",
  prediction_col_name = "PRED_csg_final",
  municipality_code = "10423",
  sex = "2",
  age_group = "75+",
  prediction_method = "LIN/EXP"
)

plot_prediction(
  train_data = csg_test_export %>% dplyr::filter(year %in% 2002:2021),
  test_data = csg_test_export %>% dplyr::filter(year %in% 2022:2024),
  prediction_data = csg_test_export %>% dplyr::filter(year %in% 2022:2024),
  train_col_name = "population",
  test_col_name = "population",
  prediction_col_name = "PRED_csg_final",
  municipality_code = "32302",
  sex = "2",
  age_group = "45 - 54",
  prediction_method = "LIN/EXP"
)



# prediction -------------------------------------------------------------------
n_prediction_periods_valid <- 5
n_prediction_periods_pred <- 11
valid_jump_off_year <- 2019
pred_jump_off_year <- 2024
validation_period <- 2020:2024
forecast_years <- 2025:2035
base_period_lengths <- 1:16

csg_data <- all_munip_pop %>%
  select(-population) %>%
  group_by(reg_code, year, sex, coarse_age_group) %>%
  mutate(district_pop = sum(smoothed_population)) %>%
  rename(population = smoothed_population) %>%
  ungroup()

csg_pred_validation <- csg_data %>%
  group_by(municipality_code, sex, coarse_age_group) %>%
  arrange(year) 


for (bpl in base_period_lengths) {
  new_col_name <- paste0("csg_prediction_", bpl, "bpl")
  
  csg_pred_validation <- csg_pred_validation %>%
    mutate(
      !!new_col_name := return_csg_projection(
        population = population,
        district_projection = district_pop,
        n_prediction_periods = n_prediction_periods_valid,
        base_period_length = bpl,
        jump_off_year = valid_jump_off_year
      )
    )
}

csg_MAE <- csg_pred_validation %>%
  group_by(municipality_code, sex, coarse_age_group) %>%
  filter(year %in% validation_period) %>%
  select(-c(municipality)) %>%
  summarise(across(ends_with("bpl"), ~ mean_absolute_error(population, .x))) %>%
  rowwise() %>%
  mutate(best_bpl = which.min(c_across(ends_with("bpl")))) %>%
  ungroup()



prediction_data <- csg_data %>%
  select(-municipality) %>%
  group_by(municipality_code, reg_code, sex, coarse_age_group) %>%
  complete(
    year = union(year, forecast_years),
    # and for any brand‐new rows fill these cols with NA
    fill = list(
      population = NA_real_,
      district_pop = NA_real_
    )
  ) 


prediction_data <- prediction_data %>%
  left_join(filter(district_projection, year %in% forecast_years), 
            by = join_by(reg_code == district_identifier, sex == sex, coarse_age_group == coarse_age_group, year == year)) %>%
  mutate(district_pop = case_when(is.na(district_pop) ~ projected_population,
                                  .default = district_pop)) %>%
  select(-projected_population) %>%
  ungroup() %>%
  left_join(
    select(csg_MAE, municipality_code, sex, coarse_age_group, best_bpl),
    by = join_by(municipality_code, sex, coarse_age_group)
  ) %>%
  group_by(municipality_code, sex, coarse_age_group) %>%
  arrange(year) %>%
  mutate(
    PRED_csg_final = return_csg_projection(
      population = population,
      district_projection = district_pop,
      n_prediction_periods = n_prediction_periods_pred,
      base_period_length = best_bpl,
      # using different bpl for each cohort requires balancing (if wished)
      jump_off_year = pred_jump_off_year
    )
  )


# export -----------------------------------------------------------------------
csg_pred_export <- prediction_data %>%
  select(municipality_code,
         sex,
         coarse_age_group,
         year,
         population,
         PRED_csg_final) %>%
  rename(age_group = coarse_age_group)

save(csg_pred_export,
     file = file.path(wd_res, "25-35_CSG_prediction.RData"))

plot_prediction(
  train_data = csg_pred_export %>% dplyr::filter(year %in% 2002:2024),
  test_data = csg_pred_export %>% dplyr::filter(year %in% 2022:2024),
  prediction_data = csg_pred_export %>% dplyr::filter(year %in% 2025:2035),
  train_col_name = "population",
  test_col_name = "population",
  prediction_col_name = "PRED_csg_final",
  municipality_code = "70333",
  sex = "2",
  age_group = "20 - 29",
  prediction_method = "LIN/EXP"
)


plot_prediction(
  train_data = csg_pred_export %>% dplyr::filter(year %in% 2002:2021),
  test_data = csg_pred_export %>% dplyr::filter(year %in% 2022:2024),
  prediction_data = csg_pred_export %>% dplyr::filter(year %in% 2025:2035),
  train_col_name = "population",
  test_col_name = "population",
  prediction_col_name = "PRED_csg_final",
  municipality_code = "20702",
  sex = "2",
  age_group = "45 - 54",
  prediction_method = "LIN/EXP"
)

prediction_data2 %>% filter(reg_code == "6030", year == 2026) %>% pull(PRED_csp_final) %>% sum()
length(csg_pred_export$PRED_csg_final[csg_pred_export$PRED_csg_final == 0 &
                                        csg_pred_export$year > 2024])
