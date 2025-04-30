############################################################################## #
# Filename
#    01_Constant_Share_Population.R
#
# Description
#   Projection with Constant Share of Population method such as in Wilson 2014
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      2025-02-25
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #

load(file.path(wd_data_work, "all_municipalities_population.RData"))
load(file.path(wd_data_work, "district_projection.RData"))





# csp function -----------------------------------------------------------------
# must be applied to data frame arragned by year !
return_csp_projection <- function(population,
                                  pop_share,
                                  district_projection,
                                  n_prediction_periods,
                                  base_period_length,
                                  jump_off_year) {
  jump_off_year_inx <- jump_off_year - 2002 + 1
  print(population[jump_off_year_inx])
  
  base_period_begin <- jump_off_year_inx - base_period_length + 1
  base_period_end <- jump_off_year_inx
  
  average_pop_share <- mean(pop_share[base_period_begin:base_period_end], na.rm = TRUE)
  
  prediction <- district_projection[(base_period_end + 1):(base_period_end + n_prediction_periods)] * average_pop_share
  
  # return
  c(population[1:base_period_end], prediction)
  
}

# 1. tuning period for averaged population share for each district
n_prediction_periods <- 3
jump_off_year <- 2018
base_period_lengths <- 1:15

csp_data <- all_munip_pop %>%
  select(-smoothed_population) %>%
  group_by(reg_code, year, sex, coarse_age_group) %>%
  mutate(district_pop = sum(population)) %>%
  ungroup() %>%
  mutate(pop_share = population / district_pop)

csp_validation <- csp_data %>%
  group_by(municipality_code, sex, coarse_age_group) %>%
  arrange(year) %>%
  filter(year %in% 2002:2021)



for (bpl in base_period_lengths) {
  new_col_name <- paste0("csp_prediction_", bpl, "bpl")
  
  csp_validation <- csp_validation %>%
    mutate(
      !!new_col_name := return_csp_projection(
        population = population,
        pop_share = pop_share,
        district_projection = district_pop,
        n_prediction_periods = n_prediction_periods,
        base_period_length = bpl,
        jump_off_year = jump_off_year
      )
    )
}

csp_MAE <- csp_validation %>%
  group_by(municipality_code, sex, coarse_age_group) %>%
  filter(year %in% 2019:2021) %>%
  select(-c(municipality)) %>%
  summarise(across(ends_with("bpl"), ~ mean_absolute_error(population, .x))) %>%
  rowwise() %>%
  mutate(best_bpl = which.min(c_across(ends_with("bpl")))) %>%
  ungroup()

# test predition ---------------------------------------------------------------
test_jump_off <- 2021
tetst_n_pred_periods <- 3

csp_test <- csp_data %>%
  left_join(
    select(csp_MAE, municipality_code, sex, coarse_age_group, best_bpl),
    by = join_by(municipality_code, sex, coarse_age_group)
  ) %>%
  group_by(municipality_code, sex, coarse_age_group) %>%
  arrange(year) %>%
  mutate(
    PRED_csp_final = return_csp_projection(
      population = population,
      pop_share = pop_share,
      district_projection = district_pop,
      n_prediction_periods = tetst_n_pred_periods,
      base_period_length = best_bpl, # using different bpl for each cohort requires balancing (if wished)
      jump_off_year = test_jump_off
    )
  )

csp_test %>% filter(reg_code == "6030", year == 2024) %>% pull(PRED_csp_final) %>% sum()

# Export -----------------------------------------------------------------------
csp_text_export <- csp_test %>% 
  select(municipality_code, sex, coarse_age_group, year, population, PRED_csp_final) %>%
  rename(age_group = coarse_age_group)

save(csp_text_export,
     file= file.path(wd_res, "final_csp_test_pred_2022-2024.RData"))


plot_prediction(train_data = csp_text_export %>% dplyr::filter(year %in% 2002:2021),
                test_data = csp_text_export %>% dplyr::filter(year %in% 2022:2024),
                prediction_data = csp_text_export %>% dplyr::filter(year %in% 2022:2024),
                train_col_name = "population",
                test_col_name = "population",
                prediction_col_name = "PRED_csp_final",
                municipality_code = "10423",
                sex = "2",
                age_group = "75+",
                prediction_method = "LIN/EXP")

plot_prediction(train_data = csp_text_export %>% dplyr::filter(year %in% 2002:2021),
                test_data = csp_text_export %>% dplyr::filter(year %in% 2022:2024),
                prediction_data = csp_text_export %>% dplyr::filter(year %in% 2022:2024),
                train_col_name = "population",
                test_col_name = "population",
                prediction_col_name = "PRED_csp_final",
                municipality_code = "70728",
                sex = "2",
                age_group = "20 - 29",
                prediction_method = "LIN/EXP")

