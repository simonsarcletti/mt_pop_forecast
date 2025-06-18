############################################################################## #
# Filename
#    04_hamilton_perry.R
#
# Description
#   Projection with Hamilton-Perry but with rolling 5 years window
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      2025
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #


##############################################################################~#
# Data reading #################################################################
load(file.path(wd_data_work, "all_municipalities_population_2025.RData"))
all_munip_pop <- all_munip_pop_2025

# HP rollig window function ----------------------------------------------------
return_hp_projection <- function(data,
                                 n_prediction_periods = 5,
                                 jump_off_year = 2024) {
  # Define cohorts and year range
  # Define cohorts and year range
  sex_age_cohorts <- c(
    "1_0 - 9",
    "1_10 - 19",
    "1_20 - 29",
    "1_30 - 44",
    "1_45 - 54",
    "1_55 - 64",
    "1_65 - 74",
    "1_75+",
    "2_0 - 9",
    "2_10 - 19",
    "2_20 - 29",
    "2_30 - 44",
    "2_45 - 54",
    "2_55 - 64",
    "2_65 - 74",
    "2_75+"
  )
  all_years <- 2002:(jump_off_year + n_prediction_periods)
  n_cohorts <- length(sex_age_cohorts)
  
  # Precompute indices for vectorized access
  cohort_idx <- setNames(seq_len(n_cohorts), sex_age_cohorts)
  mother_cohorts <- c("2_20 - 29", "2_30 - 44")
  mothers_idx <- cohort_idx[mother_cohorts]
  
  
  # Initialize projection matrices
  hp_proj <- matrix(
    NA,
    nrow = n_cohorts,
    ncol = length(all_years),
    dimnames = list(sex_age_cohorts, as.character(all_years))
  )
  
  ccr_matrix <- matrix(
    NA,
    nrow = n_cohorts,
    ncol = n_prediction_periods,
    dimnames = list(sex_age_cohorts, paste0("t+", seq_len(
      n_prediction_periods
    )))
  )
  
  # Fill historical data (2002 through jump_off_year)
  hist_years <- 2002:jump_off_year
  for (cohort in sex_age_cohorts) {
    hp_proj[cohort, as.character(hist_years)] <-
      data$population[data$cohort == cohort &
                        data$year %in% hist_years]
  }
  
  # Store base populations at jump-off
  base_pops <- hp_proj[, as.character(jump_off_year)]
  
  # Projection loop for each future period
  for (j in seq_len(n_prediction_periods)) {
    target_year <- jump_off_year + j
    past_offset_year <- jump_off_year - 10 + j
    prev_year <- target_year - 5
    
    # 1) Cohort Change Ratio / Difference for cohorts >0–9
    for (i in seq_len(n_cohorts - 1)) {
      current <- sex_age_cohorts[i]
      next_cohort <- sex_age_cohorts[i + 1]
      
      # Same sex & not the open-ended "75+" group
      if (substr(current, 1, 1) == substr(next_cohort, 1, 1) &&
          !grepl("75\\+$", current)) {
        next_val <- hp_proj[next_cohort, as.character(prev_year)]
        print(next_val)
        past_val <- hp_proj[current, as.character(past_offset_year)]
        print(past_val)
        if (!is.na(next_val) &&
            !is.na(past_val) && next_val > past_val) {
          diff_val <- next_val - past_val
          
          hp_proj[next_cohort, as.character(target_year)] <-
            hp_proj[current, as.character(prev_year)] + diff_val
        } else {
          # Ratio branch (CCR)
          if (!is.na(next_val) &&
              !is.na(past_val) && past_val > 0) {
            ccr <- next_val / past_val
          } else {
            ccr <- 1  # fallback to flat
          }
          ccr_matrix[current, paste0("t+", j)] <- ccr
          hp_proj[next_cohort, as.character(target_year)] <-
            hp_proj[current, as.character(prev_year)] * ccr
        }
      }
    }
    
    # 2) Child-Woman Ratio (CWR) for 0–9 cohorts, once both mother cohorts are projected
    jump_off_moms <- sum(hp_proj[mothers_idx, as.character(jump_off_year)], na.rm = TRUE)
    boys_ratio  <- hp_proj["1_0 - 9", as.character(jump_off_year)] / jump_off_moms
    girls_ratio <- hp_proj["2_0 - 9", as.character(jump_off_year)] / jump_off_moms
    future_moms <- sum(hp_proj[mothers_idx, as.character(target_year)], na.rm = TRUE)
    
    hp_proj["1_0 - 9", as.character(target_year)] <- boys_ratio  * future_moms
    hp_proj["2_0 - 9", as.character(target_year)] <- girls_ratio * future_moms
  }
  
  # Convert to long format once, after all projections
  hp_projection_long <- tibble::as_tibble(hp_proj, rownames = "cohort") %>%
    tidyr::pivot_longer(cols = -cohort,
                        names_to = "year",
                        values_to = "projected_population") %>%
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::arrange(cohort, year)
  
  return(hp_projection_long)
}
# ------------------------------------------------------------------------------




hp_data <- all_munip_pop %>% 
  rename(age_group = coarse_age_group) %>%
  unite("cohort", c("sex", "age_group")) %>%
  select(municipality_code, cohort, year, population) %>%
  #rename(population = smoothed_population) %>%
  mutate(year = as.character(year))


hp_test <- hp_data %>%
  mutate(year = as.character(year)) %>%
  group_by(municipality_code) %>%
  group_modify(~ return_hp_projection(.x,
                                      n_prediction_periods = 10,
                                      jump_off_year = 2025)) %>%
  ungroup()




smoothed_rolling_hp_pred_export <- hp_test %>%
  mutate(year = as.character(year)) %>%
  left_join(hp_data, by = join_by(municipality_code, cohort, year)) %>%
  separate(cohort, into = c("sex", "age_group"), sep = "_") %>%
  rename(smoothed_population = population) %>%
  mutate(year = as.numeric(year), sex = as.numeric(sex)) %>%
  left_join(
    select(all_munip_pop, -smoothed_population),
    by = join_by(
      municipality_code == municipality_code,
      sex == sex,
      age_group == coarse_age_group,
      year == year
    )
  ) %>%
  select(-municipality) %>%
  #rename(TEST_hamilton_perry = projected_population) %>%
  mutate(year = as.numeric(year)) %>%
  select(municipality_code,
         sex,
         age_group,
         year,
         population,
         projected_population) %>%
  group_by(municipality_code, sex, age_group) %>%
  arrange(year) %>%
  mutate(smoothed_pred = rollmean(
    projected_population,
    k = 5,
    fill = NA,
    align = "right"
  )) %>%
  select(-projected_population)

save(rolling_hp_pred_export,
     file = file.path(wd_res, "2026-2035_rolling_HP_on_raw.RData"))



plot_prediction(
  train_data = hp_test_export %>% dplyr::filter(year %in% 2002:2021),
  test_data = hp_test_export %>% dplyr::filter(year %in% 2022:2024),
  prediction_data = hp_test_export %>% dplyr::filter(year %in% 2022:2024),
  train_col_name = "population",
  test_col_name = "population",
  prediction_col_name = "projected_population",
  municipality_code = "10101",
  sex = 2,
  age_group = "0 - 9",
  prediction_method = "HP"
)


# rolling hp prediction 2026-2035 ----------------------------------------------
hp_data <- all_munip_pop %>% 
  rename(age_group = coarse_age_group) %>%
  unite("cohort", c("sex", "age_group")) %>%
  select(municipality_code, cohort, year, population) %>%
  #rename(population = smoothed_population) %>%
  mutate(year = as.character(year))


rolling_hp_pred_tmp <- hp_data %>%
  filter(municipality_code == "20518") %>%
  mutate(year = as.character(year)) %>%
  group_by(municipality_code) %>%
  group_modify(~ return_hp_projection(.x,
                                      n_prediction_periods = 2,
                                      jump_off_year = 2025)) %>%
  ungroup()

rolling_hp_pred <- rolling_hp_pred %>%
  mutate(year = as.character(year)) %>%
  rename(population = projected_population) %>%
  group_by(municipality_code) %>%
  group_modify(~ return_hp_projection(.x,
                                      n_prediction_periods = 5,
                                      jump_off_year = 2030)) %>%
  ungroup()



rolling_hp_pred_export <- rolling_hp_pred %>%
  mutate(year = as.character(year)) %>%
  left_join(hp_data, by = join_by(municipality_code, cohort, year)) %>%
  separate(cohort, into = c("sex", "age_group"), sep = "_") %>%
  rename(smoothed_population = population) %>%
  mutate(year = as.numeric(year), sex = as.numeric(sex)) %>%
  left_join(
    select(all_munip_pop, -smoothed_population),
    by = join_by(
      municipality_code == municipality_code,
      sex == sex,
      age_group == coarse_age_group,
      year == year
    )
  ) %>%
  select(-municipality) %>%
  #rename(TEST_hamilton_perry = projected_population) %>%
  mutate(year = as.numeric(year)) %>%
  select(municipality_code,
         sex,
         age_group,
         year,
         population,
         projected_population)


plot_prediction(
  train_data = rolling_hp_pred_export %>% dplyr::filter(year %in% 2002:2025),
  test_data = rolling_hp_pred_export %>% dplyr::filter(year %in% 2025:2035),
  prediction_data = rolling_hp_pred_export %>% dplyr::filter(year %in% 2025:2035),
  train_col_name = "projected_population",
  test_col_name = "projected_population",
  prediction_col_name = "projected_population",
  municipality_code = "20518",
  sex = 2,
  age_group = "30 - 44",
  prediction_method = "HP"
)











load(file.path(wd_data_work, "all_municipalities_population_2025.RData"))
all_munip_pop <- all_munip_pop_2025


hp_data <- all_munip_pop %>% 
  filter(municipality_code == "10101") %>%
  rename(age_group = coarse_age_group) %>%
  unite("cohort", c("sex", "age_group")) %>%
  select(municipality_code, cohort, year, smoothed_population) %>%
  rename(population = smoothed_population) %>%
  mutate(year = as.character(year))

data <- hp_data
jump_off_year <- 2021
n_prediction_periods <- 10
  # Define cohorts and year range
  sex_age_cohorts <- c(
    "1_0 - 9",
    "1_10 - 19",
    "1_20 - 29",
    "1_30 - 44",
    "1_45 - 54",
    "1_55 - 64",
    "1_65 - 74",
    "1_75+",
    "2_0 - 9",
    "2_10 - 19",
    "2_20 - 29",
    "2_30 - 44",
    "2_45 - 54",
    "2_55 - 64",
    "2_65 - 74",
    "2_75+"
  )
  all_years <- 2002:(jump_off_year + n_prediction_periods)
  n_cohorts <- length(sex_age_cohorts)
  
  # Precompute indices for vectorized access
  cohort_idx <- setNames(seq_len(n_cohorts), sex_age_cohorts)
  mother_cohorts <- c("2_20 - 29", "2_30 - 44")
  mothers_idx <- cohort_idx[mother_cohorts]
  
  
  # Initialize projection matrices
  hp_proj <- matrix(
    NA,
    nrow = n_cohorts,
    ncol = length(all_years),
    dimnames = list(sex_age_cohorts, as.character(all_years))
  )
  
  ccr_matrix <- matrix(
    NA,
    nrow = n_cohorts,
    ncol = n_prediction_periods,
    dimnames = list(sex_age_cohorts, paste0("t+", seq_len(
      n_prediction_periods
    )))
  )
  
  # Fill historical data (2002 through jump_off_year)
  hist_years <- 2002:jump_off_year
  for (cohort in sex_age_cohorts) {
    hp_proj[cohort, as.character(hist_years)] <-
      data$population[data$cohort == cohort &
                        data$year %in% hist_years]
  }
  
  # Store base populations at jump-off
  base_pops <- hp_proj[, as.character(jump_off_year)]
  
  # Projection loop for each future period
  for (j in seq_len(n_prediction_periods)) {
    target_year <- jump_off_year + j
    print(target_year)
    past_offset_year <- jump_off_year - 10 + j
    print(past_offset_year)
    prev_year <- target_year - 5
    print(prev_year)
    
    # 1) Cohort Change Ratio / Difference for cohorts >0–9
    for (i in seq_len(n_cohorts - 1)) {
      current <- sex_age_cohorts[i]
      next_cohort <- sex_age_cohorts[i + 1]

      # Same sex & not the open-ended "75+" group
      if (substr(current, 1, 1) == substr(next_cohort, 1, 1) &&
          !grepl("75\\+$", current)) {
        next_val <- hp_proj[next_cohort, as.character(prev_year)]
        past_val <- hp_proj[current, as.character(past_offset_year)]
        if (!is.na(next_val) &&
            !is.na(past_val) && next_val > past_val) {
          diff_val <- next_val - past_val
          
          hp_proj[next_cohort, as.character(target_year)] <-
            hp_proj[current, as.character(prev_year)] + diff_val
        } else {
          # Ratio branch (CCR)
          if (!is.na(next_val) &&
              !is.na(past_val) && past_val > 0) {
            ccr <- next_val / past_val
          } else {
            ccr <- 1  # fallback to flat
          }
          ccr_matrix[current, paste0("t+", j)] <- ccr
          hp_proj[next_cohort, as.character(target_year)] <-
            hp_proj[current, as.character(prev_year)] * ccr
        }
      }
    }
    
    # 2) Child-Woman Ratio (CWR) for 0–9 cohorts, once both mother cohorts are projected
    jump_off_moms <- sum(hp_proj[mothers_idx, as.character(jump_off_year)], na.rm = TRUE)
    print(jump_off_moms)
    boys_ratio  <- hp_proj["1_0 - 9", as.character(jump_off_year)] / jump_off_moms
    print(boys_ratio)
    girls_ratio <- hp_proj["2_0 - 9", as.character(jump_off_year)] / jump_off_moms
    future_moms <- sum(hp_proj[mothers_idx, as.character(target_year-1)], na.rm = TRUE)
    
    hp_proj["1_0 - 9", as.character(target_year)] <- boys_ratio  * future_moms
    hp_proj["2_0 - 9", as.character(target_year)] <- girls_ratio * future_moms
  }
  
  # Convert to long format once, after all projections
  hp_projection_long <- tibble::as_tibble(hp_proj, rownames = "cohort") %>%
    tidyr::pivot_longer(cols = -cohort,
                        names_to = "year",
                        values_to = "projected_population") %>%
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::arrange(cohort, year)
  

# ------------------------------------------------------------------------------
  plot(hp_proj[2,], x = 2002:2031, type = "l")
  grid()
  
  
  
hp_projection_long <- hp_projection_long %>%
  group_by(cohort) %>%
  mutate(smoothed_prd = rollmean(
    projected_population,
    k = 5,
    fill = NA,
    align = "right"
  ))
  








