############################################################################## #
# Filename
#    04_hamilton_perry.R
#
# Description
#   Projection with Hamilton-Perry method such as in Hauer 2018
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      2025-04-07
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #


##############################################################################~#
# Data reading #################################################################
load(file.path(wd_data_work, "all_municipalities_population.RData"))


# Hamilton/Perry prediction ----------------------------------------------------
return_hp_projection <- function(data,
                                 n_prediction_periods = 3,
                                 jump_off_year = 2021) {
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
  
  # Init matrices
  ccr_matrix <- matrix(
    NA,
    nrow = length(sex_age_cohorts),
    ncol = n_prediction_periods,
    dimnames = list(sex_age_cohorts, paste0("t+", 1:n_prediction_periods))
  )
  
  hp_projection_matrix <- matrix(
    NA,
    nrow = length(sex_age_cohorts),
    ncol = length(all_years),
    dimnames = list(sex_age_cohorts, all_years)
  )
  
  # Historical values
  for (i in seq_along(sex_age_cohorts)) {
    hp_projection_matrix[i, as.character(2002:jump_off_year)] <-
      data$population[data$cohort == sex_age_cohorts[i] &
                        data$year %in% 2002:jump_off_year]
  }
  
  # CCR/CCD projections
  for (i in 1:(length(sex_age_cohorts) - 1)) {
    current <- sex_age_cohorts[i]
    next_cohort <- sex_age_cohorts[i + 1]
    
    if (grepl("^1|^2", current) &&
        substr(current, 1, 1) == substr(next_cohort, 1, 1) &&
        !grepl("75\\+$", current)) {
      hist_years <- (jump_off_year - 11):jump_off_year
      pop_trend <- data$population[data$cohort == current &
                                     data$year %in% hist_years]
      
      total_change <- pop_trend[length(pop_trend)] - pop_trend[1]
      
      use_ccd <- total_change > 0
      
      base_pop <- data$population[data$cohort == current &
                                    data$year == jump_off_year]
      
      for (j in 1:n_prediction_periods) {
        target_year <- jump_off_year + j
        past_year <- jump_off_year - j
        
        next_value <- data$population[data$cohort == next_cohort &
                                        data$year == jump_off_year]
        past_value <- data$population[data$cohort == current &
                                        data$year == past_year]
        
        if (use_ccd &&
            length(next_value) > 0 && length(past_value) > 0) {
          diff_value <- next_value - past_value
          hp_projection_matrix[i + 1, as.character(target_year)] <- base_pop + diff_value
        } else {
          # CCR branch + zero‐denominator catch
          if (length(next_value)>0 && length(past_value)>0 && past_value > 0) {
            ccr <- next_value / past_value
          } else {
            ccr <- 1   # FALLBACK:  assume flat
          }
          ccr_matrix[i, j] <- ccr
          hp_projection_matrix[i+1, as.character(target_year)] <- base_pop * ccr
        }
      }
    }
  }
  
  # Child-Woman Ratio logic
  potential_mothers <- c("2_20 - 29", "2_30 - 44")
  potential_mother_rows <- rownames(hp_projection_matrix) %in% potential_mothers
  
  jump_off_potential_mothers <- hp_projection_matrix[potential_mother_rows, as.character(jump_off_year)]
  jump_off_boys <- hp_projection_matrix["1_0 - 9", as.character(jump_off_year)]
  jump_off_girls <- hp_projection_matrix["2_0 - 9", as.character(jump_off_year)]
  
  boys_woman_ratio <- jump_off_boys / sum(jump_off_potential_mothers, na.rm = TRUE)
  girls_woman_ratio <- jump_off_girls / sum(jump_off_potential_mothers, na.rm = TRUE)
  
  future_years <- as.character(jump_off_year + 1:n_prediction_periods)
  future_potential_mothers <- colSums(hp_projection_matrix[potential_mother_rows, future_years, drop = FALSE], na.rm = TRUE)
  
  hp_projection_matrix["1_0 - 9", future_years] <- boys_woman_ratio * future_potential_mothers
  hp_projection_matrix["2_0 - 9", future_years] <- girls_woman_ratio * future_potential_mothers
  
  # Return as long tibble
  hp_projection_long <- as_tibble(hp_projection_matrix, rownames = "cohort") %>%
    pivot_longer(cols = -cohort,
                 names_to = "year",
                 values_to = "projected_population") %>%
    mutate(year = as.numeric(year)) %>%
    arrange(cohort, year)
  
  return(hp_projection_long)
}



# test prediction --------------------------------------------------------------
hp_data <- all_munip_pop %>% filter(year %in% c(2002:2024)) %>%
  rename(age_group = coarse_age_group) %>%
  unite("cohort", c("sex", "age_group")) %>%
  select(municipality_code, cohort, year, smoothed_population) %>%
  rename(population = smoothed_population) %>%
  mutate(year = as.character(year))


hp_test <- hp_data %>%
  group_by(municipality_code) %>%
  group_modify( ~ return_hp_projection(.x)) %>%
  ungroup()




hp_test_export <- hp_test %>%
  mutate(year = as.character(year)) %>%
  left_join(hp_data, 
            by = join_by(municipality_code, cohort, year)) %>%
  separate(cohort, 
           into = c("sex", "age_group"),
           sep = "_") %>%
  rename(smoothed_population = population) %>%
  mutate(year = as.numeric(year),
         sex = as.numeric(sex)) %>%
  left_join(select(all_munip_pop, -smoothed_population), 
            by = join_by(municipality_code == municipality_code,
                         sex == sex, 
                         age_group == coarse_age_group,
                         year == year)) %>%
  select(-municipality) %>%
  rename(PRED_hamilton_perry = projected_population) %>%
  mutate(year = as.numeric(year)) %>%
  select(municipality_code, sex, age_group, year, population, PRED_hamilton_perry)

save(hp_test_export,
     file= file.path(wd_res, "final_HP_prediction.RData"))




# HP (actual) prediction -------------------------------------------------------
jump_off_year <- 2024
prediction_periods <- 11

hp_pred <- hp_data %>%
  group_by(municipality_code) %>%
  group_modify( ~ return_hp_projection(.x,
                                       n_prediction_periods = prediction_periods, 
                                       jump_off_year = jump_off_year)) %>%
  ungroup()


hp_pred_export <- hp_pred %>%
  mutate(year = as.character(year)) %>%
  left_join(hp_data, 
            by = join_by(municipality_code, cohort, year)) %>%
  separate(cohort, 
           into = c("sex", "age_group"),
           sep = "_") %>%
  rename(smoothed_population = population) %>%
  mutate(year = as.numeric(year),
         sex = as.numeric(sex)) %>%
  left_join(select(all_munip_pop, -smoothed_population), 
            by = join_by(municipality_code == municipality_code,
                         sex == sex, 
                         age_group == coarse_age_group,
                         year == year)) %>%
  select(-municipality) %>%
  rename(PRED_hamilton_perry = projected_population) %>%
  mutate(year = as.numeric(year)) %>%
  select(municipality_code, sex, age_group, year, population, PRED_hamilton_perry)


save(hp_pred_export,
     file= file.path(wd_res, "25-35_HP_prediction.RData"))


# visualisations ---------------------------------------------------------------
load(file= file.path(wd_res, "final_HP_prediction.RData"))

plot_prediction(train_data = hp_prediction_export %>% dplyr::filter(year %in% 2002:2021),
                test_data = hp_prediction_export %>% dplyr::filter(year %in% 2022:2024),
                prediction_data = hp_prediction_export %>% dplyr::filter(year %in% 2022:2024),
                train_col_name = "population",
                test_col_name = "population",
                prediction_col_name = "PRED_hamilton_perry",
                municipality_code = "70812",
                sex = 1,
                age_group = "20 - 29",
                prediction_method = "HP")



plot_prediction(train_data = hp_prediction_export %>% dplyr::filter(year %in% 2002:2021),
                test_data = hp_prediction_export %>% dplyr::filter(year %in% 2022:2024),
                prediction_data = hp_prediction_export %>% dplyr::filter(year %in% 2022:2024),
                train_col_name = "population",
                test_col_name = "population",
                prediction_col_name = "PRED_hamilton_perry",
                municipality_code = "10423",
                sex = 2,
                age_group = "75+",
                prediction_method = "HP")

plot_prediction(train_data = hp_prediction_export %>% dplyr::filter(year %in% 2002:2021),
                test_data = hp_prediction_export %>% dplyr::filter(year %in% 2022:2024),
                prediction_data = hp_prediction_export %>% dplyr::filter(year %in% 2022:2024),
                train_col_name = "population",
                test_col_name = "population",
                prediction_col_name = "PRED_hamilton_perry",
                municipality_code = "60624",
                sex = 1,
                age_group = "0 - 9",
                prediction_method = "HP")
