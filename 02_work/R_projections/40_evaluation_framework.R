############################################################################## #
# Filename
#    40_evaluation_framework.R
#
# Description
#   Evaluation of predictions
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      20xx-xx-xx
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #


# evaluation functions ---------------------------------------------------------
median_absolute_error <- function(actual, predicted) {
  median(abs(actual - predicted), na.rm = TRUE)
}

median_percentage_error <- function(actual, predicted) {
  median((actual - predicted) / actual * 100, na.rm = TRUE)
}

mean_absolute_error <- function(actual, predicted) {
  mean(abs(actual - predicted), na.rm = TRUE)
}

mean_percentage_error <- function(actual, predicted) {
  mean((actual - predicted) / actual * 100, na.rm = TRUE)
}


# loading prediciction data ----------------------------------------------------
load(file.path(wd_res, "final_LINEXP_prediction.RData"))
load(file.path(wd_data_work, "munip_size_group_mapping_2021.RData"))

# LINEXP -----------------------------------------------------------------------
LINEXP_for_evaluation <- tuned_LINEXP_pred_export %>%
  separate(index,
           into = c("municipality_code", "sex", "age_group"),
           sep = "_") %>%
  filter(year %in% 2022:2024) %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population),
            LINEXP_population = sum(PRED_tuned_LINEXP)) %>%
  left_join(municipality_size_group_mapping, by = "municipality_code") 

  LINEXP_for_evaluation2 <- LINEXP_for_evaluation %>%
    group_by(population_size_group, year) %>%
    summarise(MAE = median_absolute_error(population, LINEXP_population))




evaluate_prediction <- function(prediction_data,
                                prediction_years,
                                true_values,
                                prediction_values,
                                grouping_variable) {
  
}




