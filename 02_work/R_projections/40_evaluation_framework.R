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





#' Evaluate Prediction Errors by Group
#'
#' Computes error metrics for actual and predicted values by filtering for specified years,
#' grouping by "year" and an additional grouping variable.
#'
#' @param prediction_data Data frame containing the prediction data with a "year" column.
#' @param prediction_years Numeric vector of years to filter the data.
#' @param true_values Character name of the column with actual values.
#' @param prediction_values Character name of the column with predicted values.
#' @param grouping_variable Character name (or vector of names) for additional grouping.
#'
#' @return A data frame with columns for median absolute error, median percentage error,
#'   mean absolute error, and mean percentage error.
evaluate_prediction <- function(prediction_data,
                                prediction_years,
                                true_values,
                                prediction_values,
                                grouping_variable,
                                print_latex = FALSE,
                                prediction_method = NULL) {
  
  
  # evaluation functions ---------------------------------------------------------
  median_absolute_error <- function(actual, predicted) {
    median(abs(actual - predicted), na.rm = TRUE)
  }
  
  median_abs_percentage_error <- function(actual, predicted) {
    median(abs(actual - predicted) / actual * 100, na.rm = TRUE)
  }
  
  mean_absolute_error <- function(actual, predicted) {
    mean(abs(actual - predicted), na.rm = TRUE)
  }
  
  mean_abs_percentage_error <- function(actual, predicted) {
    mean(abs(actual - predicted) / actual * 100, na.rm = TRUE)
  }
 
  true_col <- sym(true_values)
  pred_col <- sym(prediction_values)
  grouping_col <- sym(grouping_variable)
  
  result <- prediction_data %>%
    ungroup() %>%
    filter(year %in% prediction_years) %>%
    group_by(year, !!grouping_col) %>%
    summarise(
      median_absolute_error = median_absolute_error(!!true_col, !!pred_col),
      median_abs_percentage_error = median_abs_percentage_error(!!true_col, !!pred_col),
      mean_absolute_error = mean_absolute_error(!!true_col, !!pred_col),
      mean_abs_percentage_error = mean_abs_percentage_error(!!true_col, !!pred_col)
    )
  colnames(result) <- c("Year", grouping_variable, "MedAE", "MedAPE", "MAE", "MAPE")
  
  if(print_latex == FALSE){
    return(result) 
  } else
  {
    latex_table <- xtable(result, caption = paste("Evaulation of", prediction_method, "prediction"), digits = 1)
    return(list(data_frame = result, 
                latex_table = latex_table))
  }
}





# loading prediciction data ----------------------------------------------------

load(file.path(wd_data_work, "munip_size_group_mapping_2021.RData"))
municipality_size_group_mapping_2021 <- municipality_size_group_mapping_2021 %>%
  mutate(population_size_group = factor(population_size_group, 
                                        levels = c("< 500", "500-1000", "1000-2000", "2000-5000", "5000-20000", "20000-50000","> 50000"),
                                        ordered = TRUE))

# evaluation along municipality size and year ----------------------------------
# LINEXP 
load(file.path(wd_res, "final_LINEXP_prediction.RData"))

LINEXP_for_evaluation <- tuned_LINEXP_pred_export %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population),
            LINEXP_population = sum(PRED_tuned_LINEXP)) %>%
  left_join(municipality_size_group_mapping_2021, by = "municipality_code") 

evaluate_prediction(LINEXP_for_evaluation,
                    prediction_years = 2022:2024,
                    true_values = "population",
                    prediction_values = "LINEXP_population",
                    grouping_variable = "population_size_group",
                    print_latex = FALSE,
                    prediction_method = "LINEXP")

LINEXP_eval <- evaluate_prediction(LINEXP_for_evaluation,
                    prediction_years = 2022:2024,
                    true_values = "population",
                    prediction_values = "LINEXP_population",
                    grouping_variable = "population_size_group",
                    print_latex = FALSE,
                    prediction_method = "LINEXP")

# balanced LINEXP
load(file.path(wd_res, "final_LINEXP_balanced.RData"))

balanced_LINEXP_for_evaluation <- balanced_LINEXP_pred %>%
  mutate(sex = as.character(sex)) %>%
  ungroup() %>%
  select(municipality_code, sex, age_group, year, balanced_pred) %>%
  left_join(select(tuned_LINEXP_pred_export, municipality_code, sex, age_group, year, population),
            by = join_by(municipality_code, sex, age_group, year)) %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population),
            balanced_LINEXP_population = sum(balanced_pred)) %>%
  left_join(municipality_size_group_mapping_2021, by = "municipality_code")

balanced_LINEXP_eval <- evaluate_prediction(balanced_LINEXP_for_evaluation,
                    prediction_years = 2022:2024,
                    true_values = "population",
                    prediction_values = "balanced_LINEXP_population",
                    grouping_variable = "population_size_group",
                    print_latex = FALSE,
                    prediction_method = "LINEXP")


# Hamilton Perry ---------------------------------------------------------------
load(file.path(wd_res, "final_HP_prediction.RData"))

HP_for_evaluation <- hp_prediction_export %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population),
            hp_population = sum(PRED_hamilton_perry)) %>%
  left_join(municipality_size_group_mapping_2021, by = "municipality_code") 

hp_eval <- evaluate_prediction(HP_for_evaluation,
                    prediction_years = 2022:2024,
                    true_values = "population",
                    prediction_values = "hp_population",
                    grouping_variable = "population_size_group",
                    print_latex = FALSE,
                    prediction_method = "HP")

# balanced Hamitlon Perry 
# balanced LINEXP
load(file.path(wd_res, "final_HP_balanced.RData"))

balanced_hp_for_evaluation <- balanced_hp_pred %>%
  mutate(sex = as.character(sex)) %>%
  ungroup() %>%
  select(municipality_code, sex, age_group, year, balanced_pred) %>%
  left_join(select(tuned_LINEXP_pred_export, municipality_code, sex, age_group, year, population),
            by = join_by(municipality_code, sex, age_group, year)) %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population),
            balanced_hp_population = sum(balanced_pred)) %>%
  left_join(municipality_size_group_mapping_2021, by = "municipality_code")

balanced_hp_eval <- evaluate_prediction(balanced_hp_for_evaluation,
                    prediction_years = 2022:2024,
                    true_values = "population",
                    prediction_values = "balanced_hp_population",
                    grouping_variable = "population_size_group",
                    print_latex = FALSE,
                    prediction_method = "HP")


output_table <- LINEXP_eval %>%
  left_join(balanced_LINEXP_eval, by = join_by(Year, population_size_group), suffix = c("LINEXP", "balanced_LINEXP")) %>%
  left_join(hp_eval, by = join_by(Year, population_size_group), suffix = c("", "HP")) %>%
  left_join(balanced_hp_eval, by = join_by(Year, population_size_group), suffix = c("", "balanced_HP"))
  
write.csv(output_table, file = file.path(wd_res, "eval_LINEXP_hp_size.csv"))




# evaluation along age_group and year ----------------------------------
# LINEXP 
LINEXP_for_evaluation <- tuned_LINEXP_pred_export %>%
  group_by(municipality_code, age_group, year) %>%
  summarise(population = sum(population),
            LINEXP_population = sum(PRED_tuned_LINEXP)) 

LINEXP_eval <- evaluate_prediction(LINEXP_for_evaluation,
                                   prediction_years = 2022:2024,
                                   true_values = "population",
                                   prediction_values = "LINEXP_population",
                                   grouping_variable = "age_group",
                                   print_latex = FALSE,
                                   prediction_method = "LINEXP")

# balanced LINEXP
balanced_LINEXP_for_evaluation <- balanced_LINEXP_pred %>%
  mutate(sex = as.character(sex)) %>%
  ungroup() %>%
  select(municipality_code, sex, age_group, year, balanced_pred) %>%
  left_join(select(tuned_LINEXP_pred_export, municipality_code, sex, age_group, year, population),
            by = join_by(municipality_code, sex, age_group, year)) %>%
  group_by(municipality_code, age_group, year) %>%
  summarise(population = sum(population),
            balanced_LINEXP_population = sum(balanced_pred))

balanced_LINEXP_eval <- evaluate_prediction(balanced_LINEXP_for_evaluation,
                                            prediction_years = 2022:2024,
                                            true_values = "population",
                                            prediction_values = "balanced_LINEXP_population",
                                            grouping_variable = "age_group",
                                            print_latex = FALSE,
                                            prediction_method = "LINEXP")


# Hamilton Perry ---------------------------------------------------------------
HP_for_evaluation <- hp_prediction_export %>%
  group_by(municipality_code, age_group, year) %>%
  summarise(population = sum(population),
            hp_population = sum(PRED_hamilton_perry))

hp_eval <- evaluate_prediction(HP_for_evaluation,
                               prediction_years = 2022:2024,
                               true_values = "population",
                               prediction_values = "hp_population",
                               grouping_variable = "age_group",
                               print_latex = FALSE,
                               prediction_method = "HP")

# balanced Hamitlon Perry 
# balanced LINEXP
balanced_hp_for_evaluation <- balanced_hp_pred %>%
  mutate(sex = as.character(sex)) %>%
  ungroup() %>%
  select(municipality_code, sex, age_group, year, balanced_pred) %>%
  left_join(select(tuned_LINEXP_pred_export, municipality_code, sex, age_group, year, population),
            by = join_by(municipality_code, sex, age_group, year)) %>%
  group_by(municipality_code, age_group, year) %>%
  summarise(population = sum(population),
            balanced_hp_population = sum(balanced_pred)) 

balanced_hp_eval <- evaluate_prediction(balanced_hp_for_evaluation,
                                        prediction_years = 2022:2024,
                                        true_values = "population",
                                        prediction_values = "balanced_hp_population",
                                        grouping_variable = "age_group",
                                        print_latex = FALSE,
                                        prediction_method = "HP")


output_table <- LINEXP_eval %>%
  left_join(balanced_LINEXP_eval, by = join_by(Year, age_group), suffix = c("LINEXP", "balanced_LINEXP")) %>%
  left_join(hp_eval, by = join_by(Year, age_group), suffix = c("", "HP")) %>%
  left_join(balanced_hp_eval, by = join_by(Year, age_group), suffix = c("", "balanced_HP"))

write.csv(output_table, file = file.path(wd_res, "eval_LINEXP_hp_age.csv"))


# evaluation along Bundesland and year ----------------------------------
# LINEXP 
LINEXP_for_evaluation <- tuned_LINEXP_pred_export %>%
  mutate(bundesland = substr(municipality_code, 1,1)) %>%
  group_by(municipality_code, bundesland, year) %>%
  summarise(population = sum(population),
            LINEXP_population = sum(PRED_tuned_LINEXP)) 

LINEXP_eval <- evaluate_prediction(LINEXP_for_evaluation,
                                   prediction_years = 2022:2024,
                                   true_values = "population",
                                   prediction_values = "LINEXP_population",
                                   grouping_variable = "bundesland",
                                   print_latex = FALSE,
                                   prediction_method = "LINEXP")

# balanced LINEXP
balanced_LINEXP_for_evaluation <- balanced_LINEXP_pred %>%
  mutate(bundesland = substr(municipality_code, 1,1)) %>%
  mutate(sex = as.character(sex)) %>%
  ungroup() %>%
  select(bundesland, municipality_code, sex, age_group, year, balanced_pred) %>%
  left_join(select(tuned_LINEXP_pred_export, municipality_code, sex, age_group, year, population),
            by = join_by(municipality_code, sex, age_group, year)) %>%
  group_by(municipality_code, bundesland, year) %>%
  summarise(population = sum(population),
            balanced_LINEXP_population = sum(balanced_pred))

balanced_LINEXP_eval <- evaluate_prediction(balanced_LINEXP_for_evaluation,
                                            prediction_years = 2022:2024,
                                            true_values = "population",
                                            prediction_values = "balanced_LINEXP_population",
                                            grouping_variable = "bundesland",
                                            print_latex = FALSE,
                                            prediction_method = "LINEXP")


# Hamilton Perry ---------------------------------------------------------------
HP_for_evaluation <- hp_prediction_export %>%
  mutate(bundesland = substr(municipality_code, 1,1)) %>%
  group_by(municipality_code, bundesland, year) %>%
  summarise(population = sum(population),
            hp_population = sum(PRED_hamilton_perry))

hp_eval <- evaluate_prediction(HP_for_evaluation,
                               prediction_years = 2022:2024,
                               true_values = "population",
                               prediction_values = "hp_population",
                               grouping_variable = "bundesland",
                               print_latex = FALSE,
                               prediction_method = "HP")

# balanced Hamitlon Perry 
balanced_hp_for_evaluation <- balanced_hp_pred %>%
  mutate(bundesland = substr(municipality_code, 1,1)) %>%
  mutate(sex = as.character(sex)) %>%
  ungroup() %>%
  select(bundesland, municipality_code, sex, age_group, year, balanced_pred) %>%
  left_join(select(tuned_LINEXP_pred_export, municipality_code, sex, age_group, year, population),
            by = join_by(municipality_code, sex, age_group, year)) %>%
  group_by(municipality_code, bundesland, year) %>%
  summarise(population = sum(population),
            balanced_hp_population = sum(balanced_pred)) 

balanced_hp_eval <- evaluate_prediction(balanced_hp_for_evaluation,
                                        prediction_years = 2022:2024,
                                        true_values = "population",
                                        prediction_values = "balanced_hp_population",
                                        grouping_variable = "bundesland",
                                        print_latex = FALSE,
                                        prediction_method = "HP")


output_table <- LINEXP_eval %>%
  left_join(balanced_LINEXP_eval, by = join_by(Year, bundesland), suffix = c("LINEXP", "balanced_LINEXP")) %>%
  left_join(hp_eval, by = join_by(Year, bundesland), suffix = c("", "HP")) %>%
  left_join(balanced_hp_eval, by = join_by(Year, bundesland), suffix = c("HP", "balanced_HP"))

write.csv(output_table, file = file.path(wd_res, "eval_LINEXP_hp_bundesland.csv"))




# evaluation along Carinithia, size and year ----------------------------------
# LINEXP 
LINEXP_for_evaluation <- tuned_LINEXP_pred_export %>%
  filter(substr(municipality_code, 1,1) == 2) %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population),
            LINEXP_population = sum(PRED_tuned_LINEXP)) %>%
  left_join(municipality_size_group_mapping_2021, by = "municipality_code")

LINEXP_eval <- evaluate_prediction(LINEXP_for_evaluation,
                                   prediction_years = 2022:2024,
                                   true_values = "population",
                                   prediction_values = "LINEXP_population",
                                   grouping_variable = "population_size_group",
                                   print_latex = FALSE,
                                   prediction_method = "LINEXP")

# balanced LINEXP
balanced_LINEXP_for_evaluation <- balanced_LINEXP_pred %>%
  filter(substr(municipality_code, 1,1) == 2) %>%
  mutate(sex = as.character(sex)) %>%
  ungroup() %>%
  select(municipality_code, sex, age_group, year, balanced_pred) %>%
  left_join(select(tuned_LINEXP_pred_export, municipality_code, sex, age_group, year, population),
            by = join_by(municipality_code, sex, age_group, year)) %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population),
            balanced_LINEXP_population = sum(balanced_pred))%>%
  left_join(municipality_size_group_mapping_2021, by = "municipality_code")

balanced_LINEXP_eval <- evaluate_prediction(balanced_LINEXP_for_evaluation,
                                            prediction_years = 2022:2024,
                                            true_values = "population",
                                            prediction_values = "balanced_LINEXP_population",
                                            grouping_variable = "population_size_group",
                                            print_latex = FALSE,
                                            prediction_method = "LINEXP")


# Hamilton Perry 
HP_for_evaluation <- hp_prediction_export %>%
  filter(substr(municipality_code, 1,1) == 2) %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population),
            hp_population = sum(PRED_hamilton_perry)) %>%
  left_join(municipality_size_group_mapping_2021, by = "municipality_code")

hp_eval <- evaluate_prediction(HP_for_evaluation,
                               prediction_years = 2022:2024,
                               true_values = "population",
                               prediction_values = "hp_population",
                               grouping_variable = "population_size_group",
                               print_latex = FALSE,
                               prediction_method = "HP")

# balanced Hamitlon Perry 
balanced_hp_for_evaluation <- balanced_hp_pred %>%
  filter(substr(municipality_code, 1,1) == 2) %>%
  mutate(sex = as.character(sex)) %>%
  ungroup() %>%
  select(municipality_code, sex, age_group, year, balanced_pred) %>%
  left_join(select(tuned_LINEXP_pred_export, municipality_code, sex, age_group, year, population),
            by = join_by(municipality_code, sex, age_group, year)) %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population),
            balanced_hp_population = sum(balanced_pred)) %>%
  left_join(municipality_size_group_mapping_2021, by = "municipality_code")

balanced_hp_eval <- evaluate_prediction(balanced_hp_for_evaluation,
                                        prediction_years = 2022:2024,
                                        true_values = "population",
                                        prediction_values = "balanced_hp_population",
                                        grouping_variable = "population_size_group",
                                        print_latex = FALSE,
                                        prediction_method = "HP")


output_table <- LINEXP_eval %>%
  left_join(balanced_LINEXP_eval, by = join_by(Year, population_size_group), suffix = c("LINEXP", "balanced_LINEXP")) %>%
  left_join(hp_eval, by = join_by(Year, population_size_group), suffix = c("", "HP")) %>%
  left_join(balanced_hp_eval, by = join_by(Year, population_size_group), suffix = c("HP", "balanced_HP"))

write.csv(output_table, file = file.path(wd_res, "eval_LINEXP_hp_size_carinthia.csv"))


# evaluation along Carinithia, age and year ----------------------------------
# LINEXP 
LINEXP_for_evaluation <- tuned_LINEXP_pred_export %>%
  mutate(bundesland = substr(municipality_code, 1,1)) %>%
  filter(substr(municipality_code, 1,1) == 2) %>%
  group_by(municipality_code, age_group, year) %>%
  summarise(population = sum(population),
            LINEXP_population = sum(PRED_tuned_LINEXP)) 

LINEXP_eval <- evaluate_prediction(LINEXP_for_evaluation,
                                   prediction_years = 2022:2024,
                                   true_values = "population",
                                   prediction_values = "LINEXP_population",
                                   grouping_variable = "age_group",
                                   print_latex = FALSE,
                                   prediction_method = "LINEXP")

# balanced LINEXP
balanced_LINEXP_for_evaluation <- balanced_LINEXP_pred %>%
  mutate(bundesland = substr(municipality_code, 1,1)) %>%
  filter(substr(municipality_code, 1,1) == 2) %>%
  mutate(sex = as.character(sex)) %>%
  ungroup() %>%
  select(municipality_code, sex, age_group, year, balanced_pred) %>%
  left_join(select(tuned_LINEXP_pred_export, municipality_code, sex, age_group, year, population),
            by = join_by(municipality_code, sex, age_group, year)) %>%
  group_by(municipality_code, age_group, year) %>%
  summarise(population = sum(population),
            balanced_LINEXP_population = sum(balanced_pred))

balanced_LINEXP_eval <- evaluate_prediction(balanced_LINEXP_for_evaluation,
                                            prediction_years = 2022:2024,
                                            true_values = "population",
                                            prediction_values = "balanced_LINEXP_population",
                                            grouping_variable = "age_group",
                                            print_latex = FALSE,
                                            prediction_method = "LINEXP")


# Hamilton Perry 
HP_for_evaluation <- hp_prediction_export %>%
  mutate(bundesland = substr(municipality_code, 1,1)) %>%
  filter(substr(municipality_code, 1,1) == 2) %>%
  group_by(municipality_code, age_group, year) %>%
  summarise(population = sum(population),
            hp_population = sum(PRED_hamilton_perry))

hp_eval <- evaluate_prediction(HP_for_evaluation,
                               prediction_years = 2022:2024,
                               true_values = "population",
                               prediction_values = "hp_population",
                               grouping_variable = "age_group",
                               print_latex = FALSE,
                               prediction_method = "HP")

# balanced Hamitlon Perry 
balanced_hp_for_evaluation <- balanced_hp_pred %>%
  mutate(bundesland = substr(municipality_code, 1,1)) %>%
  filter(substr(municipality_code, 1,1) == 2) %>%
  mutate(sex = as.character(sex)) %>%
  ungroup() %>%
  select(municipality_code, sex, age_group, year, balanced_pred) %>%
  left_join(select(tuned_LINEXP_pred_export, municipality_code, sex, age_group, year, population),
            by = join_by(municipality_code, sex, age_group, year)) %>%
  group_by(municipality_code, age_group, year) %>%
  summarise(population = sum(population),
            balanced_hp_population = sum(balanced_pred))

balanced_hp_eval <- evaluate_prediction(balanced_hp_for_evaluation,
                                        prediction_years = 2022:2024,
                                        true_values = "population",
                                        prediction_values = "balanced_hp_population",
                                        grouping_variable = "age_group",
                                        print_latex = FALSE,
                                        prediction_method = "HP")


output_table <- LINEXP_eval %>%
  left_join(balanced_LINEXP_eval, by = join_by(Year, age_group), suffix = c("LINEXP", "balanced_LINEXP")) %>%
  left_join(hp_eval, by = join_by(Year, age_group), suffix = c("", "HP")) %>%
  left_join(balanced_hp_eval, by = join_by(Year, age_group), suffix = c("HP", "balanced_HP"))

write.csv(output_table, file = file.path(wd_res, "eval_LINEXP_hp_age_group_carinthia.csv"))




# evaluation along Vienna, size and year ----------------------------------
# LINEXP 
LINEXP_for_evaluation <- tuned_LINEXP_pred_export %>%
  filter(substr(municipality_code, 1,1) == 9) %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population),
            LINEXP_population = sum(PRED_tuned_LINEXP)) %>%
  left_join(municipality_size_group_mapping_2021, by = "municipality_code")

LINEXP_eval <- evaluate_prediction(LINEXP_for_evaluation,
                                   prediction_years = 2022:2024,
                                   true_values = "population",
                                   prediction_values = "LINEXP_population",
                                   grouping_variable = "population_size_group",
                                   print_latex = FALSE,
                                   prediction_method = "LINEXP")

# balanced LINEXP
balanced_LINEXP_for_evaluation <- balanced_LINEXP_pred %>%
  filter(substr(municipality_code, 1,1) == 9) %>%
  mutate(sex = as.character(sex)) %>%
  ungroup() %>%
  select(municipality_code, sex, age_group, year, balanced_pred) %>%
  left_join(select(tuned_LINEXP_pred_export, municipality_code, sex, age_group, year, population),
            by = join_by(municipality_code, sex, age_group, year)) %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population),
            balanced_LINEXP_population = sum(balanced_pred))%>%
  left_join(municipality_size_group_mapping_2021, by = "municipality_code")

balanced_LINEXP_eval <- evaluate_prediction(balanced_LINEXP_for_evaluation,
                                            prediction_years = 2022:2024,
                                            true_values = "population",
                                            prediction_values = "balanced_LINEXP_population",
                                            grouping_variable = "population_size_group",
                                            print_latex = FALSE,
                                            prediction_method = "LINEXP")


# Hamilton Perry 
HP_for_evaluation <- hp_prediction_export %>%
  filter(substr(municipality_code, 1,1) == 9) %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population),
            hp_population = sum(PRED_hamilton_perry)) %>%
  left_join(municipality_size_group_mapping_2021, by = "municipality_code")

hp_eval <- evaluate_prediction(HP_for_evaluation,
                               prediction_years = 2022:2024,
                               true_values = "population",
                               prediction_values = "hp_population",
                               grouping_variable = "population_size_group",
                               print_latex = FALSE,
                               prediction_method = "HP")

# balanced Hamitlon Perry 
balanced_hp_for_evaluation <- balanced_hp_pred %>%
  filter(substr(municipality_code, 1,1) == 9) %>%
  mutate(sex = as.character(sex)) %>%
  ungroup() %>%
  select(municipality_code, sex, age_group, year, balanced_pred) %>%
  left_join(select(tuned_LINEXP_pred_export, municipality_code, sex, age_group, year, population),
            by = join_by(municipality_code, sex, age_group, year)) %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population),
            balanced_hp_population = sum(balanced_pred)) %>%
  left_join(municipality_size_group_mapping_2021, by = "municipality_code")

balanced_hp_eval <- evaluate_prediction(balanced_hp_for_evaluation,
                                        prediction_years = 2022:2024,
                                        true_values = "population",
                                        prediction_values = "balanced_hp_population",
                                        grouping_variable = "population_size_group",
                                        print_latex = FALSE,
                                        prediction_method = "HP")


output_table <- LINEXP_eval %>%
  left_join(balanced_LINEXP_eval, by = join_by(Year, population_size_group), suffix = c("LINEXP", "balanced_LINEXP")) %>%
  left_join(hp_eval, by = join_by(Year, population_size_group), suffix = c("", "HP")) %>%
  left_join(balanced_hp_eval, by = join_by(Year, population_size_group), suffix = c("HP", "balanced_HP"))

write.csv(output_table, file = file.path(wd_res, "eval_LINEXP_hp_size_vienna.csv"))


# evaluation along Carinithia, age and year ----------------------------------
# LINEXP 
LINEXP_for_evaluation <- tuned_LINEXP_pred_export %>%
  filter(substr(municipality_code, 1,1) == 9) %>%
  group_by(municipality_code, age_group, year) %>%
  summarise(population = sum(population),
            LINEXP_population = sum(PRED_tuned_LINEXP)) 

LINEXP_eval <- evaluate_prediction(LINEXP_for_evaluation,
                                   prediction_years = 2022:2024,
                                   true_values = "population",
                                   prediction_values = "LINEXP_population",
                                   grouping_variable = "age_group",
                                   print_latex = FALSE,
                                   prediction_method = "LINEXP")

# balanced LINEXP
balanced_LINEXP_for_evaluation <- balanced_LINEXP_pred %>%
  filter(substr(municipality_code, 1,1) == 9) %>%
  mutate(sex = as.character(sex)) %>%
  ungroup() %>%
  select(municipality_code, sex, age_group, year, balanced_pred) %>%
  left_join(select(tuned_LINEXP_pred_export, municipality_code, sex, age_group, year, population),
            by = join_by(municipality_code, sex, age_group, year)) %>%
  group_by(municipality_code, age_group, year) %>%
  summarise(population = sum(population),
            balanced_LINEXP_population = sum(balanced_pred))

balanced_LINEXP_eval <- evaluate_prediction(balanced_LINEXP_for_evaluation,
                                            prediction_years = 2022:2024,
                                            true_values = "population",
                                            prediction_values = "balanced_LINEXP_population",
                                            grouping_variable = "age_group",
                                            print_latex = FALSE,
                                            prediction_method = "LINEXP")


# Hamilton Perry 
HP_for_evaluation <- hp_prediction_export %>%
  filter(substr(municipality_code, 1,1) == 9) %>%
  group_by(municipality_code, age_group, year) %>%
  summarise(population = sum(population),
            hp_population = sum(PRED_hamilton_perry))

hp_eval <- evaluate_prediction(HP_for_evaluation,
                               prediction_years = 2022:2024,
                               true_values = "population",
                               prediction_values = "hp_population",
                               grouping_variable = "age_group",
                               print_latex = FALSE,
                               prediction_method = "HP")

# balanced Hamitlon Perry 
balanced_hp_for_evaluation <- balanced_hp_pred %>%
  filter(substr(municipality_code, 1,1) == 9) %>%
  mutate(sex = as.character(sex)) %>%
  ungroup() %>%
  select(municipality_code, sex, age_group, year, balanced_pred) %>%
  left_join(select(tuned_LINEXP_pred_export, municipality_code, sex, age_group, year, population),
            by = join_by(municipality_code, sex, age_group, year)) %>%
  group_by(municipality_code, age_group, year) %>%
  summarise(population = sum(population),
            balanced_hp_population = sum(balanced_pred))

balanced_hp_eval <- evaluate_prediction(balanced_hp_for_evaluation,
                                        prediction_years = 2022:2024,
                                        true_values = "population",
                                        prediction_values = "balanced_hp_population",
                                        grouping_variable = "age_group",
                                        print_latex = FALSE,
                                        prediction_method = "HP")


output_table <- LINEXP_eval %>%
  left_join(balanced_LINEXP_eval, by = join_by(Year, age_group), suffix = c("LINEXP", "balanced_LINEXP")) %>%
  left_join(hp_eval, by = join_by(Year, age_group), suffix = c("", "HP")) %>%
  left_join(balanced_hp_eval, by = join_by(Year, age_group), suffix = c("HP", "balanced_HP"))

write.csv(output_table, file = file.path(wd_res, "eval_LINEXP_hp_age_group_vienna.csv"))



load(file.path(wd_data_work, "all_municipalities_population.RData"))
colnames(all_munip_pop)
load(file.path(wd_data_work, "munip_size_group_mapping_2021.RData"))
municipality_size_group_mapping_2021 <- municipality_size_group_mapping_2021 %>%
  mutate(population_size_group = factor(population_size_group, 
                                        levels = c("< 500", "500-1000", "1000-2000", "2000-5000", "5000-20000", "20000-50000","> 50000"),
                                        ordered = TRUE))

all_pop_for_evaluation <- all_munip_pop %>% left_join(municipality_size_group_mapping_2021) %>% relocate(population_size_group, .after = year) %>%
  select(-smoothed_population, -reg_code, -municipality) %>%
  rename(age_group = coarse_age_group) %>% 
  mutate(sex = as.character(sex))

load(file.path(wd_res, "final_HP_test_pred_2022-2024.RData"))
hp_prediction_export <- hp_prediction_export %>% mutate(sex = as.character(sex))
colnames(hp_prediction_export)
load(file.path(wd_res, "final_LINEXP_test_pred_2022-2024.RData"))
tuned_LINEXP_test_export <- tuned_LINEXP_test_export %>% mutate(sex = as.character(sex))
colnames(tuned_LINEXP_test_export)




  






  
 

evaluate_predictions <- function(prediction_dfs,
                                 true_value_df,
                                 true_pop_col,      # bare name, no quotes
                                 pred_cols,         # still strings OK
                                 grouping_col,      # bare name, no quotes
                                 prediction_years = 2022:2024) {

  median_abs_perc_error <- function(actual, predicted) {
    median(abs(actual - predicted) / actual * 100, na.rm = TRUE)
  }
  
  # capture the bare names
  tv_col  <- enquo(true_pop_col)
  grp_col <- enquo(grouping_col)
  
  # filter
  tv <- true_value_df %>%
    filter(year %in% prediction_years)
  
  # join
  for(df in prediction_dfs){
    tv <- tv %>%
      left_join(select(df, -population),
                by = join_by(municipality_code, age_group, sex, year))
  }
  
  # aggregate
  agg <- tv %>%
    group_by(year, municipality_code, !!grp_col) %>%
    summarise(across(all_of(c(as_name(tv_col), pred_cols)), sum),
              .groups = "drop")
  
  # compute medAPE
  med_ape_df <- agg %>%
    group_by(year, !!grp_col) %>%
    summarise(across(all_of(pred_cols),
                     ~ median_abs_perc_error(!!tv_col, .x)),
              .groups = "drop")
  
  med_ape_df
}

# call with bare names:
evaluate_predictions(
  prediction_dfs  = list(tuned_LINEXP_test_export,
                         hp_prediction_export),
  true_value_df   = all_pop_for_evaluation,
  true_pop_col    = population,
  pred_cols       = c("PRED_tuned_LINEXP",
                      "PRED_hamilton_perry"),
  grouping_col    = population_size_group,
  prediction_years = 2022:2024
)
