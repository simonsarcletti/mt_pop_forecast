############################################################################## #
# Filename
#    42_eval_projections.R
#
# Description
#   Evaluation of many predictions simultanously
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      20xx-xx-xx
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #

# evaluation function -----------------------------------------------------------
evaluate_predictions <- function(prediction_dfs,
                                 true_value_df,
                                 true_pop_col,      # bare name, no quotes
                                 pred_cols,         # still strings OK
                                 grouping_col,      # bare name, no quotes
                                 prediction_years = 2022:2024,
                                 grouping_col_nice = NULL,
                                 model_names = NULL,
                                 print_latex = FALSE) {
  
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
  
  
  
  if(print_latex == FALSE){
    return(med_ape_df) 
  } else
  {
    colnames(med_ape_df) <- c("Year", grouping_col_nice, model_names)
    latex_table <- xtable(med_ape_df, caption = "Prediction Evaluation", digits = 1)
    return(list(data_frame = med_ape_df, 
                latex_table = latex_table))
  }
}





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
load(file.path(wd_res, "final_LINEXP_test_pred_2022-2024.RData"))
tuned_LINEXP_test_export <- tuned_LINEXP_test_export %>% mutate(sex = as.character(sex))
load(file.path(wd_res, "final_csp_test_pred_2022-2024.RData"))
csp_text_export <- csp_text_export %>% mutate(sex = as.character(sex))
load(file.path(wd_res, "final_vsg_test_pred_2022-2024.RData"))
vsg_test_for_export <- vsg_test_for_export %>% mutate(sex = as.character(sex))
load(file.path(wd_res, "final_csp-vsg_test_2022-2024.RData"))
csp_vsg_test <- csp_vsg_test %>% mutate(sex = as.character(sex))


tft_test <- read.csv2(file.path(wd_res, "tft_test_2022-2024.csv"), sep = ",") %>%
  mutate(prediction = as.numeric(prediction)) %>%
  filter(quantile == "0.5") %>%
  select(-quantile) %>%
  separate(
    original_index,
    into = c("municipality_code", "sex", "age_group"),
    sep = "_"
  ) %>%
  mutate(sex = as.character(round(as.numeric(sex), 0))) %>%
  rename(PRED_tft = prediction) %>%
  mutate(population = NA)
  


# evaluate along size groups
evaluation_of_all_unbalanced <- evaluate_predictions(
  prediction_dfs  = list(tuned_LINEXP_test_export,
                         hp_prediction_export,
                         csp_text_export,
                         vsg_test_for_export,
                         csp_vsg_test,
                         tft_test),
  true_value_df   = all_pop_for_evaluation,
  true_pop_col    = population,
  pred_cols       = c("PRED_tuned_LINEXP",
                      "PRED_hamilton_perry",
                      "PRED_csp_final",
                      "PRED_vsg",
                      "PRED_csp_vsg",
                      "PRED_tft"),
  grouping_col    = population_size_group,
  prediction_years = 2022:2024,
  grouping_col_nice = "Population Size Group",
  model_names = c("LINEXP", "HP", "CSP", "VSG", "CSP-VSG", "TFT"),
  print_latex = TRUE)
)

evaluation_of_all_unbalanced[1] %>% write.csv(file.path(wd_res, "evaluations\\evaluation_unbalanced_size_group.csv"))


# evaluate along age groups
evaluation_of_all_unbalanced <- evaluate_predictions(
  prediction_dfs  = list(tuned_LINEXP_test_export,
                         hp_prediction_export,
                         csp_text_export,
                         vsg_test_for_export,
                         csp_vsg_test,
                         tft_test),
  true_value_df   = all_pop_for_evaluation,
  true_pop_col    = population,
  pred_cols       = c("PRED_tuned_LINEXP",
                      "PRED_hamilton_perry",
                      "PRED_csp_final",
                      "PRED_vsg",
                      "PRED_csp_vsg",
                      "PRED_tft"),
  grouping_col    = age_group,
  prediction_years = 2022:2024,
  grouping_col_nice = "Population Size Group",
  model_names = c("LINEXP", "HP", "CSP", "VSG", "CSP-VSG", "TFT"),
  print_latex = TRUE)
)

evaluation_of_all_unbalanced[1] %>% write.csv(file.path(wd_res, "evaluations\\evaluation_unbalanced_age_group.csv"))

evaluation_of_all_unbalanced[2]

# evaluate balanced preds ------------------------------------------------------
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

load(file.path(wd_res, "2022-2024_LINEXP_test_balanced.RData"))
balanced_LINEXP_pred <- balanced_LINEXP_pred %>%
  ungroup() %>%
  select(year, municipality_code, sex, age_group, balanced_pred) %>%
  rename(PRED_linexp = balanced_pred) %>%
  mutate(population = NA) %>%
  mutate(sex = as.character(sex))

load(file.path(wd_res, "2022-2024_HP_test_balanced.RData"))
balanced_hp_pred <- balanced_hp_pred %>%
  ungroup() %>%
  select(year, municipality_code, sex, age_group, balanced_pred) %>%
  rename(PRED_linexp = balanced_pred) %>%
  mutate(population = NA) %>%
  mutate(sex = as.character(sex))

load(file.path(wd_res, "2022-2024_VSG_balanced.RData"))
balanced_LINEXP_pred <- balanced_LINEXP_pred %>%
  ungroup() %>%
  select(year, municipality_code, sex, age_group, balanced_pred) %>%
  rename(PRED_linexp = balanced_pred) %>%
  mutate(population = NA) %>%
  mutate(sex = as.character(sex))

load(file.path(wd_res, "2022-2024_LINEXP_test_balanced.RData"))
balanced_LINEXP_pred <- balanced_LINEXP_pred %>%
  ungroup() %>%
  select(year, municipality_code, sex, age_group, balanced_pred) %>%
  rename(PRED_linexp = balanced_pred) %>%
  mutate(population = NA) %>%
  mutate(sex = as.character(sex))

load(file.path(wd_res, "2022-2024_LINEXP_test_balanced.RData"))
balanced_LINEXP_pred <- balanced_LINEXP_pred %>%
  ungroup() %>%
  select(year, municipality_code, sex, age_group, balanced_pred) %>%
  rename(PRED_linexp = balanced_pred) %>%
  mutate(population = NA) %>%
  mutate(sex = as.character(sex))

