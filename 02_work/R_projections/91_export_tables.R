############################################################################## #
# Filename
#    91_export_tables.R
#
# Description
#   Compose csv files for ÖROK
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      20xx-xx-xx
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #




# test prediction data ---------------------------------------------------------
# hamilton perry
load(file.path(wd_res, "final_HP_balanced.RData"))
load(file.path(wd_res, "final_HP_prediction.RData"))

balanced_hp_pred <- balanced_hp_pred %>%
  ungroup()

hp_table <- hp_prediction_export %>% 
  left_join(select(balanced_hp_pred,municipality_code, sex, age_group, year, balanced_pred),
            join_by(municipality_code, sex, age_group, year)) %>%
  mutate(
    PRED_hamilton_perry = if_else(
      year >= 2002 & year <= 2021,
      population,
      PRED_hamilton_perry
    )
  ) %>%
  mutate(
    balanced_pred = if_else(
      year >= 2002 & year <= 2021,
      population,
      balanced_pred
    )
  ) %>%
  rename(true_population = population,
         predicted_population = PRED_hamilton_perry,
         balanced_predicted_population = balanced_pred) %>%
  select(-c(true_population, predicted_population)) %>%
  pivot_wider(
              names_from = year,
              values_from = balanced_predicted_population) %>%
  write.csv2(file.path(wd_res, "hp_balanced_wide.csv"))

write.csv(hp_table,
          file = file.path(wd_res, "HP_test_prediction_2021-2024.csv"))
rm(balanced_hp_pred)
rm(hp_prediction_export)
rm(hp_table)


# LINEXP
load(file.path(wd_res, "final_LINEXP_balanced.RData"))
load(file.path(wd_res, "final_LINEXP_prediction.RData"))

balanced_LINEXP_pred <- balanced_LINEXP_pred %>%
  ungroup()

LINEXP_table <- tuned_LINEXP_pred_export %>% 
  mutate(sex = as.numeric(sex)) %>%
  left_join(select(balanced_LINEXP_pred ,municipality_code, sex, age_group, year, balanced_pred),
            join_by(municipality_code, sex, age_group, year)) %>%
  mutate(
    PRED_tuned_LINEXP = if_else(
      year >= 2002 & year <= 2021,
      population,
      PRED_tuned_LINEXP
    )
  ) %>%
  mutate(
    balanced_pred = if_else(
      year >= 2002 & year <= 2021,
      population,
      balanced_pred
    )
  ) %>%
  rename(true_population = population,
         predicted_population = PRED_tuned_LINEXP,
         balanced_predicted_population = balanced_pred) %>%
  select(-c(true_population, predicted_population)) %>%
  pivot_wider(
    names_from = year,
    values_from = balanced_predicted_population) %>%
  write.csv2(file.path(wd_res, "LINEXP_balanced_wide.csv"))

write.csv(LINEXP_table,
          file = file.path(wd_res, "LINEXP_test_prediction_2021-2024.csv"))
rm(balanced_LINEXP_pred)
rm(tuned_LINEXP_pred_export)
rm(LINEXP_table)


# actual prediction ------------------------------------------------------------
load(file.path(wd_res, "25-35_LINEXP_prediction.RData"))
load(file.path(wd_res, "25-35_HP_prediction.RData"))

prediction_table_hp <- hp_pred_export %>%
  mutate(population = case_when(year %in% 2002:2024 ~ population,
                                year %in% 2025:2035 ~ PRED_hamilton_perry)) %>%
  select(-PRED_hamilton_perry) %>%
  pivot_wider(names_from = year,
              values_from = population)

write.csv(prediction_table_hp,
          file = file.path(wd_res, "HP_prediction_2025-2035.csv"))


prediction_table_LINEXP <- tuned_LINEXP_pred_export %>%
  mutate(population = case_when(year %in% 2002:2024 ~ population,
                                year %in% 2025:2035 ~ PRED_tuned_LINEXP)) %>%
  select(-PRED_tuned_LINEXP)  %>%
  pivot_wider(names_from = year,
              values_from = population)

write.csv(prediction_table_hp,
          file = file.path(wd_res, "LINEXP_prediction_2025-2035.csv"))


