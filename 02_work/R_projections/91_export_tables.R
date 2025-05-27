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


# function to export results in wide format
export_wide_result <- function(data,
                               historic_pop,
                               forecast_pop,
                               output_name,
                               wd_res,
                               historic_years = 2002:2024,
                               forecast_years  = 2025:2035) {
  
  
  data_out <- data %>%
    rename(forecast = {{forecast_pop}}) %>%
    ungroup() %>%
    select(-{{historic_pop}}) %>%
    #mutate(
    #  !!forecast_q := as.numeric(!!forecast_q)
    #) %>%
    full_join(select(all_munip_pop, municipality_code, sex, coarse_age_group, year,population) %>% ungroup(),
              by = join_by(municipality_code, sex, coarse_age_group, year)) %>%
    mutate(
      population = case_when(
        year %in% historic_years ~ population,
        year %in% forecast_years ~ forecast,
        TRUE                   ~ NA_real_
      )
    ) %>%
    arrange(year) %>%
    # drop the original columns you just used
    select(-forecast) %>%
    pivot_wider(
      names_from  = year,
      values_from = population
    )
      # write out
      write.csv2(data_out, file = file.path(wd_res, output_name), row.names = FALSE)
    }



# test  data ---------------------------------------------------------
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


#
load(file.path(wd_data_work, "all_municipalities_population.RData"))

load(file.path(wd_res, "25-35_CSG_prediction.RData"))
csg_pred_export <- csg_pred_export %>% rename(coarse_age_group = age_group)
export_wide_result(csg_pred_export, "population", "PRED_csg_final", output_name = "2025-2035_CSG_prediction.csv", wd_res = wd_res)

load(file.path(wd_res, "25-35_CSP_prediction.RData"))
csp_pred_export <- csp_pred_export %>% rename(coarse_age_group = age_group)
export_wide_result(csp_pred_export, "population", "PRED_csp_final", output_name = "2025-2035_CSP_prediction.csv", wd_res = wd_res)

load(file.path(wd_res, "25-35_VSG_prediction.RData"))
vsg_pred_for_export <- vsg_pred_for_export %>% rename(coarse_age_group = age_group)
export_wide_result(vsg_pred_for_export, "population", "PRED_vsg", output_name = "2025-2035_VSG_prediction.csv", wd_res = wd_res)

load(file.path(wd_res, "25-35_LINEXP_prediction.RData"))
tuned_LINEXP_pred_export <- tuned_LINEXP_pred_export %>% rename(coarse_age_group = age_group) %>% mutate(sex = as.numeric(sex))
export_wide_result(tuned_LINEXP_pred_export, "population", "PRED_tuned_LINEXP", output_name = "2025-2035_LINEXP_prediction.csv", wd_res = wd_res)

load(file.path(wd_res, "25-35_HP_prediction.RData"))
hp_pred_export %>% rename(coarse_age_group = age_group) %>% mutate(sex = as.numeric(sex)) %>%
  export_wide_result(., "population", "PRED_hamilton_perry", output_name = "2025-2035_HP_prediction.csv", wd_res = wd_res)

load(file.path(wd_res, "25-35_CSP-VSG_prediction.RData"))
csp_vsg_pred %>% rename(coarse_age_group = age_group) %>% mutate(sex = as.numeric(sex)) %>%
  export_wide_result(., "population", "PRED_csp_vsg", output_name = "2025-2035_CSP-VSG_prediction.csv", wd_res = wd_res)


tft_pred <- read.csv2(file.path(wd_res, "tft_prediction_2025-2035.csv"), sep = ",") %>%
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



# export balanced --------------------------------------------------------------
load(file.path(wd_res, "2025-2035_LINEXP_balanced.RData"))
balanced_LINEXP_pred %>% rename(coarse_age_group = age_group) %>% mutate(sex = as.numeric(sex)) %>% ungroup() %>% select(-reg_code) %>%
  export_wide_result(., "PRED_tuned_LINEXP", "balanced_pred", output_name = "2025-2035_LINEXP_balanced.csv", wd_res = wd_res)


load(file.path(wd_res, "2025-2035_HP_balanced.RData"))
balanced_hp_pred %>% rename(coarse_age_group = age_group) %>% mutate(sex = as.numeric(sex)) %>% ungroup() %>% select(-reg_code) %>%
  export_wide_result(., "PRED_hamilton_perry", "balanced_pred", output_name = "2025-2035_HP_balanced.csv", wd_res = wd_res)


load(file.path(wd_res, "2025-2035_CSP_balanced.RData"))
balanced_csp_pred %>% rename(coarse_age_group = age_group) %>% mutate(sex = as.numeric(sex)) %>% ungroup() %>% select(-reg_code) %>%
  export_wide_result(., "PRED_csp_final", "balanced_pred", output_name = "2025-2035_CSP_balanced.csv", wd_res = wd_res)


load(file.path(wd_res, "2025-2035_VSG_balanced.RData"))
balanced_vsg_pred %>% rename(coarse_age_group = age_group) %>% mutate(sex = as.numeric(sex)) %>% ungroup() %>% select(-reg_code) %>%
  export_wide_result(., "PRED_vsg", "balanced_pred", output_name = "2025-2035_VSG_balanced.csv", wd_res = wd_res)


load(file.path(wd_res, "2025-2035_CSP-VSG_balanced.RData"))
balanced_csp_vsg_pred %>% rename(coarse_age_group = age_group) %>% mutate(sex = as.numeric(sex)) %>% ungroup() %>% select(-reg_code) %>%
  export_wide_result(., "PRED_csp_vsg", "balanced_pred", output_name = "2025-2035_CSP-VSG_balanced.csv", wd_res = wd_res)



