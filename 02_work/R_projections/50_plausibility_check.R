############################################################################## #
# Filename
#    50_plausibility_check.R
#
# Description
#   Check for plausibility of forecasts
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      2025-02-25
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #

load(file.path(wd_data_work, "all_municipalities_population.RData"))
load(file.path(wd_res, "2025-2035_TFT_balanced.RData"))
load(file.path(wd_res, "2025-2035_CSP-VSG_balanced.RData"))
static_vars <- read.csv2(file.path(wd_data_orig, "static_variables.csv"),
                         encoding = "latin1")
static_vars <- static_vars %>%
  rename(municipality_code = ID) %>%
  mutate(municipality_code = as.character(municipality_code)) %>%
  mutate(simplified_urban_rural = (round(Urban.Rural.Typologie/100, 0))) 


# trend changes in urban/rural muns --------------------------------------------

check_trend_changes <- function(forecast_names,
                               past_population,
                               list_of_forecasts,
                               grouping_df){
  past_trends <- past_population %>% 
    filter(year %in% c(2014, 2024)) %>%
    group_by(municipality_code, year) %>%
    summarise(population = sum(population)) %>%
    pivot_wider(names_from = year, values_from = population) %>%
    mutate(past_trend = `2024`-`2014`) %>% 
    left_join(static_vars %>% select(municipality_code, simplified_urban_rural)) %>%
    select(municipality_code, past_trend, simplified_urban_rural)
  
  #list_of_forecasts <- list(balanced_csp_vsg_pred, balanced_tft_pred)
  
  list_future_trends <- list()
  result_df <- data.frame(simplified_urban_rural = unique(past_trends$simplified_urban_rural)) 
  
  for(forecast in list_of_forecasts){
    future_trends <- forecast %>% 
      filter(year %in% c(2025, 2034)) %>%
      group_by(municipality_code, year) %>%
      summarise(population = sum(balanced_pred)) %>%
      pivot_wider(names_from = year, values_from = population) %>%
      mutate(future_trend = `2034`-`2025`) %>% 
      select(municipality_code, future_trend)%>%
      left_join(past_trends, by = join_by(municipality_code)) %>%
      mutate(trend_change = case_when(sign(past_trend) != sign(future_trend) ~ 1,
                                      sign(past_trend) == sign(future_trend) ~ 0)) %>%
      group_by(simplified_urban_rural) %>%
      summarise(trend_change_share = sum(trend_change) / n()) 
    
    result_df <- result_df %>%
      left_join(future_trends,by = "simplified_urban_rural") 
  }
  names(result_df)[2:ncol(result_df)] <- forecast_names
  
  return(result_df)
}

check_trend_changes(forecast_names = c("CSP-VSG", "TFT"),
                   past_population = all_munip_pop,
                   list_of_forecasts = list(balanced_csp_vsg_pred, balanced_tft_pred),
                   grouping_df = static_vars) 



# share of each age group in total austrian pop --------------------------------
load(file.path(wd_data_work, "aut_forecast.RData"))



age_group_share <- aut_forecast %>%
  rename(age_group = coarse_age_group) %>%
  group_by(year) %>%
  mutate(total_pop = sum(population)) %>%
  mutate(age_group_share = population / total_pop) %>%
  select(year, age_group, age_group_share)


list_of_forecasts <- list(balanced_tft_pred)

for(forecast in list_of_forecasts){
  forecasts_age_group_shares <- forecast %>%
    group_by(year, age_group) %>%
    summarise(pop_per_age_group = sum(tft_prediction)) #%>%
    #group_by(year) %>%
    #mutate(total_pop = sum(pop_per_age_group))
}

forecasts_age_group_shares <- balanced_csp_vsg_pred %>%
  group_by(year, reg_code) %>%
  summarise(bal = sum(balanced_pred),
            unbal = sum(PRED_csp_vsg)) %>%
  group_by(year) %>%
  summarise(sum(pop_per_age_group))




