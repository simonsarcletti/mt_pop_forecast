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


check_composition_by_age <- function(forecast_names = c("CSP-VSG", "TFT"),
                                     past_population,
                                     list_of_forecasts,
                                     relevant_year){
  
  age_group_share <- past_population %>%
    rename(age_group = coarse_age_group) %>%
    group_by(year) %>%
    mutate(total_pop = sum(population)) %>%
    mutate(age_group_share = population / total_pop) %>%
    select(year, age_group, age_group_share)
  

  results <- data.frame(age_group = unique(age_group_share$age_group))

  for(forecast in list_of_forecasts){
    forecasts_age_group_shares <- forecast %>%
      group_by(year, age_group) %>%
      summarise(pop_per_age_group = sum(balanced_pred)) %>%
      group_by(year) %>%
      mutate(total_pop = sum(pop_per_age_group)) %>%
      mutate(age_group_share = pop_per_age_group / total_pop) %>%
      select(year, age_group, age_group_share) %>%
      left_join(age_group_share, by = join_by(year, age_group)) %>%
      mutate(difference = round((age_group_share.x - age_group_share.y)*100, 2)) %>%
      filter(year == relevant_year) %>%
      ungroup() %>%
      select(age_group, difference)
    print(forecasts_age_group_shares)
    results <- results %>%
      left_join(forecasts_age_group_shares, by = "age_group")
  }
  names(results)[2:ncol(results)] <- forecast_names
  
  return(results)
}


check_composition_by_age(forecast_names = c("CSP-VSG"),
                    past_population = aut_forecast,
                    list_of_forecasts = list(balanced_csp_vsg_pred),
                    relevant_year = 2035)


# extreme growth/decline
check_extreme_change <- function(forecast_names,
                                past_population,
                                list_of_forecasts,
                                grouping_df){
  past_trends <- past_population %>% 
    filter(year %in% c(2014, 2024)) %>%
    group_by(municipality_code, year) %>%
    summarise(population = sum(population)) %>%
    pivot_wider(names_from = year, values_from = population) %>%
    mutate(past_trend = abs((`2024`-`2014`)/`2014`)) %>% 
    left_join(grouping_df %>% select(municipality_code, simplified_urban_rural)) %>%
    select(municipality_code, past_trend, simplified_urban_rural) %>%
    group_by(simplified_urban_rural) %>%
    summarise(max_change = max(past_trend))
    
  
  max_change1 <- past_trends %>% filter(simplified_urban_rural ==1) %>% pull(max_change)
  max_change2 <- past_trends %>% filter(simplified_urban_rural ==2) %>% pull(max_change)
  max_change3 <- past_trends %>% filter(simplified_urban_rural ==3) %>% pull(max_change)
  max_change4 <- past_trends %>% filter(simplified_urban_rural ==4) %>% pull(max_change)
  
  
  list_future_trends <- list()
  result_df <- data.frame(simplified_urban_rural = unique(past_trends$simplified_urban_rural)) 
  
  for (i in seq_along(list_of_forecasts)) {
    forecast_df <- list_of_forecasts[[i]]
    f_name     <- forecast_names[i]
    
    future_trends <- forecast_df %>%
      filter(year %in% c(2025, 2034)) %>%
      group_by(municipality_code, year) %>%
      summarise(population = sum(balanced_pred), .groups = "drop") %>%
      pivot_wider(names_from = year, values_from = population) %>%
      mutate(
        future_trend = abs((`2034` - `2025`) / `2025`)
      ) %>%
      select(municipality_code, future_trend) %>%
      left_join(
        grouping_df %>% select(municipality_code, simplified_urban_rural),
        by = "municipality_code"
      )
    
    # Now, count how many “future_trend” exceed the historical max_change for each category
    extreme_count <- c(
      future_trends %>% filter(simplified_urban_rural == 1, future_trend > max_change1) %>% nrow(),
      future_trends %>% filter(simplified_urban_rural == 2, future_trend > max_change2) %>% nrow(),
      future_trends %>% filter(simplified_urban_rural == 3, future_trend > max_change3) %>% nrow(),
      future_trends %>% filter(simplified_urban_rural == 4, future_trend > max_change4) %>% nrow()
    )
    
    # Attach that vector as a new column named after f_name:
    result_df[[ f_name ]] <- extreme_count
  }
  
  return(result_df)
}


check_extreme_change(forecast_names = c("CSP-VSG"),
                    past_population = all_munip_pop,
                    list_of_forecasts = list(balanced_csp_vsg_pred),
                    grouping_df = static_vars) 


# deviation from expected cohort count -----------------------------------------
# 10 years later, the entire cohort should have moved on: 2034, all in 0 - 9 in 2024 should be 10-19
check_deviation_from_moved_on <- function(forecast_names,
                                          past_population,
                                          list_of_forecasts){
  
  expected_pop <- past_population %>%
    filter(year == 2024) %>%
    group_by(coarse_age_group) %>%
    reframe(population = sum(population)) %>%
    filter(!coarse_age_group %in% c("30 - 44", "75+")) %>%
    mutate(age_group_ten_years = c("10 - 19", "20 - 29", NA, "55 - 64", "65 - 74", "75+")) %>%
    rename(age_group = coarse_age_group) %>%
    filter(!is.na(age_group_ten_years)) %>%
    select(-age_group)
  
  
  result_df <- data.frame(age_group = unique(expected_pop$age_group_ten_years))

  
  for (i in seq_along(list_of_forecasts)) {
    forecast_df <- list_of_forecasts[[i]]
    f_name     <- forecast_names[i]
    
    future_pop <- forecast_df %>%
      filter(year == 2034) %>%
      group_by(year, age_group) %>%
      summarise(population = sum(balanced_pred), .groups = "drop") %>%
      left_join(expected_pop, by = join_by(age_group == age_group_ten_years)) %>%
      select(-year) %>%
      filter(!is.na(population.y)) %>%
      mutate(percentage_deviation = (population.x - population.y)/population.y)
    
    result_df <- result_df %>%
      left_join(future_pop %>% select(age_group, percentage_deviation))
  }
  
  names(results)[2:ncol(results)] <- forecast_names
  return(result_df)
  
}


check_deviation_from_moved_on(forecast_names = c("CSP-VSG"),
                              past_population = all_munip_pop,
                              list_of_forecasts = list(balanced_csp_vsg_pred))




