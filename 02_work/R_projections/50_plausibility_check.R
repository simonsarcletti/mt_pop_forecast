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
load(file.path(wd_data_work, "munip_size_group_mapping_2024.RData"))
municipality_size_group_mapping_2024 <- municipality_size_group_mapping_2024 %>%
  mutate(population_size_group = factor(population_size_group, 
                                        levels = c("< 500", "500-1000", "1000-2000", "2000-5000", "5000-20000", "20000-50000","> 50000"),
                                        ordered = TRUE))


load(file.path(wd_data_work, "all_municipalities_population.RData"))
load(file.path(wd_res, "2025-2035_TFT_balanced.RData"))
load(file.path(wd_res, "2025-2035_CSP-VSG_balanced.RData"))
load(file.path(wd_res, "2025-2035_CSP_balanced.RData"))
load(file.path(wd_res, "2025-2035_LINEXP_balanced.RData"))
load(file.path(wd_res, "2025-2035_HP_balanced.RData"))
load(file.path(wd_res, "2025-2035_VSG_balanced.RData"))
static_vars <- read.csv2(file.path(wd_data_orig, "static_variables.csv"), encoding = "latin1")
static_vars <- static_vars %>%
  rename(municipality_code = ID) %>%
  mutate(municipality_code = as.character(municipality_code)) %>%
  mutate(simplified_urban_rural = (round(urban.rural.typologie / 100, 0)))


# trend changes in urban/rural muns --------------------------------------------

check_extreme_trend_changes <- function(forecast_names,
                                past_population,
                                list_of_forecasts,
                                grouping_df) {
  past_trends <- past_population %>%
    filter(year %in% c(2014, 2024)) %>%
    group_by(municipality_code, year) %>%
    summarise(population = sum(population)) %>%
    pivot_wider(names_from = year, values_from = population) %>%
    mutate(past_trend = `2024` - `2014`) %>%
    left_join(grouping_df %>% select(municipality_code, population_size_group)) %>%
    select(municipality_code, past_trend, population_size_group) %>%
    group_by(population_size_group) %>%
    mutate(max_change = quantile(past_trend, 0.85),
           min_change = quantile(past_trend, 0.15)) %>%
    mutate(extreme_neg_trend = case_when(past_trend < 0 & (past_trend < min_change) ~ 1,
                                         .default = 0)) %>%
    mutate(extreme_pos_trend = case_when(past_trend > 0 & (past_trend > max_change) ~ 1,
                                         .default = 0))
  
  
  list_future_trends <- list()
  result_df <- data.frame(population_size_group = unique(past_trends$population_size_group))
  
  for (forecast in list_of_forecasts) {
    future_trends <- forecast %>%
      filter(year %in% c(2025, 2034)) %>%
      group_by(municipality_code, year) %>%
      summarise(population = sum(balanced_pred)) %>%
      pivot_wider(names_from = year, values_from = population) %>%
      mutate(future_trend = `2034` - `2025`) %>%
      left_join(grouping_df %>% select(municipality_code, population_size_group)) %>%
      select(municipality_code, future_trend, population_size_group) %>%
      group_by(population_size_group) %>%
      mutate(max_change = quantile(future_trend, 0.85),
             min_change = quantile(future_trend, 0.15)) %>%
      mutate(extreme_neg_future_trend = case_when(future_trend < 0 & (future_trend < min_change*0.3) ~ 1,
                                                  .default = 0)) %>%
      mutate(extreme_pos_future_trend = case_when(future_trend > 0 & (future_trend > max_change*0.3) ~ 1,
                                                  .default = 0)) %>%
      ungroup() %>%
      select(municipality_code, population_size_group, extreme_neg_future_trend, extreme_pos_future_trend) %>%
      left_join(select(ungroup(past_trends), municipality_code, extreme_neg_trend, extreme_pos_trend),
                by = join_by(municipality_code)) %>%
      mutate(extreme_trend_change = case_when(
        extreme_neg_trend == 1 & extreme_pos_future_trend == 1 ~ 1,
        extreme_pos_trend == 1 & extreme_neg_future_trend == 1 ~ 1,
        .default = 0
      )) %>%
      group_by(population_size_group) %>%
      summarise(extreme_trend_change = sum(extreme_trend_change) / n()*100)
    
    result_df <- result_df %>%
      left_join(future_trends, by = "population_size_group")
  }
  names(result_df)[2:ncol(result_df)] <- forecast_names
  
  return(result_df)
}



trend_change_df <- check_extreme_trend_changes(
  forecast_names = c("CSP-VSG", "TFT", "HP", "LINEXP", "VSG", "CSP"),
  past_population = all_munip_pop,
  list_of_forecasts = list(
    balanced_csp_vsg_pred,
    balanced_tft_pred,
    balanced_hp_pred,
    balanced_LINEXP_pred,
    balanced_vsg_pred,
    balanced_csp_pred
  ),
  grouping_df = municipality_size_group_mapping_2024
) 


trend_change_df <- check_trend_changes(
  forecast_names = c("CSP-VSG", "TFT", "HP", "LINEXP", "VSG", "CSP"),
  past_population = all_munip_pop,
  list_of_forecasts = list(
    balanced_csp_vsg_pred,
    balanced_tft_pred,
    balanced_hp_pred,
    balanced_LINEXP_pred,
    balanced_vsg_pred,
    balanced_csp_pred
  ),
  grouping_df = municipality_size_group_mapping_2024
) 



# share of each age group in total austrian pop --------------------------------
load(file.path(wd_data_work, "aut_forecast.RData"))


check_composition_by_age <- function(forecast_names = c("CSP-VSG", "TFT"),
                                     past_population,
                                     list_of_forecasts,
                                     relevant_year) {
  age_group_share_aut <- past_population %>%
    rename(age_group = coarse_age_group) %>%
    group_by(year) %>%
    mutate(total_pop = sum(population)) %>%
    mutate(age_group_share = population / total_pop) %>%
    select(year, age_group, age_group_share)
  
  
  results <- data.frame(age_group = unique(age_group_share_aut$age_group))
  
  for (forecast in list_of_forecasts) {
    forecasts_age_group_shares <- forecast %>%
      group_by(year, age_group, municipality_code) %>%
      summarise(pop_per_age_group = sum(balanced_pred)) %>%
      group_by(year, municipality_code) %>%
      mutate(total_pop = sum(pop_per_age_group)) %>%
      mutate(age_group_share = pop_per_age_group / total_pop) %>%
      group_by(year, age_group) %>%
      summarise(age_group_share = mean(age_group_share)) %>%
      select(year, age_group, age_group_share) %>%
      left_join(age_group_share_aut, by = join_by(year, age_group)) %>%
      mutate(difference = round((
        age_group_share.x - age_group_share.y
      ) * 100, 2)) %>%
      filter(year == relevant_year) %>%
      ungroup() %>%
      select(age_group, difference)
    results <- results %>%
      left_join(forecasts_age_group_shares, by = "age_group")
  }
  names(results)[2:ncol(results)] <- forecast_names
  
  return(results)
}

age_composition_df <- check_composition_by_age(
  forecast_names = c("CSP-VSG", "TFT", "HP", "LINEXP", "VSG", "CSP"),
  past_population = aut_forecast,
  list_of_forecasts = list(
    balanced_csp_vsg_pred,
    balanced_tft_pred,
    balanced_hp_pred,
    balanced_LINEXP_pred,
    balanced_vsg_pred,
    balanced_csp_pred
  ),
  relevant_year = 2035
)




# extreme growth/decline
check_extreme_change <- function(forecast_names,
                                 past_population,
                                 list_of_forecasts,
                                 grouping_df) {
  past_trends <- past_population %>%
    filter(year %in% c(2014, 2024)) %>%
    group_by(municipality_code, year) %>%
    summarise(population = sum(population)) %>%
    pivot_wider(names_from = year, values_from = population) %>%
    mutate(past_trend = (`2024` - `2014`) / `2014`) %>%
    left_join(grouping_df %>% select(municipality_code, population_size_group)) %>%
    select(municipality_code, past_trend, population_size_group) %>%
    group_by(population_size_group) %>%
    summarise(max_change = max(past_trend),
              min_change = min(past_trend))
  
  
  max_change1 <- past_trends %>% filter(population_size_group == "< 500") %>% pull(max_change)
  max_change2 <- past_trends %>% filter(population_size_group == "500-1000") %>% pull(max_change)
  max_change3 <- past_trends %>% filter(population_size_group == "1000-2000") %>% pull(max_change)
  max_change4 <- past_trends %>% filter(population_size_group == "2000-5000") %>% pull(max_change)
  max_change6 <- past_trends %>% filter(population_size_group == "5000-20000") %>% pull(max_change)
  max_change7 <- past_trends %>% filter(population_size_group == "20000-50000") %>% pull(max_change)
  max_change5 <- past_trends %>% filter(population_size_group == "> 50000") %>% pull(max_change)
  min_change1 <- past_trends %>% filter(population_size_group == "< 500") %>% pull(min_change)
  min_change2 <- past_trends %>% filter(population_size_group == "500-1000") %>% pull(min_change)
  min_change3 <- past_trends %>% filter(population_size_group == "1000-2000") %>% pull(min_change)
  min_change4 <- past_trends %>% filter(population_size_group == "2000-5000") %>% pull(min_change)
  min_change6 <- past_trends %>% filter(population_size_group == "5000-20000") %>% pull(min_change)
  min_change7 <- past_trends %>% filter(population_size_group == "20000-50000") %>% pull(min_change)
  min_change5 <- past_trends %>% filter(population_size_group == "> 50000") %>% pull(min_change)
  
  list_future_trends <- list()
  result_df <- data.frame(direction_of_change = c(rep("postive", 7), rep("negative", 7)),
                          population_size_group = rep(unique(past_trends$population_size_group), 2))
  
  for (i in seq_along(list_of_forecasts)) {
    forecast_df <- list_of_forecasts[[i]]
    f_name     <- forecast_names[i]
    
    future_trends <- forecast_df %>%
      filter(year %in% c(2025, 2034)) %>%
      group_by(municipality_code, year) %>%
      summarise(population = sum(balanced_pred),
                .groups = "drop") %>%
      pivot_wider(names_from = year, values_from = population) %>%
      mutate(future_trend = (`2034` - `2025`) / `2025`) %>%
      select(municipality_code, future_trend) %>%
      left_join(grouping_df %>% select(municipality_code, population_size_group),
                by = "municipality_code")
    
    # Now, count how many “future_trend” exceed the historical max_change for each category
    extreme_count <- c(
      # positive “extreme” growth (one row per size‐group)
      future_trends %>% 
        filter(population_size_group == "< 500", 
               future_trend >  0, 
               future_trend >  max_change1 * 0.8) %>% 
        nrow(),
      future_trends %>% 
        filter(population_size_group == "500-1000", 
               future_trend >  0, 
               future_trend >  max_change2 * 0.8) %>% 
        nrow(),
      future_trends %>% 
        filter(population_size_group == "1000-2000", 
               future_trend >  0, 
               future_trend >  max_change3 * 0.8) %>% 
        nrow(),
      future_trends %>% 
        filter(population_size_group == "2000-5000", 
               future_trend >  0, 
               future_trend >  max_change4 * 0.8) %>% 
        nrow(),
      future_trends %>% 
        filter(population_size_group == "5000-20000", 
               future_trend >  0, 
               future_trend >  max_change6 * 0.8) %>% 
        nrow(),
      future_trends %>% 
        filter(population_size_group == "20000-50000", 
               future_trend >  0, 
               future_trend >  max_change7 * 0.8) %>% 
        nrow(),
      future_trends %>% 
        filter(population_size_group == "> 50000", 
               future_trend >  0, 
               future_trend >  max_change5 * 0.8) %>% 
        nrow(),
      
      # negative “extreme” decline (one row per size‐group)
      future_trends %>% 
        filter(population_size_group == "< 500", 
               future_trend <  0, 
               future_trend <  min_change1 * 0.8) %>% 
        nrow(),
      future_trends %>% 
        filter(population_size_group == "500-1000", 
               future_trend <  0, 
               future_trend <  min_change2 * 0.8) %>% 
        nrow(),
      future_trends %>% 
        filter(population_size_group == "1000-2000", 
               future_trend <  0, 
               future_trend <  min_change3 * 0.8) %>% 
        nrow(),
      future_trends %>% 
        filter(population_size_group == "2000-5000", 
               future_trend <  0, 
               future_trend <  min_change4 * 0.8) %>% 
        nrow(),
      future_trends %>% 
        filter(population_size_group == "5000-20000", 
               future_trend <  0, 
               future_trend <  min_change6 * 0.8) %>% 
        nrow(),
      future_trends %>% 
        filter(population_size_group == "20000-50000", 
               future_trend <  0, 
               future_trend <  min_change7 * 0.8) %>% 
        nrow(),
      future_trends %>% 
        filter(population_size_group == "> 50000", 
               future_trend <  0, 
               future_trend <  min_change5 * 0.8) %>% 
        nrow()
    )
    
    # Attach that vector as a new column named after f_name:
    result_df[[f_name]] <- extreme_count
  }
  
  return(result_df)
}


extreme_change_df <- check_extreme_change(
  forecast_names = c("CSP-VSG", "TFT", "HP", "LINEXP", "VSG", "CSP"),
  past_population = all_munip_pop,
  list_of_forecasts = list(
    balanced_csp_vsg_pred,
    balanced_tft_pred,
    balanced_hp_pred,
    balanced_LINEXP_pred,
    balanced_vsg_pred,
    balanced_csp_pred
  ),
  grouping_df = municipality_size_group_mapping_2024
)


# deviation from expected cohort count -----------------------------------------
# 10 years later, the entire cohort should have moved on: 2034, all in 0 - 9 in 2024 should be 10-19
check_deviation_from_moved_on <- function(forecast_names,
                                          past_population,
                                          list_of_forecasts) {
  expected_pop <- past_population %>%
    filter(year == 2024) %>%
    group_by(coarse_age_group, municipality_code) %>%
    reframe(population = sum(population)) %>%
    filter(!coarse_age_group %in% c("30 - 44", "75+")) %>%
    mutate(age_group_ten_years = case_when(coarse_age_group == "0 - 9" ~ "10 - 19",
                                           coarse_age_group == "10 - 19" ~ "20 - 29",
                                           coarse_age_group == "30 - 44" ~ NA,
                                           coarse_age_group == "45 - 54" ~ "55 - 64",
                                           coarse_age_group == "55 - 64" ~ "65 - 74",
                                           coarse_age_group == "65 - 74" ~ "75+")) %>%
    #mutate(age_group_ten_years = rep(c("10 - 19", "20 - 29", NA, "55 - 64", "65 - 74", "75+"), 2115)) %>%
    rename(age_group = coarse_age_group) %>%
    filter(!is.na(age_group_ten_years)) %>%
    select(-age_group)
  
  
  result_df <- data.frame(age_group = unique(expected_pop$age_group_ten_years))
  
  
  for (i in seq_along(list_of_forecasts)) {
    forecast_df <- list_of_forecasts[[i]]
    f_name     <- forecast_names[i]
    
    future_pop <- forecast_df %>%
      filter(year == 2034) %>%
      group_by(year, age_group, municipality_code) %>%
      summarise(population = sum(balanced_pred),
                .groups = "drop") %>%
      left_join(expected_pop, by = join_by(age_group == age_group_ten_years, municipality_code)) %>%
      select(-year) %>%
      filter(!is.na(population.y)) %>%
      mutate(percentage_deviation = (population.x - population.y) / population.y) %>%
      group_by(age_group) %>%
      summarise(!!f_name := mean(percentage_deviation))

    
    result_df <- result_df %>%
      left_join(future_pop %>% select(age_group, f_name))

  }
  
  names(result_df)[2:ncol(result_df)] <- forecast_names
  return(result_df)
  
}


deviation_moved_on_df <- check_deviation_from_moved_on(
  forecast_names =  c("CSP-VSG", "TFT", "HP", "LINEXP", "VSG", "CSP"),
  past_population = all_munip_pop,
  list_of_forecasts = list(
    balanced_csp_vsg_pred,
    balanced_tft_pred,
    balanced_hp_pred,
    balanced_LINEXP_pred,
    balanced_vsg_pred,
    balanced_csp_pred
  )
)


trend_change_df %>%
  rename(class = population_size_group) %>%
  rbind(age_composition_df %>% rename(class = age_group)) %>%
  rbind(extreme_change_df %>% unite("class", direction_of_change:population_size_group, sep ="_")) %>%
  rbind(deviation_moved_on_df %>% rename(class = age_group)) %>%
  write.csv2(file = file.path(wd_res, "plausibility_check.csv"))

