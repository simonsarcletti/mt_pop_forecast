############################################################################## #
# Filename
#    01_EDA.R
#
# Description
#   Explanatory Data Analysis
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      2025-02-25
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #

# overview over population distribution ----------------------------------------
load(file.path(wd_data_work, "all_municipalities_population.RData"))
View(head(all_munip_pop, 100))

summarized_munip_pop <- all_munip_pop %>%
  select(-reg_code) %>%
  group_by(municipality, municipality_code, year) %>%
  summarise(population = sum(population, na.rm = T)) %>%
  filter(year %in% c(2014,2024))

View(summarized_munip_pop)


ggplot(filter(summarized_munip_pop, year == 2024), aes(x = population)) +
  geom_histogram(
    fill = "cornflowerblue",
    color = "black",
    bins = 30
  ) +
  scale_x_continuous(breaks = c(1000, 100000, 200000, 300000),
                     labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "", x = "Population", y = "Count (log-scale)") +
  theme_minimal()


average_growth_rate_df <- summarized_munip_pop %>%
  pivot_wider(id_cols = municipality_code,
              names_from = year,
              values_from = population) %>%
  group_by(municipality_code) %>%
  mutate(average_growth_rate = (`2024`/`2014`)^(1/10)-1)

quantile(average_growth_rate_df$average_growth_rate)


hist(average_growth_rate_df$average_growth_rate)


summary_df <- data.frame(
  Variable = c(
    "N",
    "Median population",
    "Mean population",
    "Standard Deviation",
    "N by 2024 population",
    "0-1,000",
    "1,001-2,000",
    "2,001-5,000",
    "5,001-10,000",
    "10,001-100,000",
    "> 100,000",
    "N by average growth rate 2014-2024",
    "< -1%",
    "-1% - 0%",
    "0% - 1%",
    "1%-2%",
    ">2%"
  ),
  Value = c(
    nrow(filter(summarized_munip_pop, year == 2024)),  # Total number of municipalities
    median(summarized_munip_pop$population, na.rm = TRUE),
    mean(summarized_munip_pop$population, na.rm = TRUE),
    sd(summarized_munip_pop$population, na.rm = TRUE),
    NA,
    nrow(filter(summarized_munip_pop, population <= 1000, year ==2024)),
    nrow(filter(summarized_munip_pop, population > 1000 & population <= 2000, year ==2024)),
    nrow(filter(summarized_munip_pop, population > 2000 & population <= 5000, year ==2024)),
    nrow(filter(summarized_munip_pop, population > 5000 & population <= 10000, year ==2024)),
    nrow(filter(summarized_munip_pop, population > 10000 & population <= 100000, year ==2024)),
    nrow(filter(summarized_munip_pop, population > 100000, year ==2024)),
    NA,
    nrow(filter(average_growth_rate_df, average_growth_rate < -0.01)),
    nrow(filter(average_growth_rate_df, average_growth_rate >= -0.01 & average_growth_rate < 0)),
    nrow(filter(average_growth_rate_df, average_growth_rate >= 0 & average_growth_rate < 0.01)),
    nrow(filter(average_growth_rate_df, average_growth_rate >= 0.01 & average_growth_rate <= 0.02)),
    nrow(filter(average_growth_rate_df, average_growth_rate > 0.02))
  )
)

xtable(summary_df)



# cohort size distribution
load(file.path(wd_data_work, "all_municipalities_population.RData"))
View(head(all_munip_pop, 100))

cohort_size_dist <- all_munip_pop %>%
  filter(year == 2024) %>%
  group_by(sex, age_group) %>%
  mutate(coarse_age_groups = case_when(
    age_group %in% c("bis 4 Jahre", "5 bis 9 Jahre") ~ "bis 9 Jahre",
    age_group %in% c("10 bis 14 Jahre", "15 bis 19 Jahre") ~ "10 bis 19 Jahre",
    age_group %in% c("20 bis 24 Jahre", "25 bis 29 Jahre") ~ "20 bis 29 Jahre",
    age_group %in% c("30 bis 34 Jahre", "35 bis 39 Jahre", "40 bis 44 Jahre") ~ "30 bis 44 Jahre",
    age_group %in% c("45 bis 49 Jahre", "50 bis 54 Jahre") ~ "45 bis 54 Jahre",
    age_group %in% c("55 bis 59 Jahre", "60 bis 64 Jahre") ~ "55 bis 64 Jahre",
    age_group %in% c("65 bis 69 Jahre", "70 bis 74 Jahre") ~ "65 bis 74 Jahre",
    age_group %in% c("75 bis 79 Jahre", "80+") ~ "75+"
  )) %>%
  unite(index, sex:age_group, remove = FALSE) 

coarse_cohort_size_dist <- cohort_size_dist %>%
  unite(index2, c(sex, coarse_age_groups)) %>%
  group_by(municipality, index2) %>%
  summarise(population = sum(population, na.rm = TRUE))



set.seed(123)
random_indices <- sample(unique(cohort_size_dist$index), 7)
random_indices
random_indices2 <- sample(unique(coarse_cohort_size_dist$index2), 7)
random_indices2<-c("weiblich_65 bis 74 Jahre", "männlich_75+", "weiblich_10 bis 19 Jahre",
  "männlich_65 bis 74 Jahre","männlich_20 bis 29 Jahre","männlich_45 bis 54 Jahre", "weiblich_10 bis 19 Jahre")


ggplot(filter(cohort_size_dist, index %in% random_indices), 
              aes( x=index,y = population)) +
         geom_boxplot() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Population by 5-Year Cohort")

ggplot(filter(coarse_cohort_size_dist, index2 %in% random_indices2), 
       aes( x=index2, y = population)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Population by 10-Year Cohort")


# smoothed time series

smoothed_time_series <- all_munip_pop %>%
  filter(municipality %in% sample(unique(all_munip_pop$municipality), 500)) %>%
  replace_na(list(population = 0)) %>%
  group_by(municipality, sex, age_group) %>%
  arrange(year) %>%
  mutate(rolling_mean_population = rollmean(population, k = 3, fill = NA, align = "right")) %>%
  ungroup()





unique_combinations <- smoothed_time_series %>%
  distinct(municipality, sex, age_group)

# Randomly select 3 combinations
set.seed(42) # for reproducibility
random_combinations <- unique_combinations %>%
  sample_n(3)

# Filter the dataframe for the randomly selected combinations
plot_data_multiple <- smoothed_time_series %>%
  inner_join(random_combinations, by = c("municipality", "sex", "age_group")) %>%
  # Create a unique identifier for each combination for faceting
  mutate(group_id = paste(municipality, sex, age_group, sep = " - "))

# Create the time series plot with faceting
ggplot(plot_data_multiple, aes(x = year)) +
  geom_line(aes(y = population, color = "Original Population", group = 1), linewidth = 0.8) +
  geom_line(aes(y = rolling_mean_population, color = "Smoothed Population", group = 1), linewidth = 0.8) +
  scale_color_manual(values = c("Original Population" = "blue", "Smoothed Population" = "red")) +
  labs(title = "Population Time Series for Randomly Selected Groups",
       x = "Year",
       y = "Population",
       color = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ group_id, scales = "free_y") # Display plots side by side






# for 2nd workshop
load(file.path(wd_res, "final_LINEXP_balanced.RData"))
load(file.path(wd_res, "final_LINEXP_prediction.RData"))
load(file.path(wd_res, "final_HP_balanced.RData"))
load(file.path(wd_res, "final_HP_prediction.RData"))
data <- tuned_LINEXP_test_export

set.seed(123)

train <- tuned_LINEXP_test_export %>% 
  filter(municipality_code == 41123,
         sex == 1,
         age_group == "65 - 74",
         year %in% 2002:2021) %>% pull(population)       

test <- tuned_LINEXP_test_export %>% 
  filter(municipality_code == 41123,
         sex == 1,
         age_group == "65 - 74",
         year %in% 2022:2024)  %>% pull(population)

pred <- data %>% 
  filter(municipality_code == 41123,
         sex == 1,
         age_group == "65 - 74",
         year %in% 2022:2024)  %>% pull(PRED_tuned_LINEXP)


plot_prediction_from_vectors(
  years_train = 2002:2021, 
  pop_train = train,
  years_test = 2022:2024, 
  pop_test = test,
  years_pred = 2022:2024,  
  pop_pred =pred,
  title = "LINEXP: Saxen (OÖ), männlich, 65-74",
  xlab  = "Year",
  ylab  = "Population"
)



# summarise to entrie munip
load(file.path(wd_res, "final_LINEXP_balanced.RData"))
load(file.path(wd_res, "final_LINEXP_prediction.RData"))
load(file.path(wd_res, "final_HP_balanced.RData"))
load(file.path(wd_res, "final_HP_prediction.RData"))
data <- tuned_LINEXP_test_export

train <- tuned_LINEXP_test_export %>% 
  filter(municipality_code == 20518,
         year %in% 2002:2021) %>%
  group_by(year) %>%
  summarise(population = sum(population), 
          PRED_tuned_LINEXP = sum(PRED_tuned_LINEXP)) %>% pull(population) 

test <- tuned_LINEXP_test_export %>% 
  filter(municipality_code == 20518, 
         year %in% 2022:2024) %>%
         group_by(year) %>%
  summarise(population = sum(population), 
            PRED_tuned_LINEXP = sum(PRED_tuned_LINEXP)) %>% pull(population)  

pred <- data %>% 
  filter(municipality_code == 20518,
         year %in% 2022:2024) %>%
  group_by(year) %>%
  summarise(PRED_tuned_LINEXP = sum(PRED_tuned_LINEXP)) %>% pull(PRED_tuned_LINEXP) 


plot_prediction_from_vectors(
  years_train = 2002:2021, 
  pop_train = train,
  years_test = 2022:2024, 
  pop_test = test,
  years_pred = 2022:2024,  
  pop_pred =pred,
  title = "LINEXP: Saxen (OÖ)",
  xlab  = "Year",
  ylab  = "Population"
)
