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
  group_by(municipality_code, year) %>%
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

munip_pop_by_cohort <- all_munip_pop %>%
  select(-reg_code, - municipality) %>%
  group_by(municipality_code, coarse_age_group, year) %>%
  summarise(population = sum(population, na.rm = T)) 


summary_df <- data.frame(
  Variable = c(
    "N",
    "Median population",
    "Mean population",
    "Standard deviation",
    "N by 2024 population",
    "0-1,000",
    "1,001-2,000",
    "2,001-5,000",
    "5,001-10,000",
    "10,001-100,000",
    "> 100,000",
    "Median Population per age group 2024",
    "0 - 9",
    "10 - 19",
    "20 - 29",
    "30 - 44",
    "45 - 54",
    "55 - 64",
    "65 - 74",
    "75+"
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
    median(munip_pop_by_cohort %>% filter(year == 2024, coarse_age_group == "0 - 9") %>% pull(population)),
    median(munip_pop_by_cohort %>% filter(year == 2024, coarse_age_group == "10 - 19")%>% pull(population)),
    median(munip_pop_by_cohort %>% filter(year == 2024, coarse_age_group == "20 - 29")%>% pull(population)),
    median(munip_pop_by_cohort %>% filter(year == 2024, coarse_age_group == "30 - 44")%>% pull(population)),
    median(munip_pop_by_cohort %>% filter(year == 2024, coarse_age_group == "45 - 54")%>% pull(population)),
    median(munip_pop_by_cohort %>% filter(year == 2024, coarse_age_group == "55 - 64")%>% pull(population)),
    median(munip_pop_by_cohort %>% filter(year == 2024, coarse_age_group == "65 - 74")%>% pull(population)),
    median(munip_pop_by_cohort %>% filter(year == 2024, coarse_age_group == "75+")%>% pull(population))
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



# heatmap of correlation of static covariates ----------------------------------
load(file.path(wd_data_work, "all_municipalities_population.RData"))

covariates <- read.csv2(file.path(wd_data_orig, "static_variables.csv"),
                        encoding = "latin1")

pop_dynamic_10y <- all_munip_pop %>%
  filter(year %in% c(2014,2024)) %>%
  group_by(municipality_code, year) %>%
  summarise(population = sum(population)) %>%
  pivot_wider(names_from = year, values_from = population) %>% 
  ungroup() %>%
  mutate(pop_dynamic_10y = `2024` / `2014` - 1)

all <- covariates %>%
  select(-Name) %>%
  rename(municipality_code = ID) %>%
  mutate(
    klassifikation_palme95 = case_when(
      klassifikation_palme95 == "Touristische Randgebiete" ~ 1,
      klassifikation_palme95 == "Extensive Industrieregionen" ~ 2,
      klassifikation_palme95 == "Umland" ~ 3,
      klassifikation_palme95 == "Industrialisierte Randgebiete" ~ 4,
      klassifikation_palme95 == "Mittelstädte" ~ 5,
      klassifikation_palme95 == "Intensive Tourismusregionen" ~ 6,
      klassifikation_palme95 == "Intensive Industrieregionen" ~ 7,
      klassifikation_palme95 == "Großstädte" ~ 8,
      klassifikation_palme95 == "Metropole" ~ 9,
      TRUE ~ NA_real_ # This is a catch-all for any values not matched, assign NA
    )
  ) %>%
  mutate(
    OeV.Güteklassen = case_when(
      OeV.Güteklassen == "B" ~ 1,
      OeV.Güteklassen == "C" ~ 2,
      OeV.Güteklassen == "D" ~ 3,
      OeV.Güteklassen == "E" ~ 4,
      OeV.Güteklassen == "F" ~ 5,
      OeV.Güteklassen == "G" ~ 6,
      OeV.Güteklassen == "ausser_oev_gk" ~ 7,
      TRUE ~ NA_real_ # Catch-all for any values not explicitly matched
    )
  ) %>%
  mutate(municipality_code = as.character(municipality_code)) %>%
  left_join(pop_dynamic_10y %>% select(municipality_code, pop_dynamic_10y))

cor_matrix <- cor(all[2:ncol(all)], use = "pairwise.complete.obs")

melted_correlation_matrix <- melt(cor_matrix) %>%
  filter(Var2 == "pop_dynamic_10y", Var1 != "pop_dynamic_10y")


ggplot(data = melted_correlation_matrix, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = brewer.pal(n = 8, name = "RdBu")[8], # Dark blue
                       high = brewer.pal(n = 8, name = "RdBu")[1], # Dark red
                       mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 1.5) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1), # Rotate x-axis labels
    axis.text.y = element_text(size = 8), # Adjust y-axis label size
    axis.title.x = element_blank(), # Remove x-axis title
    axis.title.y = element_blank(), # Remove y-axis title
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    plot.title = element_text(hjust = 0.5, face = "bold") # Center and bold the plot title
  ) +
  coord_fixed() #+
  #labs(title = "Correlation Covariates and Population Dynamics (2014-2024")





load(file.path(wd_data_work, "all_municipalities_population.RData"))

ggplot(all_munip_pop %>% filter(municipality_code == "62010", sex == 1, coarse_age_group == "20 - 29"),
       aes(x = year, y = population)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() + 
  labs(title = "Hohentauern, m, 20-29",
    x     = "Year",
    y     =  "Population")


load(file.path(wd_res, "2025-2035_TFT_balanced.RData"))
load(file.path(wd_data_work, "all_municipalities_population.RData"))


test <- filter(all_munip_pop, municipality_code == "10101", sex == 1, coarse_age_group == "0 - 9") %>%
  select(municipality_code, sex, coarse_age_group, year, population)%>%
  rename(age_group = coarse_age_group) %>%
  rename(tft_prediction = population) %>%
  mutate(sex = as.character(sex)) %>%
  bind_rows(tft_pred %>%
              filter(municipality_code == "10101", sex == "1.0", age_group == "0 - 9") %>%
              ungroup() %>%
              select(municipality_code, sex, age_group, year, tft_prediction))

ggplot(test, aes(x = year, y = tft_prediction))+
  geom_line()
