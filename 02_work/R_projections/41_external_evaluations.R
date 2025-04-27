############################################################################## #
# Filename
#    40_external_evaluation.R
#
# Description
#   Evaluation of predictions by AG Prognose
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      20xx-xx-xx
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #
load(file.path(wd_data_work, "all_municipalities_population.RData"))
load(file.path(wd_data_work, "munip_size_group_mapping_2021.RData"))
municipality_size_group_mapping_2021 <- municipality_size_group_mapping_2021 %>%
  mutate(population_size_group = factor(population_size_group, 
                                        levels = c("< 500", "500-1000", "1000-2000", "2000-5000", "5000-20000", "20000-50000","> 50000"),
                                        ordered = TRUE))
# Carinthia --------------------------------------------------------------------
carinathia_forecast <- read_delim(
  file = file.path(wd_data_orig, "gemeindeprognose kaernten ab 2021.csv"),
  delim = ";",
  locale = locale(decimal_mark = ",", encoding = "Latin1")
)

carinthia_data <- carinathia_forecast %>%
  select(-Nummer) %>%
  rename(municipality_code = gkz,
         sex = Geschlecht,
         age = Alter) %>%
  pivot_longer(
    cols = -c(municipality_code, sex, age),
    names_to = "year",
    values_to = "carinthia_pred"
  ) %>%
  mutate(
    coarse_age_group = case_when(
      age >= 0 & age <= 9 ~ "0 - 9",
      age >= 10 & age <= 19 ~ "10 - 19",
      age >= 20 & age <= 29 ~ "20 - 29",
      age >= 30 & age <= 44 ~ "30 - 44",
      age >= 45 & age <= 54 ~ "45 - 54",
      age >= 55 & age <= 64 ~ "55 - 64",
      age >= 65 & age <= 74 ~ "65 - 74",
      age >= 75 ~ "75+",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(municipality_code, sex, year, coarse_age_group) %>%
  summarise(carinthia_pred = sum(carinthia_pred)) %>%
  filter(year >= 2021)

carinthia_data_size <- carinthia_data %>%
  mutate(municipality_code = as.character(municipality_code),
         year = as.numeric(year)) %>%
  left_join(
    select(
      all_munip_pop,
      municipality_code,
      year,
      sex,
      coarse_age_group,
      population
    ),
    by = join_by(municipality_code, year, sex, coarse_age_group)
  )  %>%
  left_join(municipality_size_group_mapping_2021) %>%
  group_by(municipality_code, population_size_group, year) %>%
  summarise(population = sum(population, na.rm=T),
            carinthia_pred = sum(carinthia_pred, na.rm = T))

eval_carinthia_size <- evaluate_prediction(carinthia_data_size,
                    prediction_year = 2022:2024,
                    true_values = "population",
                    prediction_values = "carinthia_pred",
                    grouping_variable = "population_size_group")

write.csv(eval_carinthia_size, file = file.path(wd_res, "eval_carinthia_size.csv"))


carinthia_data_age_group <- carinthia_data %>%
  mutate(municipality_code = as.character(municipality_code),
         year = as.numeric(year)) %>%
  left_join(
    select(
      all_munip_pop,
      municipality_code,
      year,
      sex,
      coarse_age_group,
      population
    ),
    by = join_by(municipality_code, year, sex, coarse_age_group)
  )  %>%
  group_by(municipality_code, coarse_age_group, year) %>%
  summarise(population = sum(population, na.rm=T),
            carinthia_pred = sum(carinthia_pred, na.rm = T))

eval_carinthia_age <- evaluate_prediction(carinthia_data_age_group,
                                           prediction_year = 2022:2024,
                                           true_values = "population",
                                           prediction_values = "carinthia_pred",
                                           grouping_variable = "coarse_age_group")

write.csv(eval_carinthia_age, file = file.path(wd_res, "eval_carinthia_age.csv"))

# upper Austria ----------------------------------------------------------------
upper_austria_forecast <- read_delim(
  file = file.path(wd_data_orig, "gemeindeprognose oberoesterreich ab 2024.csv"),
  delim = ";",
  locale = locale(decimal_mark = ",", encoding = "Latin1")
)

upper_austria_data <- upper_austria_forecast %>%
  select(-Gemeinde) %>%
  rename(municipality_code = Gemnr) %>%
  mutate(municipality_code = as.character(municipality_code)) %>%
  pivot_longer(
    cols = -c(municipality_code),
    names_to = "year",
    values_to = "upper_austria_pred"
  ) %>% 
  mutate(year = as.numeric(year)) %>%
  filter(year == 2024) %>%
  left_join(all_munip_pop %>% group_by(municipality_code, year) %>% summarise(population = sum(population)),
            by = join_by(municipality_code, year)) %>%
  left_join(municipality_size_group_mapping_2021)



evaluate_prediction(upper_austria_data,
                    prediction_year = 2024,
                    true_values = "population",
                    prediction_values = "upper_austria_pred",
                    grouping_variable = "population_size_group")


# Vienna -----------------------------------------------------------------------
vienna_forecast_1 <- read_delim(
  file = file.path(wd_data_orig, "gemeindeprognose Wien ab 2018.csv"),
  delim = ";",
  locale = locale(decimal_mark = ",", encoding = "Latin1")
) %>%
  filter(REF_YEAR %in% 2021:2022) 
vienna_forecast_2 <- read_delim(
  file = file.path(wd_data_orig, "gemeindeprognose Wien ab 2018.csv"),
  delim = ";",
  locale = locale(decimal_mark = ",", encoding = "Latin1")
) %>%
  filter(REF_YEAR %in% 2023:2024) 


vienna_data <- vienna_forecast_1 %>%
  select(DISTRICT_CODE, REF_YEAR, SEX, AGE5, AUT, FOR) %>%
  bind_rows(select(vienna_forecast_2, DISTRICT_CODE, REF_YEAR, SEX, AGE5, AUT, FOR)) %>%
  mutate(vienna_pred = AUT + FOR) %>%
  select(-c(AUT, FOR)) %>%
  rename(municipality_code = DISTRICT_CODE,
         year = REF_YEAR, 
         sex = SEX) %>%
  mutate(age_group = case_when(AGE5 %in% c(1,2) ~ "0 - 9",
                               AGE5 %in% c(3,4) ~ "10 - 19",
                               AGE5 %in% c(5,6) ~ "20 - 29",
                               AGE5 %in% c(7,8,9) ~ "30 - 44",
                               AGE5 %in% c(10, 11) ~ "45 - 54",
                               AGE5 %in% c(12, 13) ~ "55 - 64",
                               AGE5 %in% c(14, 15) ~ "65 - 74",
                               AGE5 %in% c(16, 17) ~ "75+")) %>%
  select(-AGE5) %>%
  group_by(municipality_code, sex, age_group, year) %>%
  summarise(vienna_pred = sum(vienna_pred))
 
vienna_data <- vienna_data %>%
  mutate(municipality_code = as.character(municipality_code+1)) %>%
  left_join(select(all_munip_pop, municipality_code, sex, year, coarse_age_group, population),
            by = join_by(municipality_code, year, sex, age_group == coarse_age_group)) %>%
  left_join(municipality_size_group_mapping_2021)

vienna_data_size <- vienna_data %>%
  ungroup() %>% group_by(municipality_code, year, population_size_group) %>%
  summarise(population = sum(population, na.rm = T),
            vienna_pred = sum(vienna_pred, na.rm = T))


eval_size <- evaluate_prediction(vienna_data_size,
                    prediction_year = 2022:2024,
                    true_values = "population",
                    prediction_values = "vienna_pred",
                    grouping_variable = "population_size_group")


write.csv(eval_size, file = file.path(wd_res, "eval_vienna_size.csv"))


vienna_data_age <- vienna_data %>%
  ungroup() %>% group_by(municipality_code, year, age_group) %>%
  summarise(population = sum(population, na.rm = T),
            vienna_pred = sum(vienna_pred, na.rm = T))


eval_age <- evaluate_prediction(vienna_data_age,
                    prediction_year = 2022:2024,
                    true_values = "population",
                    prediction_values = "vienna_pred",
                    grouping_variable = "age_group")


write.csv(eval_age, file = file.path(wd_res, "eval_vienna_age.csv"))


