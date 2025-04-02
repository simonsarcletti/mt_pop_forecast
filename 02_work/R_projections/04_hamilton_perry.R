############################################################################## #
# Filename
#    01_hamilton_perry.R
#
# Description
#   Projection with Hamilton-Perry method such as in Hauer 2018
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      2025-02-25
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #


##############################################################################~#
# Data reading #################################################################
raw_data <- read.csv2(file.path(wd_data_orig, "test_murtal.csv"),
                  header = TRUE,
                  sep = ";",
                  skip = 6,
                  fileEncoding = "latin1") 
View(data)

data <- raw_data %>%
  select(-Anmerkungen, -Werte) %>%
  rename(
    municipality = Gemeinde..Vergröberung.über.Politischen.Bezirk.,
    sex = Geschlecht,
    age = Alter.in.5.Jahresgruppen,
    year = Jahr,
    population = Anzahl
  ) %>%
  mutate(
    population = as.numeric(ifelse(population == "-", 0, population)),
    age = case_when(
      age %in% c("90 bis 94 Jahre", "95 bis 99 Jahre", "100 Jahre und älter") ~ "90+",
      TRUE ~ age
    )
  ) %>%
  group_by(municipality, sex, age, year) %>%
  summarise(population = sum(population), .groups = "drop") %>%
  unite("index", municipality, sex, age, sep = "_") # %>%
  #pivot_wider(names_from = year, values_from = population)

train_data <- data %>% filter(year %in% c(2002:2021))
test_data <- data %>% filter(year %in% c(2022:2024))

##############################################################################~#
# LIN/EXP ######################################################################
calculate_population_change <- function(x){
  c(NA, x[2:length(x)] - x[1:(length(x)-1)])
}

calculate_relative_population_change <- function(x){
  c(NA, (x[2:length(x)] - x[1:(length(x)-1)]) / x[1:(length(x)-1)])
}

train_data_growth <- train_data %>%
  group_by(index) %>%
  mutate(population_change = calculate_population_change(population)) %>%
  mutate(relative_population_change = calculate_relative_population_change(population))
  
prediction <- data.frame(index = NA, year = NA, population = NA)
for(group in unique(train_data$index)){
  annual_average_pop_growth <- mean(filter(train_data_growth, index == group)$population_change, na.rm = TRUE) 
  print(annual_average_pop_growth)
  #if(meanfilter(train_data_growth, index == group)
}












