############################################################################## #
# Filename
#    01_data_prep.R
#
# Description
#   Prepare data for projection via different methods
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      2025-02-25
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #




##############################################################################~#
# Indepentend projection of municipality districts by OEROK
# Data reading #################################################################
district_projection_raw <- read_delim(
  file.path(wd_data_orig, "oerok_pop_projeciton.csv"),
  delim = ";",
  locale = locale(decimal_mark = ",", encoding = "Latin1")
) %>%
  select(
    -c(
      "Prognoseregion",
      "bundesland",
      "NUTS-3",
      "Geburtsland",
      "Geschlecht",
      "Altersgruppe...12"
    )
  ) %>% # Geschlechtgruppe 1:männlich, 2:weiblich
  rename(
    district_identifier = Kennzahl,
    name = Name,
    reg_code = `reg-code`,
    pop_group = Bevölkerungsgruppe,
    sex = Geschlechtsgruppe,
    age = `Altersgruppe...11`
  ) %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "year",
               values_to = "projected_population")



district_projection <- district_projection_raw %>%
  mutate(age = if_else(age == "100 und älter", 100, as.numeric(age))) %>%
  group_by(district_identifier, sex, age, year) %>%
  summarise(projected_population = sum(projected_population),
            .groups = "drop") %>%
  mutate(age_group = case_when(age >= 80 ~ "80+", TRUE ~ paste0(floor(age / 5) * 5, "-", floor(age / 5) * 5 + 4))) %>%
  group_by(district_identifier, sex, age_group, year) %>%
  summarise(projected_population = sum(projected_population),
            .groups = "drop") %>%
  mutate(age_group = factor(age_group, levels = c(paste0(
    seq(0, 75, 5), "-", seq(4, 79, 5)
  ), "80+"))) %>%
  arrange(age_group)


district_projection <- district_projection %>%
  mutate(
    coarse_age_group = case_when(
      age_group %in% c("0-4", "5-9") ~ "0 - 9",
      age_group %in% c("10-14", "15-19") ~ "10 - 19",
      age_group %in% c("20-24", "25-29") ~ "20 - 29",
      age_group %in% c("30-34", "35-39", "40-44") ~ "30 - 44",
      age_group %in% c("45-49", "50-54") ~ "45 - 54",
      age_group %in% c("55-59", "60-64") ~ "55 - 64",
      age_group %in% c("65-69", "70-74") ~ "65 - 74",
      age_group %in% c("75-79", "80+") ~ "75+"
    )
  ) %>%
  group_by(district_identifier, sex, year, coarse_age_group) %>%
  summarise(projected_population = sum(projected_population)) %>%
  relocate(coarse_age_group, .after = sex) %>%
  mutate(year = as.numeric(year))


# control sum
if (nrow(district_projection) != 121 * 2 * 8 * 31) {
  stop("1111 ALERT")
} else {
  print("nice!!!")
}

# control sums
filter(district_projection_raw,
       district_identifier == 4090 &
         year == 2030) %>% ungroup() %>% summarise(sum(projected_population))
filter(district_projection, district_identifier == 4090 &
         year == 2030) %>% ungroup() %>% summarise(sum(projected_population))



save(district_projection,
     file = file.path(wd_data_work, "district_projection.RData"))





##############################################################################~#
# Municipality Population Data
# Data reading #################################################################
files <- list.files(path = wd_data_orig,
                    pattern = "^munip_pop.*\\.csv$",
                    full.names = TRUE)

all_munip_pop <- tibble(
  municipality = character(),
  year = character(),
  sex = character(),
  age_group = character(),
  population = numeric()
)


for (file in files) {
  print(file)
  munip_data <- read_delim(
    file,
    delim = ";",
    skip = 6,
    locale = locale(decimal_mark = ",", encoding = "Latin1")
  ) %>%
    select(-c(Werte, Anmerkungen)) %>%
    rename(
      year = Jahr,
      municipality = `Gemeinde (Vergröberung über Politischen Bezirk)`,
      sex = Geschlecht,
      age_group = `Alter in 5-Jahresgruppen`,
      population = Anzahl
    ) %>%
    slice(1:(n() - 9)) %>%
    relocate(municipality, .before = year) %>%
    mutate(population = as.numeric(population)) %>%
    mutate(age_group = case_when(
      age_group %in% c(
        "80 bis 84 Jahre",
        "85 bis 89 Jahre",
        "90 bis 94 Jahre",
        "95 bis 99 Jahre",
        "100 Jahre und älter"
      ) ~ "80+",
      TRUE ~ age_group
    )) %>%
    group_by(year, municipality, sex, age_group) %>%
    summarise(population = sum(population, na.rm = TRUE),
              .groups = "drop")
  
  all_munip_pop <- all_munip_pop %>%
    bind_rows(munip_data)
}




all_munip_pop <- all_munip_pop %>%
  separate_wider_delim(
    municipality,
    delim = "<",
    names = c("municipality", "municipality_code")
  ) %>%
  mutate(municipality_code = str_remove(municipality_code, ">"))


all_munip_pop %>% filter(municipality_code == 50209, year == 2023) %>% summarise(population = sum(population, na.rm = T))

all_munip_pop <- all_munip_pop %>%
  filter(municipality != "Wien ") %>%
  replace_na(list(population = 0)) %>%
  group_by(municipality_code, sex, age_group) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(year) %>%
  mutate(rolling_mean_population = rollmean(
    population,
    k = 3,
    fill = NA,
    align = "right"
  )) %>%
  ungroup()

municipality_munip_code_map <- distinct(all_munip_pop, municipality_code, .keep_all =
                                          TRUE) %>%
  select(municipality_code, municipality)


all_munip_pop <- all_munip_pop %>%
  select(-municipality) %>%
  mutate(
    coarse_age_group = case_when(
      age_group %in% c("bis 4 Jahre", "5 bis 9 Jahre") ~ "0 - 9",
      age_group %in% c("10 bis 14 Jahre", "15 bis 19 Jahre") ~ "10 - 19",
      age_group %in% c("20 bis 24 Jahre", "25 bis 29 Jahre") ~ "20 - 29",
      age_group %in% c("30 bis 34 Jahre", "35 bis 39 Jahre", "40 bis 44 Jahre") ~ "30 - 44",
      age_group %in% c("45 bis 49 Jahre", "50 bis 54 Jahre") ~ "45 - 54",
      age_group %in% c("55 bis 59 Jahre", "60 bis 64 Jahre") ~ "55 - 64",
      age_group %in% c("65 bis 69 Jahre", "70 bis 74 Jahre") ~ "65 - 74",
      age_group %in% c("75 bis 79 Jahre", "80+") ~ "75+"
    )
  ) %>%
  mutate(sex = case_when(sex == "männlich" ~ 1, sex == "weiblich" ~ 2)) %>%
  group_by(municipality_code, sex, year, coarse_age_group) %>%
  summarise(
    population = sum(population),
    smoothed_population = sum(rolling_mean_population)
  ) %>%
  ungroup() %>%
  relocate(coarse_age_group, .after = sex) %>%
  left_join(municipality_munip_code_map, by = join_by(municipality_code)) %>%
  relocate(municipality, .after = municipality_code)


all_munip_pop <- all_munip_pop %>%
  mutate(reg_code = substr(municipality_code, 1, 3)) %>%
  relocate(reg_code, .after = municipality_code) %>%
  mutate(reg_code = as.numeric(reg_code)) %>%
  mutate(municipality = trimws(municipality, which = "right", )) %>%
  mutate(
    reg_code = case_when(
      municipality %in% c(
        "Alland",
        "Bad Vöslau",
        "Baden",
        "Blumau-Neurißhof",
        "Ebreichsdorf",
        "Günselsdorf",
        "Heiligenkreuz",
        "Klausen-Leopoldsdorf",
        "Kottingbrunn",
        "Leobersdorf",
        "Mitterndorf an der Fischa",
        "Oberwaltersdorf",
        "Pfaffstätten",
        "Pottendorf",
        "Reisenberg",
        "Schönau an der Triesting",
        "Seibersdorf",
        "Sooß",
        "Tattendorf",
        "Teesdorf",
        "Traiskirchen",
        "Trumau"
      ) ~ 3061,
      municipality %in% c(
        "Altenmarkt an der Triesting",
        "Berndorf",
        "Enzesfeld-Lindabrunn",
        "Furth an der Triesting",
        "Hernstein",
        "Hirtenberg",
        "Pottenstein",
        "Weissenbach an der Triesting"
      ) ~ 3062,
      municipality %in% c(
        "Drösing",
        "Dürnkrut",
        "Hauskirchen",
        "Hohenau an der March",
        "Jedenspeigen",
        "Neusiedl an der Zaya",
        "Palterndorf-Dobermannsdorf",
        "Ringelsdorf-Niederabsdorf",
        "Sulz im Weinviertel",
        "Zistersdorf"
      ) ~ 3082,
      municipality %in% c(
        "Aderklaa",
        "Andlersdorf",
        "Angern an der March",
        "Auersthal",
        "Bad Pirawarth",
        "Deutsch-Wagram",
        "Ebenthal",
        "Eckartsau",
        "Engelhartstetten",
        "Gänserndorf",
        "Glinzendorf",
        "Groß-Enzersdorf",
        "Großhofen",
        "Groß-Schweinbarth",
        "Haringsee",
        "Hohenruppersdorf",
        "Lassee",
        "Leopoldsdorf im Marchfeld",
        "Mannsdorf an der Donau",
        "Marchegg",
        "Markgrafneusiedl",
        "Matzen-Raggendorf",
        "Obersiebenbrunn",
        "Orth an der Donau",
        "Parbasdorf",
        "Prottes",
        "Raasdorf",
        "Schönkirchen-Reyersdorf",
        "Spannberg",
        "Strasshof an der Nordbahn",
        "Untersiebenbrunn",
        "Velm-Götzendorf",
        "Weiden an der March",
        "Weikendorf"
      ) ~ 3081,
      municipality %in% c(
        "Altlichtenwarth",
        "Asparn an der Zaya",
        "Bernhardsthal",
        "Drasenhofen",
        "Falkenstein",
        "Fallbach",
        "Gaubitsch",
        "Gaweinstal",
        "Gnadendorf",
        "Großharras",
        "Großkrut",
        "Hausbrunn",
        "Herrnbaumgarten",
        "Laa an der Thaya",
        "Ladendorf",
        "Mistelbach",
        "Neudorf im Weinviertel",
        "Niederleis",
        "Ottenthal",
        "Poysdorf",
        "Rabensburg",
        "Schrattenberg",
        "Staatz",
        "Stronsdorf",
        "Unterstinkenbrunn",
        "Wildendürnbach",
        "Wilfersdorf"
      ) ~ 3161,
      municipality %in% c(
        "Bockfließ",
        "Großebersdorf",
        "Großengersdorf",
        "Hochleithen",
        "Kreuttal",
        "Kreuzstetten",
        "Pillichsdorf",
        "Ulrichskirchen-Schleinbach",
        "Wolkersdorf im Weinviertel"
      ) ~ 3162,
      municipality %in% c(
        "Gablitz",
        "Mauerbach",
        "Pressbaum",
        "Purkersdorf",
        "Tullnerbach",
        "Wolfsgraben"
      ) ~ 3191,
      municipality %in% c(
        "Altlengbach",
        "Asperhofen",
        "Böheimkirchen",
        "Brand-Laaben",
        "Eichgraben",
        "Frankenfels",
        "Gerersdorf",
        "Hafnerbach",
        "Haunoldstein",
        "Herzogenburg",
        "Hofstetten-Grünau",
        "Inzersdorf-Getzersdorf",
        "Kapelln",
        "Karlstetten",
        "Kasten bei Böheimkirchen",
        "Kirchberg an der Pielach",
        "Kirchstetten",
        "Loich",
        "Maria Anzbach",
        "Markersdorf-Haindorf",
        "Michelbach",
        "Neidling",
        "Neulengbach",
        "Neustift-Innermanzing",
        "Nußdorf ob der Traisen",
        "Ober-Grafendorf",
        "Obritzberg-Rust",
        "Prinzersdorf",
        "Pyhra",
        "Rabenstein an der Pielach",
        "St. Margarethen an der Sierning",
        "Schwarzenbach an der Pielach",
        "Statzendorf",
        "Stössing",
        "Traismauer",
        "Weinburg",
        "Perschling",
        "Wilhelmsburg",
        "Wölbling"
      ) ~ 3192,
      municipality %in% c(
        "Bad Leonfelden",
        "Haibach im Mühlkreis",
        "Oberneukirchen",
        "Ottenschlag im Mühlkreis",
        "Reichenau im Mühlkreis",
        "Reichenthal",
        "Schenkenfelden",
        "Vorderweißenbach",
        "Zwettl an der Rodl"
      ) ~ 4161,
      municipality %in% c(
        "Alberndorf in der Riedmark",
        "Altenberg bei Linz",
        "Eidenberg",
        "Engerwitzdorf",
        "Feldkirchen an der Donau",
        "Gallneukirchen",
        "Goldwörth",
        "Gramastetten",
        "Hellmonsödt",
        "Herzogsdorf",
        "Kirchschlag bei Linz",
        "Lichtenberg",
        "Ottensheim",
        "Puchenau",
        "Sonnberg im Mühlkreis",
        "St. Gotthard im Mühlkreis",
        "Steyregg",
        "Walding"
      ) ~ 4162,
      municipality %in% c(
        "Andelsbuch",
        "Au",
        "Bezau",
        "Bizau",
        "Damüls",
        "Egg",
        "Hittisau",
        #"Krumbach",
        "Langenegg",
        "Lingenau",
        "Mellau",
        "Mittelberg",
        "Reuthe",
        "Schnepfau",
        "Schoppernau",
        "Schröcken",
        "Schwarzenberg",
        "Sibratsgfäll"
      ) ~ 8021,
      municipality %in% c(
        "Alberschwende",
        "Bildstein",
        "Bregenz",
        "Buch",
        "Doren",
        "Eichenberg",
        "Fußach",
        "Gaißau",
        "Hard",
        "Höchst",
        "Hohenweiler",
        "Hörbranz",
        "Kennelbach",
        "Langen bei Bregenz",
        "Lauterach",
        "Lochau",
        "Möggers",
        "Riefensberg",
        "Schwarzach",
        "Sulzberg",
        "Wolfurt"
      ) ~ 8022,
      # sonderfall warth und krumbach!
      municipality_code == "31843" ~ 318,
      municipality_code == "80239" ~ 8021,
      municipality_code == "80221" ~ 8021,
      reg_code %in% c(102, 103) ~ 102,
      TRUE ~ reg_code
    )
  )

all_munip_pop$reg_code <- ifelse(all_munip_pop$reg_code <= 1000,
                                 all_munip_pop$reg_code * 10,
                                 all_munip_pop$reg_code)

# check whether each Bundesland does have all municipalities
all_munip_pop %>% filter(substr(reg_code, 1, 1) == 9) %>%
  distinct(municipality_code) %>% nrow()


if (nrow(all_munip_pop) != 2 * 8 * 2115 * 23) {
  stop("1111 ALERT")
} else {
  print("nice, row count is okay!!!")
}

if (length(unique(all_munip_pop$reg_code)) != 121) {
  stop("1111 ALERT")
} else {
  print("nice, all reg_codes!!!")
}



save(all_munip_pop,
     file = file.path(wd_data_work, "all_municipalities_population.RData"))


# for emergency: municipality_code, reg_code mapping
municipality_reg_mapping <- all_munip_pop %>%
  select(municipality_code, reg_code) %>%
  distinct(municipality_code, .keep_all = TRUE)

save(
  municipality_reg_mapping,
  file = file.path(wd_data_work, "municipality_code_reg_code_mapping.RData")
)


# Prep auf Austrian forecast for Plausbility 
aut_forecast <- read_delim(
  file.path(wd_data_orig, "stat_aut_forecast.csv"),
  delim = ";",
  skip = 6,
  locale = locale(decimal_mark = ",", encoding = "Latin1")
) %>%
  select(-c(Werte, Anmerkungen)) %>%
  slice(1:(n() - 9)) %>%
  rename(
    year = Jahr,
    age = Alter,
    population = Anzahl
  ) %>%
  select(- `...6`) %>%
  mutate(coarse_age_group = case_when(
    # 0–9 Jahre
    age %in% c(
      "0 Jahre", "1 Jahr", "2 Jahre", "3 Jahre", "4 Jahre",
      "5 Jahre", "6 Jahre", "7 Jahre", "8 Jahre", "9 Jahre"
    ) ~ "0 - 9",
    
    # 10–19 Jahre
    age %in% c(
      "10 Jahre", "11 Jahre", "12 Jahre", "13 Jahre", "14 Jahre",
      "15 Jahre", "16 Jahre", "17 Jahre", "18 Jahre", "19 Jahre"
    ) ~ "10 - 19",
    
    # 20–29 Jahre
    age %in% c(
      "20 Jahre", "21 Jahre", "22 Jahre", "23 Jahre", "24 Jahre",
      "25 Jahre", "26 Jahre", "27 Jahre", "28 Jahre", "29 Jahre"
    ) ~ "20 - 29",
    
    # 30–44 Jahre
    age %in% c(
      "30 Jahre", "31 Jahre", "32 Jahre", "33 Jahre", "34 Jahre",
      "35 Jahre", "36 Jahre", "37 Jahre", "38 Jahre", "39 Jahre",
      "40 Jahre", "41 Jahre", "42 Jahre", "43 Jahre", "44 Jahre"
    ) ~ "30 - 44",
    
    # 45–54 Jahre
    age %in% c(
      "45 Jahre", "46 Jahre", "47 Jahre", "48 Jahre", "49 Jahre",
      "50 Jahre", "51 Jahre", "52 Jahre", "53 Jahre", "54 Jahre"
    ) ~ "45 - 54",
    
    # 55–64 Jahre
    age %in% c(
      "55 Jahre", "56 Jahre", "57 Jahre", "58 Jahre", "59 Jahre",
      "60 Jahre", "61 Jahre", "62 Jahre", "63 Jahre", "64 Jahre"
    ) ~ "55 - 64",
    
    # 65–74 Jahre
    age %in% c(
      "65 Jahre", "66 Jahre", "67 Jahre", "68 Jahre", "69 Jahre",
      "70 Jahre", "71 Jahre", "72 Jahre", "73 Jahre", "74 Jahre"
    ) ~ "65 - 74",
    
    # 75+ Jahre (including “100 Jahre und älter”)
    age %in% c(
      "75 Jahre", "76 Jahre", "77 Jahre", "78 Jahre", "79 Jahre",
      "80 Jahre", "81 Jahre", "82 Jahre", "83 Jahre", "84 Jahre",
      "85 Jahre", "86 Jahre", "87 Jahre", "88 Jahre", "89 Jahre",
      "90 Jahre", "91 Jahre", "92 Jahre", "93 Jahre", "94 Jahre",
      "95 Jahre", "96 Jahre", "97 Jahre", "98 Jahre", "99 Jahre",
      "100 Jahre und älter"
    ) ~ "75+",
    
    TRUE ~ NA_character_
  )) %>%
  group_by(year, coarse_age_group) %>%
  summarise(population = sum(population))

save(
  aut_forecast,
  file = file.path(wd_data_work, "aut_forecast.RData")
)



