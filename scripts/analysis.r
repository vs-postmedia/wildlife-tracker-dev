library(jsonlite)
library(dplyr)
library(purrr)
library(readxl)
library(janitor)
library(lubridate)

bcc_data <- read_xlsx('data/bc-conservation/tabula-predatorstatisticsblackbear.xlsx') %>% clean_names()
# downloaded csv from warp map interface
# dl_data <- read.csv('data/encounter-267428-266283.csv') 
warp_json <- fromJSON('data/warp/warp-data-2019.json')

# load in all the records in the warp-data folder
setwd('data/warp/')
# grab each file in the directory & load it into a list
warp_json <- list.files() %>% map(fromJSON)
# back to the main directoru
setwd('../../')

# extract recrods from list & combine into single dataframe
df <- data.frame()
bind_data <- function(d) {
  tbl = bind_rows(df, d$records)
}

warp_data <- map_df(warp_json, bind_data)

# extract records & parse year & month into separate columns
bear_data <-warp_data %>% 
  mutate(
    year = year(encounter_date),
    month = month(encounter_date, label = TRUE)
  )

# join so we can create lookup tables for encounter & outcome IDs
join_data <- inner_join(warp_data, dl_data, by = 'encounter_id') %>%
  select(-ends_with('.x'))
# outcome lookup table
outcome_lookup <- data %>%
  group_by(outcome_name, encounter_outcome_id) %>% 
  count() %>% 
  na.omit() %>% 
  select(-n)
# encounter type lookup
encounter_lookup <- data %>%
  group_by(enctype_name, encounter_enctype_id) %>% 
  count() %>% 
  na.omit() %>% 
  select(-n)
  

####
# 
# ANALYSIS 
# 
####

# bears by type
bear_data %>% 
  group_by(species_name) %>% 
  count

# by year
by_year <- bear_data %>% 
  group_by(species_name, year) %>% 
  count %>% 
  arrange(-year)

# by month
by_month <- bear_data %>% 
  group_by(species_name, month) %>% 
  count

# by various data subsets
bear_data %>% 
  # encounter type - 40% sightings, 35% food conditioned
  # group_by(encounter_enctype_id) %>% 
  # outcome - 75% Not specified, 15% advice
  # group_by(outcome_name) %>%
  # location
  # group_by(encounter_locality) %>%
  # by attraction - 56% NA, 25% garbage
  group_by(attractant_names) %>%
  summarize(
    total = n()
  ) %>% 
  mutate(
    pct = total/sum(total) * 100
  ) %>% 
  # inner_join(encounter_lookup, by = 'encounter_enctype_id') %>% 
  arrange(-pct)


# METRO by attractant
metro_attr <- bear_data %>% 
  filter(
    tolower(encounter_locality) %in% metro_van
  ) %>% 
  group_by(encounter_locality, attractant_names) %>%
  summarize(
    total = n()
  ) %>% 
  mutate(
    pct = total/sum(total) * 100
  ) %>% 
  filter(
    attractant_names != 'NOT APPLICABLE'
  ) %>% 
  arrange(-total)

  





