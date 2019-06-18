library(jsonlite)
library(dplyr)
library(readxl)
library(janitor)
library(lubridate)

bcc_data <- read_xlsx('data/bc-conservation/tabula-predatorstatisticsblackbear.xlsx') %>% clean_names()
warp_data <- fromJSON('data/warp/warp-data-2019.json')
dl_data <- read.csv('data/encounter-267428-266283.csv')

# join so we can create lookup tables for encounter & outcome IDs
data <- inner_join(warp_data$records, dl_data, by = 'encounter_id') %>%
  select(-ends_with('.x'))

# extract records & parse year & month into separate columns
bears <- warp_data$records %>% 
  mutate(
    year = year(encounter_date),
    month = month(encounter_date)
  )

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
bears %>% 
  group_by(species_name) %>% 
  count

# by month
bears %>% 
  group_by(species_name, month) %>% 
  count %>% 
  arrange(-month)

# by various data subsets
loc <- data %>% 
  # encounter type - 40% sightings, 35% food conditioned
  # group_by(enctype_name) %>%
  # outcome - 75% Not specified, 15% advice
  # group_by(outcome_name) %>%
  # location
  group_by(encounter_locality.y) %>%
  # by attraction - 56% NA, 25% garbage
  # group_by(attractant_names.y) %>%
  summarize(
    total = n()
  ) %>% 
  mutate(
    pct = total/sum(total) * 100
  ) %>% 
  arrange(-pct)


# METRO by attractant
data %>% 
  filter(
    tolower(encounter_locality.y) %in% metro_van
  ) %>% 
  group_by(encounter_locality.y, attractant_names.y) %>%
  summarize(
    total = n()
  ) %>% 
  mutate(
    pct = total/sum(total) * 100
  ) %>% 
  filter(
    attractant_names.y != 'NOT APPLICABLE'
  ) %>% 
  arrange(-total)

  





