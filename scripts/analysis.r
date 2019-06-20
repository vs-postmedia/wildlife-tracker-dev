library(jsonlite)
library(dplyr)
library(purrr)
library(readxl)
library(janitor)
library(lubridate)
library(readr)

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
metro_data <- bear_data %>% 
  filter(
    tolower(encounter_locality) %in% metro_van
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
metro_by_year <- metro_data %>% 
  group_by(species_name, year) %>% 
  count %>% 
  arrange(-year)

# by month
metro_by_month <- metro_data %>% 
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
metro_attr <- metro_data %>% 
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

# metro 2019 w/ average
metro_monthly_w_average <- metro_data %>% 
  filter(
    year == 2019
  ) %>% 
  group_by(month) %>% 
  summarize(
    total_2019 = n()
  ) %>% 
  right_join(metro_monthly_avg, by = 'month')
  
# metro averages
metro_monthly_avg <- metro_data %>% 
  group_by(month, year) %>% 
  summarize(
    total = n()
  ) %>% 
  group_by(month) %>% 
  summarize(
    avg = mean(total),
    median = median(total)
  )


# muni 2019 w/ average
muni_monthly_w_average <- metro_data %>% 
  filter(
    year == 2019
  ) %>% 
  group_by(encounter_locality, month) %>% 
  summarize(
    total_2019 = n()
  ) %>% 
  right_join(muni_monthly_avg, by = c('encounter_locality', 'month'))


# metro muni breakdown averages
muni_monthly_avg <- metro_data %>% 
  group_by(encounter_locality, month, year) %>% 
  summarize(
    total = n()
  ) %>% 
  group_by(encounter_locality, month) %>% 
  summarize(
    monthly_total = sum(total),
    average = round(mean(total), 1),
    median = median(total)
  )
  
# Garbage over tme
garbage <- bear_data %>% 
  filter(
    grepl('17', attractant_ids),
    year == 2019
  ) %>% 
  group_by(attractant_names, encounter_locality, year) %>% 
  summarize(
    total = n()
  ) %>% 
  arrange(-total)




