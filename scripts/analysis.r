library(jsonlite)
library(dplyr)
library(readxl)
library(janitor)
library(lubridate)

bcc_data <- read_xlsx('data/bc-conservation/tabula-predatorstatisticsblackbear.xlsx') %>% clean_names()
data <- fromJSON('data/warp-data.json')

# extract recrods & parse year & month into separate columns
bears <- data$records %>% 
  mutate(
    year = year(encounter_date),
    month = month(encounter_date)
  )


# bears by type
bears %>% 
  group_by(species_name) %>% 
  count

# by month
bears %>% 
  group_by(species_name, month) %>% 
  count %>% 
  arrange(-month)

# by location
bears %>% 
  group_by(encounter_locality) %>% 
  summarize(
    total = n()
  ) %>% 
  mutate(
    pct = total/sum(total) * 10
  ) %>% 
  arrange(-pct)


# by attraction
bears %>% 
  group_by(attractant_names) %>% 
  summarize(
    total = n()
  ) %>% 
  mutate(
    pct = total/sum(total) * 10
  ) %>% 
  arrange(-pct)


