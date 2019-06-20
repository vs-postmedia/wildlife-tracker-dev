library(dplyr)
library(ggplot2)
library(ggmap)
library(sf)
library(mapview)

# draw them on a map
as_tibble <- as_tibble(filter(metro_data, location_approximate == 0, year == 2019))
as_sf <- st_as_sf(as_tibble, coords = c('encounter_lng', 'encounter_lat'), crs = 4326)
# zol assigns col to legend, cex sizes circle by col value
mapview(as_sf, 
        # zcol = 'species_name'
        zcol = 'year'
)


# setup Google API key (w/ sanity check)
api = ''
register_google(key = api)
ggmap_credentials()

gg <- get_googlemap(
  center = c(-122.926256, 49.246885),
  maptype = 'terrain'
  # scale = 2,
  # zoom = 10
)

# ggplot() +
gg +
  stat_density2d(
    data = filter(bear_data, tolower(encounter_locality) %in% metro_van),
    aes(x = encounter_lng, y = encounter_lat, fill = ..level.., alpha = 0.25),
    size = 0.01, bins = 30,
    geom = "polygon"
  ) + 
  scale_fill_gradient(low="deepskyblue2", high="firebrick1", name="Distribution") +
  NULL



# BY YEAR
metro_by_year %>% 
  ggplot(aes(
    x = year, 
    y = n, 
    color = species_name,
    fill = species_name  
  )) + 
  geom_bar(stat = 'identity') +
  # geom_line() + 
  theme_minimal() +
  theme(legend.position = 'top') +
  labs(title = 'Metro Encounters by year')
  

# BY MONTH
metro_by_month %>% ggplot(aes(
  x = month, 
  y = n, 
  fill = species_name  
  )) + 
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(title = 'Metro Encounters by month')


# BY MUNI/YEAR w/ garbage
bear_data %>% 
  filter(
    tolower(encounter_locality) %in% metro_van
  ) %>% 
  group_by(encounter_locality, year, attractant_names, species_name) %>% 
  summarize(
    total = n()
  ) %>%
  filter(
    attractant_names == 'GARBAGE'
  ) %>%
  ggplot(aes(
    x = year, 
    y = total,
    color = species_name,
    fill = species_name  
  )) + 
  geom_bar(stat = 'identity') +
  facet_wrap(~encounter_locality) +
  theme_minimal() +
  theme(legend.position = 'top') +
  labs(title = 'Encounters by year b/c garbage')


# metro by month w/ average
muni_monthly_w_average %>% 
  select(-monthly_total) %>% 
  tidyr::gather(key = measure, value = value, total_2019:median) %>% 
  ggplot(aes(
    x = month,
    y = value,
    fill = measure
  )) + 
  geom_col(position = 'dodge') +
  facet_wrap(~encounter_locality) +
  theme_minimal() +
  theme(legend.position = 'top') + 
  labs(title = 'Encounters by month by metro')
  
  
bear_data %>% 
  filter(grepl('17', attractant_ids)) %>% 
  ggplot(aes(
    y = count(attractant_ids),
    x = year
  )) +
  geom_line()
  
  
  
  

   