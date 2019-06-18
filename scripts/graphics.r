library(ggplot2)
library(ggmap)
library(mapview)

# setup Google API key (w/ sanity check)
api = "AIzaSyBF-15yhpfXVwqu3VXQ2DA-DO-kTpC6hs8"
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
by_year %>% 
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
  labs(title = 'Encounters by year')
  

# BY MONTH
by_month %>% ggplot(aes(
  x = month, 
  y = n, 
  fill = species_name  
  )) + 
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(title = 'Encounters by month')


# BY MUNI/YEAR
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
  # geom_line() + 
  facet_wrap(~encounter_locality) +
  theme_minimal() +
  theme(legend.position = 'top') +
  labs(title = 'Encounters by year')
   