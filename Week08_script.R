## Data Visualization (GOVT16-QSS17) Fall 2020
## Geospatial Data in R
##
## Name: John Keane
## Date: 11/11/20

# Set up environment
library(tidyverse)
library(USAboundaries)
library(USAboundariesData)
library(sf)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

# 1.
city_data <- us_cities(states = c("NH", "VT"))
state_data <- us_states(states = c("NH", "VT"))

st_point(c(-72.2896, 43.7022))

hanover <- st_sf(
  city = "Hanover",
  state_name = "New Hampshire",
  state_abbr = "NH",
  county = "GRAFTON",
  county_name = "Grafton",
  stplfips_2010 = "3369969",
  name_2010 = "Hanover city",
  city_source = "Me",
  population_source = "Google",
  place_type = "Incorporated City",
  year = as.integer(2010),
  population = as.integer(11225),
  geometry = st_sfc(st_point(c(-72.2896, 43.7022)), crs = 4326)
)

city_whanover <- rbind(city_data, hanover)
state_palette <- colorRampPalette(brewer.pal(3, "Dark2"))

# ADD HANOVER AND COLOR STUFF
city_whanover %>%
  ggplot() +
  geom_sf(data = state_data, aes(fill = stusps)) +
  geom_sf(aes(size = population)) +
  scale_fill_manual(values = state_palette(2)) +
  theme_minimal() +
  guides(size=guide_legend("Population"), fill = FALSE) +
  labs(
    title = "Cities in Vermont and New Hampshire",
    x = "Latitude",
    y = "Longitude"
  )

# 2.
# Couldn't get a match for Kosovo without loosing larger countries :/
wiid <- read_csv("hw2data/WIID_Dec2018.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")


gini_europe <- wiid %>%
  filter(
    year >= 2000,
    region_un == "Europe",
    country != "Russia"
  ) %>%
  mutate(adm0_a3_us = c3) %>%
  group_by(adm0_a3_us) %>%
  summarize(avgGini = mean(gini_reported, na.rm = TRUE))

geo_europe <- world %>%
  filter(
    region_un == "Europe",
    name != "Russia"
  ) %>%
  select(adm0_a3_us, geometry)

total_europe <- st_sf(inner_join(gini_europe, geo_europe))

total_europe %>%
  ggplot() +
  geom_sf(size = 0.2, aes(fill = avgGini)) +
  coord_sf(xlim = c(-10, 40), ylim = c(32, 71)) +
  theme_minimal() +
  labs(
    title = "Income Inequality in Europe in the 21st Century",
    fill = "Gini Coefficient",
    x = "Latitude",
    y = "Longitude"
  )

glimpse(world)
  

# 3.
louisiana_purchase <- us_states("1803-12-12")

louisiana_purchase %>%
  ggplot() +
  geom_sf(aes(fill = if_else(abbr_name == "LA Pur", "#33673B", "#9A6D38"))) +
  geom_sf_label(aes(label = abbr_name), size = 2) +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Land Increase from Louisiana Purchase",
    x = "Latitude",
    y = "Longitude"
  )


# 4.
trail_of_tears <- st_read("hw8_data/trail_of_tears/trte_nht_100k_line.shp")
tot_enddate_us <- us_states("1877-12-12")
state_palette <- colorRampPalette(brewer.pal(8, "Dark2"))

trail_of_tears %>%
  ggplot() +
  geom_sf(data = tot_enddate_us, aes(fill = id)) +
  geom_sf(color = "#8A0303") +
  coord_sf(xlim = c(-100, -72), ylim = c(27, 44)) +
  scale_fill_manual(values = state_palette(50)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Trail of Tears (1831 - 1877)",
    x = "Latitude",
    y = "Longitude"
  )

# 5.
fl_2016 <- read_csv("hw8_data/us_pres_elec_16_cty/US_County_Level_Presidential_Results_08-16.csv")
county_data <- us_counties()

fl_election <- fl_2016 %>%
  filter(str_detect(fips_code, "12[0-9]{3}")) %>%
  mutate(is_red = gop_2016 > dem_2016) %>%
  select(fips_code, is_red)

fl_counties <- county_data %>%
  filter(statefp == 12) %>%
  mutate(fips_code = geoid)

fl_total <- st_sf(inner_join(fl_election, fl_counties))


fl_total %>%
  filter(statefp == 12) %>%
  ggplot() +
  geom_sf(aes(fill = is_red)) +
  scale_fill_manual(
    values = c("#0015BC", "#FF0000"),
    labels = c("Democrat", "Republican")
  ) +
  theme_minimal() +
  theme(legend.position = c(0.3, 0.5)) +
  labs(
    title = "Florida 2016 Presidential Election Results",
    x = "Latitude",
    y = "Longitude",
    fill = "Party"
  )

