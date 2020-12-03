## Data Visualization (GOVT16-QSS17) Fall 2020
## Final Exam
##
## Name: John Keane
## Date: 12/3/20


# Set up environment
library(tidyverse)
library(rvest)
library(sf)
library(USAboundaries)
library(USAboundariesData)


# 1.
base_eo_url <- "https://www.presidency.ucsb.edu/documents/app-categories/written-presidential-orders/presidential/executive-orders?items_per_page=60"

eo_vector <- c()

for (i in 0:100) {
  page_num <- if_else(i == 0, "", paste("&page=", i, sep = ""))
  if(i %% 10 == 0) {
    print(paste(i, 100, sep = "/"))
  }
  curr_dates <- read_html(paste(base_eo_url, page_num, sep = "")) %>%
    html_nodes("div.col-sm-8 > h4 > span") %>%
    html_text()
  eo_vector <- append(eo_vector, curr_dates)
}

as_tibble(eo_vector) %>%
  mutate(year = as.numeric(format(as.Date(value, format = "%B %d, %Y"), "%Y"))) %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Number of Orders",
    title = "Total Executive Orders by Year"
  )


# 2.

hot_stuff <- read_csv("dv_final_datasets/billboard_dat/hot_stuff.csv")
hot100_features <- read_csv("dv_final_datasets/billboard_dat/hot100_audio_features.csv")

min(hot100_features$danceability, na.rm = TRUE)

weeks_on_chart <- hot_stuff %>%
  mutate(year = as.numeric(format(as.Date(WeekID, format = "%m/%d/%Y"), "%Y"))) %>%
  group_by(SongID, year) %>%
  summarise(weeks_on_100 = max(`Weeks on Chart`))

dance <- hot100_features %>%
  filter(!is.na(danceability)) %>%
  mutate(dance_level = cut(danceability, seq(0, 1, 0.2), right = FALSE)) %>%
  select(SongID, dance_level)

hot100_danceability <- inner_join(weeks_on_chart, dance)

hot100_danceability %>%
  ggplot(aes(x = year, y = weeks_on_100)) +
  geom_point()


# 3.
australia <- st_read("dv_final_datasets/aus_spatial_dat/Aussie.shp")
copepods <- read_csv("dv_final_datasets/aus_spatial_dat/copepods_standardised.csv")

australia %>%
  ggplot() +
  geom_sf()

copepods %>%
  ggplot(aes(y = latitude, x = longitude)) +
  geom_point(color = "grey70", alpha=0.6) +
  stat_density_2d(aes(fill = ..level..), geom="polygon", alpha=0.8)


# 4.

ca_counties <- us_counties() %>%
  filter(state_abbr == "CA") %>%
  select(name, geometry)

ca_sat <- read_csv("dv_final_datasets/sat_2016/sat_2016.csv")

county_scores <- ca_sat %>%
  mutate(
    avgscrmath = as.numeric(avgscrmath),
    avgscrread = as.numeric(avgscrread),
    name = cname
  ) %>%
  group_by(name) %>%
  summarise(
    avgMath = mean(avgscrmath, na.rm = TRUE),
    avgRead = mean(avgscrread, na.rm = TRUE)
  )

st_sf(inner_join(county_scores, ca_counties)) %>%
  ggplot(aes(fill = avgMath)) +
  geom_sf()


# 5.

car_speed <- read_html("https://en.wikipedia.org/wiki/Production_car_speed_record") %>%
  html_node("table") %>%
  html_table()


glimpse(car_speed)


# 6.

geese <- read_csv("dv_final_datasets/geese_problem/geese.csv")

geese %>%
  ggplot(aes(x = `location-long`, y = `location-lat`)) +
  geom_point()


# 7.

us_climate <- st_read("dv_final_datasets/us_climate/US_Climate.shp")


us_climate %>%
  ggplot(aes(color = T01)) +
  geom_sf()


# 8.

wiid <- read_csv("hw2data/WIID_Dec2018.csv")


# 9.

read_html("https://www.espn.com/nba/stats/team/_/table/offensive/sort/avgPoints/dir/desc")
read_html("https://www.espn.com/nba/stats/team/_/view/opponent/table/offensive/sort/avgPoints/dir/desc")
read_html("https://www.espn.com/nba/standings/_/group/league")

# 10.

black_kites <- read_csv("dv_final_datasets/black_kites/black_kites.csv")
