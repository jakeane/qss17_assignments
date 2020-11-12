## QSS 17
## John Keane

# Set up environment
library(tidyverse)
library(USAboundaries)
library(USAboundariesData)
library(gganimate)
library(sf)
library(transformr)
library(lubridate)
covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
county_pop <- read_csv("projectData/co-est2019-alldata.csv")
county_data <- us_counties()
state_data <- us_states()

county_gis <- county_data %>%
  mutate(fips = paste(statefp, countyfp, sep = "")) %>%
  select(fips, name, state_name, state_abbr, geometry)

county_popest <- county_pop %>%
  mutate(fips = paste(STATE, COUNTY, sep = "")) %>%
  select(fips, POPESTIMATE2019)

covid_agg <- covid %>%
  filter(!is.na(fips)) %>%
  select(date, fips, cases) %>%
  mutate(date = as.character(date)) %>%
  pivot_wider(
    names_from = "date",
    values_from = "cases"
  ) %>%
  replace_na(set_names(as.list(rep(0, length(.))), names(.))) %>%
  pivot_longer(names_to = "date", values_to = "cases", cols = `2020-01-21`:`2020-11-08`) %>%
  group_by(fips) %>%
  mutate(change = cases - lag(cases, n = 7, default = cases[1]))

combine_long <- inner_join(county_gis, covid_agg, by = "fips")
combine_pop <- inner_join(combine_long, county_popest, by = "fips") %>%
  mutate(change_rate = change / POPESTIMATE2019 * 100)


states <- state_data %>%
  filter(state_abbr %in% c("NY", "CT", "NJ", "PA", "MA", "VT", "NH", "DE", "MD"))

covid_ts <- combine_pop %>%
  filter(
    state_abbr %in% c("NY", "CT", "NJ", "PA", "MA", "VT", "NH", "DE", "MD"),
    date >= as.Date("2020-03-15"),
    date <= as.Date("2020-05-15"),
    as.numeric(as.Date(date) - as.Date("2020-03-01")) %% 3 == 0
  ) %>%
  ggplot() +
  geom_sf(aes(fill = change_rate), color = alpha("white", 0.01)) +
  geom_sf(data = states, fill = alpha("white", 0), size = 1.5) +
  coord_sf(xlim = c(-76.5, -71), ylim = c(38.5, 43)) +
  transition_states(date, transition_length = 10, state_length = 1) +
  scale_fill_viridis_c() +
  labs(
    title = "Effect of First COVID-19 Epicenter in US",
    subtitle = "Date: {closest_state}",
    caption = "NYT Coronavirus Data in the United States",
    fill = "% Increase of COVID-19"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(.75, .2)
  )

animate(covid_ts, duration = 30, fps = 5)

combine_pop %>%
  filter(
    state_abbr %in% c("NY", "CT", "NJ", "PA", "MA", "VT", "NH", "DE", "MD"),
    date == as.Date("2020-04-15"),
    as.numeric(as.Date(date) - as.Date("2020-03-01")) %% 3 == 0
  ) %>%
  ggplot() +
  geom_sf(aes(fill = change_rate), color = alpha("white", 0.01)) +
  geom_sf(data = states, fill = alpha("white", 0), size = 1.5) +
  coord_sf(xlim = c(-76.5, -71), ylim = c(38.5, 43)) +
  scale_fill_viridis_c() +
  labs(
    title = "Effect of First COVID-19 Epicenter in US",
    subtitle = "Date: {closest_state}",
    caption = "NYT Coronavirus Data in the United States",
    fill = "% Increase of COVID-19"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(.75, .2)
  )
  
 

covid_anim <- combine_pop %>%
  group_by(date) %>%
  top_n(10, change_rate) %>%
  mutate(rank = rank(-change_rate, ties.method = "random")) %>%
  filter(rank <= 10, date >= as.Date("2020-07-10")) %>%
  mutate(county_name = paste(name, state_abbr, sep = ", ")) %>%
  select(fips, date, change_rate, rank, county_name) %>%
  ggplot(aes(x = rank, y = change_rate, fill = fips)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = county_name, y = -0.2), color = "black", hjust = 1) +
  enter_fly(x_loc = -11) +
  exit_fly(x_loc = -11) +
  transition_states(date, transition_length = 100, state_length = 1) +
  coord_flip() +
  scale_x_reverse() +
  ylim(c(-1.5, 12)) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Breakouts during {closest_state}",
    y = "Change over past week"
  )

animate(covid_anim, duration = 80, fps = 10)

covid_rank <- combine_pop %>%
  group_by(date) %>%
  top_n(10, change_rate) %>%
  mutate(rank = rank(-change_rate, ties.method = "random")) %>%
  filter(rank <= 10, date >= as.Date("2020-07-10")) %>%
  mutate(county_name = paste(name, state_abbr, sep = ", ")) %>%
  select(fips, date, change_rate, rank, county_name)
  

sum(is.na(covid_rank$fips))
