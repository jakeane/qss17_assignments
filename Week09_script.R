## Data Visualization (GOVT16-QSS17) Fall 2020
## Data Scraping in R
##
## Name: John Keane
## Date: 11/18/20

# Set up environment
library(tidyverse)
library(rvest)
library(sf)
library(USAboundaries)
library(USAboundariesData)
library(RColorBrewer)

# 1.

read_html("https://en.wikipedia.org/wiki/Ivy_League") %>%
  html_node("table") %>%
  html_table() %>%
  subset("Ivy League" != "") %>%
  rename("key" = "Ivy League", "value" = "Ivy League.1") %>%
  filter(key != "")

ivy <- read_html("https://en.wikipedia.org/wiki/Ivy_League") %>%
  html_node("table.wikitable") %>%
  html_table()


ivy$Colors <- read_html("https://en.wikipedia.org/wiki/Ivy_League") %>%
  html_nodes("td:nth-child(8) span:nth-child(1)") %>%
  str_match("#[0-9A-F]{6}")

## OLD IMPLEMENTATION ##
  # # Returns a vector of NA's and style attributes
  # # Each "cluster" of attributes maps a schools colors
  # colors <- read_html("https://en.wikipedia.org/wiki/Ivy_League") %>%
  #     html_nodes("table.wikitable span") %>%
  #     html_attr(name = "style")
  # 
  # # Gets index of the first value in each cluster
  # first_color_ind <- which(!is.na(colors))[which(!is.na(colors)) %in% (which(is.na(colors)) + 1)]
  # 
  # # Get hexcode
  # ivy$Colors <- str_match(colors[first_color_ind], "#[0-9A-F]{6}")
  # 
## END OLD IMPLEMENTATION ##

ivy %>%
  ggplot(aes(x = Institution, y = as.numeric(gsub(",", "", Undergraduates)))) +
  geom_bar(stat = "identity", aes(fill = Institution)) +
  scale_fill_manual(values = ivy$Colors) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Ivy League Undergraduate Enrollment",
    y = "Number of Students"
  )

# 2.

for500 <- read_html("https://en.wikipedia.org/wiki/Fortune_Global_500") %>%
  html_node("table") %>%
  html_table()

for500 %>%
  mutate(revenue = as.numeric(str_match(`Revenue in USD`, "[0-9]{1,}"))) %>%
  ggplot(aes(x = Company, y = revenue)) +
  geom_bar(
    stat = "identity",
    aes(fill = if_else(Industry == "Petroleum", "#aa0000", "#00aaaa"))
  ) +
  scale_fill_identity() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Revenue of Top Fortune 500 Companies",
    y = "Revenue (Billions of USD)"
  )


# 3.

state_sf <- us_states() %>%
  select(name, geometry)

state_emissions <- read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_carbon_dioxide_emissions") %>%
  html_node("table") %>%
  html_table() %>%
  mutate(
    name = Jurisdiction,
    emissions = as.numeric(`CO2 emissions  per capita(in metric tons)`),
    emission_bin = cut(
      emissions,
      c(0, 10, 15, 20, 25, 30, 50, max(emissions, na.rm = TRUE) + 1),
      right = FALSE
    )
  ) %>%
  select(name, emissions, emission_bin)

state_data <- st_sf(inner_join(state_emissions, state_sf))

state_palette <- colorRampPalette(c("#ffcccc", "#ff0000", "#990000", "#330000"))

state_data %>%
  ggplot() +
  geom_sf(aes(fill = emission_bin)) +
  coord_sf(xlim = c(-123, -68), ylim = c(24, 50)) +
  scale_fill_manual(
    values = state_palette(7),
    labels = c('0-10', '10-15', '15-20', '20-25', '25-30', '30-50', '50+'),
    guide = guide_legend(reverse=TRUE)
  ) +
  theme_minimal() +
  theme(
    legend.key.size = unit(.5, "cm"),
    legend.title = element_text(size = 8),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.9, 0.2)
  ) +
  labs(
    title = "CO2 Emissions per Capita per State (2016)",
    fill = "Millions of metric tons"
  )


# 4.

win_loss <- read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/169") %>%
  html_node("table") %>%
  html_table() %>%
  select(Team, Pct)


scor_off <- read_html("https://www.ncaa.com/stats/basketball-women/d1/current/team/111") %>%
  html_node("table") %>%
  html_table() %>%
  select(Team, PPG)

inner_join(win_loss, scor_off) %>%
  ggplot(aes(x = PPG, y = Pct, color = Team)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "How Scoring Relates to Winning",
    x = "Points per Game",
    y = "Winning Percentage"
  )
  
# Can we download this?

