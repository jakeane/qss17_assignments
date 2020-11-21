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
library(gganimate)
library(ggimage)
library(rsvg)
library(RImagePalette)

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

# if_else(Industry == "Petroleum", "#aa0000", "#00aaaa")
for500 %>%
  mutate(
    revenue = as.numeric(str_match(`Revenue in USD`, "[0-9]{1,}")),
    petroleum = Industry == "Petroleum"
  ) %>%
  ggplot(aes(x = Company, y = revenue)) +
  geom_bar(
    stat = "identity",
    aes(fill = petroleum)
  ) +
  scale_fill_manual(
    values = c("#00aaaa", "#aa0000"),
    labels = c("Non-petroleum", "Petroleum")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    title = "Revenue of Top Fortune 500 Companies",
    y = "Revenue (Billions of USD)",
    fill = "Industry:"
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

# Legend position may be slightly off depending on window width
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

win_loss <- data.frame()
scor_off <- data.frame()

# Iterate through each page and append to df's
for(i in 1:7) {
  curr_wl <- read_html(paste("https://www.ncaa.com/stats/basketball-women/d1/current/team/169/p", i, sep = "")) %>%
    html_node("table") %>%
    html_table() %>%
    select(Team, Pct)
  win_loss <- rbind(win_loss, curr_wl)
  
  curr_so <- read_html(paste("https://www.ncaa.com/stats/basketball-women/d1/current/team/111/p", i, sep = "")) %>%
    html_node("table") %>%
    html_table() %>%
    select(Team, PPG)
  
  scor_off <- rbind(scor_off, curr_so)
}
scor_off

inner_join(win_loss, scor_off) %>%
  ggplot(aes(x = PPG, y = Pct)) +
  geom_point(color = "#000080", alpha = 0.8) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "How Scoring Relates to Winning",
    x = "Points per Game",
    y = "Winning Percentage"
  )
  
# Bonus

wl_time <- data.frame()
for(y in 0:8) {
  year <- ifelse(y != 8, 2012 + y, "current")
  curr_wl <- read_html(paste("https://www.ncaa.com/stats/basketball-women/d1/", year, "/team/169", sep = "")) %>%
    html_node("table") %>%
    html_table() %>%
    mutate(Year = 2012 + y) %>%
    select(Year, Team, Pct)
  
  curr_wl$Logo <- read_html(paste("https://www.ncaa.com/stats/basketball-women/d1/", year, "/team/169", sep = "")) %>%
    html_node("table") %>%
    html_nodes("img") %>%
    html_attr("src")
  
  wl_time <- rbind(wl_time, curr_wl)
}

# Account for some name changes
wl_time$Team <- recode(wl_time$Team, "Army West Point" = "Army")
wl_time$Team <- recode(wl_time$Team, "Connecticut" = "UConn")
wl_time$Logo <- recode(wl_time$Logo, "https://i.turner.ncaa.com/sites/default/files/images/logos/schools/bgl/army-west-point.svg" = "https://i.turner.ncaa.com/sites/default/files/images/logos/schools/bgl/army.svg")
wl_time$Logo <- recode(wl_time$Logo, "https://i.turner.ncaa.com/sites/default/files/images/logos/schools/bgl/connecticut.svg" = "https://i.turner.ncaa.com/sites/default/files/images/logos/schools/bgl/uconn.svg")

wl_time_final <- wl_time %>%
  group_by(Year) %>%
  top_n(10, Pct) %>%
  mutate(rank = rank(-Pct, ties.method = "random")) %>%
  filter(rank <= 10)

# Use average logo color as bar color
# See RImagePalette and rsvg for details
# The colors a kind of dark oh well
Color <- c()
for(i in 1:nrow(wl_time_final)) {
  if(i %% 10 == 0) {
    print(paste(i, nrow(wl_time_final), sep = "/"))
  }
  curr_color <- image_palette(rsvg(wl_time_final$Logo[i]), n=1)
  Color <- append(Color, curr_color)
}
wl_time_final$Color <- Color



win_anim <- wl_time_final %>%
  ggplot(aes(x = rank, y = Pct, fill = Color)) +
  geom_bar(stat = "identity") +
  geom_image(aes(image = Logo, y = -5)) +
  scale_fill_identity() +
  enter_fly(x_loc = -12) +
  exit_fly(x_loc = -12) +
  transition_states(
    Year,
    transition_length = 4,
    state_length = 1,
    wrap = FALSE
  ) +
  coord_flip() +
  scale_x_reverse() +
  ylim(c(-5, 101)) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Top NCAA Women's Basketball Teams in {closest_state}",
    y = "Win Percentage"
  )

animate(win_anim, duration = 20, fps = 10)
