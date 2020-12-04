## Data Visualization (GOVT16-QSS17) Fall 2020
## Final Exam
##
## Name: John Keane
## Date: 12/3/20


# Set up environment
library(tidyverse)
library(gganimate)
library(rvest)
library(sf)
library(USAboundaries)
library(USAboundariesData)
library(gridExtra)
library(grid)
library(ggrepel)
library(rworldmap)
library(rworldxtra)

# Set up output directory
if (!file.exists("final_figures")) {
  dir.create("final_figures")
}


# 1.
base_eo_url <- "https://www.presidency.ucsb.edu/documents/app-categories/written-presidential-orders/presidential/executive-orders?items_per_page=60"

eo_vector <- c()

# Scrape data
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

# Show data
plot_1 <- as_tibble(eo_vector) %>%
  mutate(year = as.numeric(format(as.Date(value, format = "%B %d, %Y"), "%Y"))) %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Number of Orders",
    title = "Total Executive Orders by Year"
  ) +
  transition_reveal(along = year)

# "Preview" app does not play gif, just shows frames individually
# Needs to be opened in Chrome
anim_save(
  "final_figures/plot_1.png",
  animate(plot_1, width = 800, height = 400, duration = 20, fps = 20)
)

anim_save(
  "final_figures/plot_1.gif",
  animate(plot_1, width = 800, height = 400, duration = 20, fps = 20)
)



# 2.

hot_stuff <- read_csv("dv_final_datasets/billboard_dat/hot_stuff.csv")
hot100_features <- read_csv("dv_final_datasets/billboard_dat/hot100_audio_features.csv")

hot_base <- hot_stuff %>%
  mutate(
    year = as.numeric(format(as.Date(WeekID, format = "%m/%d/%Y"), "%Y")),
    month = as.numeric(format(as.Date(WeekID, format = "%m/%d/%Y"), "%m"))
  ) %>%
  select(year, month, SongID, `Weeks on Chart`)

dance_base <- hot100_features %>%
  select(SongID, danceability)

plot_2 <- inner_join(hot_base, dance_base) %>%
  filter(!is.na(danceability)) %>%
  mutate(
    dance_level = cut(danceability, seq(0, 1, 0.2), right = FALSE)
  ) %>%
  group_by(year, month, dance_level) %>%
  summarise(
    weeks_on_100 = mean(`Weeks on Chart`)
  ) %>%
  ggplot(aes(x = year, y = weeks_on_100, color = dance_level)) +
  geom_point(alpha = 0.5) +
  geom_smooth(span = 1.25) +
  theme_minimal() +
  scale_color_manual(
    labels = c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5"),
    values = c("#580afb", "#20a400", "#c6c200", "#eb8a02", "#d32700"),
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = "Slow Songs No More! Billboard Top 100: 1958-2019",
    x = "Year",
    y = "Average Weeks on Billboard 100",
    color = "Danceability"
  )
  

  
ggsave(plot_2, width = 10, height = 5, filename = "final_figures/plot_2.pdf")
ggsave(plot_2, width = 10, height = 5, filename = "final_figures/plot_2.png")

# 3.
australia <- st_read("dv_final_datasets/aus_spatial_dat/Aussie.shp")
copepods <- read_csv("dv_final_datasets/aus_spatial_dat/copepods_standardised.csv")

plot_3 <- australia %>%
  ggplot() +
  stat_density_2d_filled(
    data = copepods,
    aes(
      y = latitude,
      x = longitude,
      alpha = ..level..
    ),
    fill = "#3185ec"
  ) +
  scale_alpha_manual(values = seq(0, 1, 0.09)) +
  geom_sf(fill = "#065800", color = "#3a8306", size = 0.35) +
  geom_sf_text(aes(label = adm), color = "#52a01e", size = 3) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#dadef2"),
    panel.grid = element_line(color = "grey93")
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Australian Copepods",
    subtitle = "Plankton and Crustacean Concentrations"
  )

ggsave(plot_3, width = 10, height = 5, filename = "final_figures/plot_3.pdf")
ggsave(plot_3, width = 10, height = 5, filename = "final_figures/plot_3.png")


# 4.

# Math #1900ae -> #f59700
# Read #1c00b0 -> #62f180

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


ca_math <- st_sf(right_join(county_scores, ca_counties)) %>%
  ggplot(aes(fill = avgMath)) +
  geom_sf(color = alpha("black", 0.01)) +
  scale_fill_gradient(
    high = "#1900ae",
    low = "#f59700",
    limits = c(440, 578)
  ) +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Average Math SAT Scores",
    fill = "Score"
  )

ca_read <- st_sf(right_join(county_scores, ca_counties)) %>%
  ggplot(aes(fill = avgRead)) +
  geom_sf(color = alpha("black", 0.01)) +
  scale_fill_gradient(
    high = "#1c00b0",
    low = "#62f180",
    limits = c(440, 557)
  ) +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Average Reading SAT Scores",
    fill = "Score",
    caption = "Source: California Department of Education"
  )


plot_4 <- grid.arrange(
  ca_math,
  ca_read,
  nrow = 1,
  top = "California 2015-2016 SAT Scores by County"
)


ggsave(plot_4, width = 10, height = 5, filename = "final_figures/plot_4.pdf")
ggsave(plot_4, width = 10, height = 5, filename = "final_figures/plot_4.png")


# 5.

car_speed <- read_html("https://en.wikipedia.org/wiki/Production_car_speed_record") %>%
  html_node("table") %>%
  html_table()

plot_5 <- car_speed %>%
  mutate(kmh = str_match(`Top speed of production car`, "\\d+(\\.\\d+)?")[,1])  %>%
  select(`Make and model`, Year, kmh) %>%
  ggplot(aes(x = Year, y = as.numeric(kmh), label = `Make and model`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel() +
  theme_minimal() +
  labs(
    y = "Speed in Kilometers Per Hour (KPH)",
    title = "Street Legal Car Speed Records",
    caption = "Sources: https://en.wikipedia.org/wiki/Production_car_speed_record"
  )

ggsave(plot_5, width = 10, height = 5, filename = "final_figures/plot_5.pdf")
ggsave(plot_5, width = 10, height = 5, filename = "final_figures/plot_5.png")

# 6.

geese <- read_csv("dv_final_datasets/geese_problem/geese.csv")

land <- st_read("dv_final_datasets/geese_problem/natural_usa/ne_10m_land/ne_10m_land.shp")
lakes <- st_read("dv_final_datasets/geese_problem/natural_usa/ne_10m_lakes/ne_10m_lakes.shp")
rivers <- st_read("dv_final_datasets/geese_problem/natural_usa/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")

plot_6 <- geese %>%
  ggplot() +
  geom_sf(data = land, color = "black", size = 0.25) +
  geom_sf(data = lakes, fill = "#6491f2", color = "#6491f2") +
  geom_sf(data = rivers, color = "#6491f2", size = 0.25) +
  geom_point(
    aes(x = `location-long`, y = `location-lat`),
    color = "#fe3f41",
    size = 0.05
  ) +
  coord_sf(xlim = c(0, 85), ylim = c(36.5, 80)) +
  theme_minimal() +
  theme(aspect.ratio = 0.75) +
  labs(
    title = "White-Fronted Geese in Europe and Northern Asia",
    subtitle = "Observations of Migrating Geese in Red",
    x = "Longitude",
    y = "Latitude"
  )

ggsave(plot_6, width = 10, height = 5, filename = "final_figures/plot_6.pdf")
ggsave(plot_6, width = 10, height = 5, filename = "final_figures/plot_6.png")


# 7.

states <- us_states()

us_climate <- st_read("dv_final_datasets/us_climate/US_Climate.shp")

june_palette <- colorRampPalette(c("#fd9100", "#e1e100", "#18a400", "#2a277c"))
jan_palette <- colorRampPalette(c("#d8dd00", "#30a900", "#2a287b", "#24e1f4"))


june <- us_climate %>%
  ggplot() +
  geom_sf(
    data = states,
    fill = "grey60",
    color = "grey70",
    size = 0.3
  ) +
  geom_sf(aes(color = T06), size = 0.65) +
  scale_x_continuous(
    limits = c(-125, -65),
    breaks = c(-120, -100, -80),
    labels = c("-120", "-100", "-80")
  ) +
  scale_y_continuous(
    limits = c(25, 50),
    breaks = c(30, 40, 50),
    labels = c("30", "40", "50")
  ) +
  scale_color_gradientn(colors = rev(june_palette(4))) +
  theme_minimal() +
  theme(axis.ticks = element_line(color = "black")) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "U.S. Climate Station Data",
    subtitle = "June Temperatures",
    color = "Degrees"
  ) 

january <- us_climate %>%
  ggplot() +
  geom_sf(
    data = states,
    fill = "grey60",
    color = "grey70",
    size = 0.3
  ) +
  geom_sf(aes(color = T01), size = 0.65) +
  scale_x_continuous(
    limits = c(-125, -65),
    breaks = c(-120, -100, -80),
    labels = c("-120", "-100", "-80")
  ) +
  scale_y_continuous(
    limits = c(25, 50),
    breaks = c(30, 40, 50),
    labels = c("30", "40", "50")
  ) +
  scale_color_gradientn(colors = rev(jan_palette(4))) +
  theme_minimal() +
  theme(axis.ticks = element_line(color = "black")) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "U.S. Climate Station Data",
    subtitle = "January Temperatures",
    color = "Degrees",
    caption = "Source:"
  ) 

plot_7 <- grid.arrange(
  june,
  january,
  nrow = 1
)

ggsave(plot_7, width = 10, height = 5, filename = "final_figures/plot_7.pdf")
ggsave(plot_7, width = 10, height = 5, filename = "final_figures/plot_7.png")


# 8.

wiid <- read_csv("hw2data/WIID_Dec2018.csv")

americas <- wiid %>%
  filter(year > 1980) %>%
  mutate(global_mean = mean(gini_reported, na.rm = TRUE)) %>%
  group_by(country) %>%
  filter(region_un == "Americas") %>%
  summarize(rel_gini = mean(gini_reported - global_mean, na.rm = TRUE)) %>%
  select(country, rel_gini) %>%
  ggplot(aes(x = reorder(country, rel_gini), y = rel_gini)) +
  geom_bar(stat = "identity", fill = "#246d0c") +
  coord_flip() +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(size = 5.5)
  ) +
  labs(
    title = "Americas Gini Index Scores",
    caption = "Source: World Income Inequality Database\nNote: Values are centered by subtracting the global mean since 1980."
  )

asia <- wiid %>%
  filter(year > 1980) %>%
  mutate(global_mean = mean(gini_reported, na.rm = TRUE)) %>%
  group_by(country) %>%
  filter(region_un == "Asia") %>%
  summarize(rel_gini = mean(gini_reported - global_mean, na.rm = TRUE)) %>%
  select(country, rel_gini) %>%
  ggplot(aes(x = reorder(country, rel_gini), y = rel_gini)) +
  geom_bar(stat = "identity", fill = "#6778c0") +
  coord_flip() +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(size = 5.5)
  ) +
  labs(
    title = "Asian Gini Index Scores"
  )

plot_8 <- grid.arrange(
  asia,
  americas,
  top = "Centered Gini Index Scores by Region",
  nrow = 1
)

ggsave(plot_8, width = 8, height = 6, filename = "final_figures/plot_8.pdf")
ggsave(plot_8, width = 8, height = 6, filename = "final_figures/plot_8.png")

  
# 9.

ppg <- read_html("https://www.espn.com/nba/stats/team/_/table/offensive/sort/avgPoints/dir/desc") %>%
  html_nodes("table") %>%
  html_table()

opg <- read_html("https://www.espn.com/nba/stats/team/_/view/opponent/table/offensive/sort/avgPoints/dir/desc") %>%
  html_nodes("table") %>%
  html_table()

wpct <- read_html("https://www.espn.com/nba/standings/_/group/league") %>%
  html_nodes("table") %>%
  html_table()

wpct_teams <- read_html("https://www.espn.com/nba/standings/_/group/league") %>%
  html_node("table") %>%
  html_nodes("span.hide-mobile > a.AnchorLink") %>%
  html_text()

wpct[[2]]$Team <- wpct_teams


ppg_table <- cbind(ppg[[1]], ppg[[2]]) %>%
  mutate(ppg = PTS) %>%
  select(Team, ppg)


opg_table <- cbind(opg[[1]], opg[[2]]) %>%
  mutate(opg = PTS) %>%
  select(Team, opg)

wpct_table <- wpct[[2]] %>%
  select(Team, PCT)

oppg_table <- inner_join(ppg_table, opg_table)

all_teams <- inner_join(oppg_table, wpct_table)

offense <- all_teams %>%
  ggplot(aes(x = ppg, y = PCT)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  labs(
    title = "Does Great Offense Win More Games in the NBA?",
    x = "Points Per Game",
    y = "Win Percentage"
  )

defense <- all_teams %>%
  ggplot(aes(x = opg, y = PCT)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  labs(
    title = "Or Does Great Defense?",
    x = "Points Per Game",
    y = "Win Percentage",
    caption = "Source: https://www.espn.com/nba/stats"
  )

plot_9 <- grid.arrange(
  offense,
  defense,
  nrow = 1
)

ggsave(plot_9, width = 10, height = 6, filename = "final_figures/plot_9.pdf")
ggsave(plot_9, width = 10, height = 6, filename = "final_figures/plot_9.png")



# 10.

black_kites <- read_csv("dv_final_datasets/black_kites/black_kites.csv")
bk_ts <- read_csv(
  "dv_final_datasets/black_kites/Black",
  col_types = cols(
    `tag-local-identifier` = col_character(),
    `individual-local-identifier` = col_character()
  )
)

world <- st_as_sf(getMap(resolution = "high"))

class(world)


plot_10 <- world %>%
  ggplot() +
  geom_sf() +
  geom_point(
    data = filter(bk_ts, `tag-local-identifier` == "kite10"),
    aes(x = `location-long`, y = `location-lat`),
    color = "blue",
    size = 0.1
  ) +
  coord_sf(xlim = c(-6.3, -5), ylim = c(35.75, 36.25)) +
  theme_minimal() +
  annotate(
    "text",
    label = "SPAIN",
    x = -5.7,
    y = 36.2,
    color = "grey40",
    size = 2.8,
    fontface = "italic"
  ) +
  annotate(
    "text",
    label = "MOROCCO",
    x = -5.6,
    y = 35.8,
    color = "grey40",
    size = 2.8,
    fontface = "italic"
  ) +
  annotate(
    "text",
    label = "Strait of Gibraltar",
    x = -5.8,
    y = 35.9,
    color = "grey40",
    size = 2,
    fontface = "italic",
    hjust = 0
  ) +
  annotate(
    "rect",
    xmin = -5.45,
    xmax = -5.05,
    ymin = 35.95,
    ymax = 36.05,
    fill = "grey70",
    alpha = 0.5
  ) +
  annotate(
    "text",
    label = "Black Kites are migratory and spend\nthe winter in Africa south of the Sahara.\nSee Kite 10's summer flight in blue.",
    x = -5.25,
    y = 36,
    color = "grey40",
    size = 2.5,
    fontface = "italic"
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "European Black Kites \"Summer\" in the Strait of Gibraltar"
  )

ggsave(plot_10, width = 10, height = 6, filename = "final_figures/plot_10.pdf")
ggsave(plot_10, width = 10, height = 6, filename = "final_figures/plot_10.png")
 
