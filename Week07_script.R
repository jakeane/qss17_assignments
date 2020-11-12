## Data Visualization (GOVT16-QSS17) Fall 2020
## Ggplot 2, Part III
##
## Name: John Keane
## Date: 11/3/20

# Setup environment
library(magick)
library(tidyverse)
library(gganimate)
library(gifski)
library(RColorBrewer)

# 1.
voted_yet_url <- "https://media.giphy.com/media/7HWA5HEboTAiuibisj/giphy.gif"
thank_you_url <- "https://media.giphy.com/media/26gsjCZpPolPr3sBy/giphy.gif"

voted_yet <- image_read(voted_yet_url)
thank_you <- image_read(thank_you_url)

# voted_yet
# thank_you

# Rescale to make dimensions identical
thank_you_scaled <- image_scale(thank_you, "x480")

# Annotated frame
voted_yet_annotated <- image_annotate(
  voted_yet[25],
  "You sure?",
  size = 50,
  gravity = "southeast",
  location = "+100+200",
  color = "white",
  strokecolor = "black",
  weight = 600,
)
voted_yet_annotated

# Slow down first gif
voted_yet_slowed <- voted_yet[c(
                          1:16,
                          rep(17:20, each = 2),
                          rep(21:24, each = 4)
                        )]

# Stitch gifs together
have_you <- c(
          voted_yet_slowed,
          voted_yet_annotated[rep(1, each = 25)],
          thank_you_scaled
        )

# FINAL GIF
have_you


image_write_gif(have_you, "hw7gifs/have_you.gif")=


# 2.
wiid <- read_csv("hw2data/WIID_Dec2018.csv")

ramp_pallete <- colorRampPalette(c("#FF5666", "#690375"))

color_palette <- c(ramp_pallete(7), "#007FFF")

wiid %>%
  filter(region_un_sub == "Western Europe" | country == "United States", year >= 1999) %>%
  select(year, country, gini_reported) %>%
  group_by(country, year) %>%
  mutate(avgGini = mean(gini_reported)) %>%
  ggplot(aes(x = year, color = country)) +
  geom_path(aes(y = avgGini)) +
  geom_point(aes(y = avgGini), alpha = 0.3) +
  coord_cartesian(xlim = c(2000, 2017)) +
  scale_color_manual(values = color_palette) +
  transition_reveal(along = year) +
  shadow_wake(0.3, size = 2.5, colour = 'grey90') +
  theme_minimal() +
  labs(
    color = "Country",
    x = "Year",
    y = "Gini Coefficient",
    title = "Income Inequality in the Western World"
  )


# 3.
country_palette <- colorRampPalette(brewer.pal(9, "Set1"))

top_gdp <- wiid %>%
  filter(!is.na(gdp_ppp_pc_usd2011), !is.na(gini_reported), year <= 2016) %>%
  select(country, year, gdp_ppp_pc_usd2011, gini_reported) %>%
  group_by(country, year) %>%
  summarize(gdp_per_gini = mean(gdp_ppp_pc_usd2011 / gini_reported)) %>%
  ungroup() %>%
  group_by(year) %>%
  top_n(10, gdp_per_gini) %>%
  mutate(rank = rank(-gdp_per_gini)) %>%
  ggplot(aes(x = rank, y = gdp_per_gini, fill = country)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = country, y = -200), color = "black", hjust = 1) +
  scale_fill_manual(values = country_palette(22)) +
  enter_fly(x_loc = -11) +
  exit_fly(x_loc = -11) +
  transition_states(year, transition_length = 100, state_length = 1) +
  coord_flip() +
  scale_x_reverse() +
  ylim(c(-1000, 4500)) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Countries with Best GDP/Gini in {closest_state}",
    y = "GDP/Gini"
  )

animate(top_gdp, duration = 40, fps = 10)


# 4.
senate <- read_csv("hw7data/Sall_members.csv")
senate$party_code <- recode(senate$party_code, `100` = "D", `200` = "R", `328` = "I", .default = "O")

senate_time <- senate %>%
  select(congress, nominate_dim1, nominate_dim2, party_code) %>%
  filter(congress >= 93, !is.na(nominate_dim2)) %>%
  ggplot(
    aes(
      x = nominate_dim1,
      y = nominate_dim2,
      color = party_code,
      label = party_code
    )
  ) +
  geom_text() +
  scale_color_manual(values = c("#0015BC", "#0B6623", "#FD6A02", "#FF0000")) +
  transition_states(congress, transition_length = 5, state_length = 1) +
  ease_aes("cubic-in-out") +
  enter_fade() +
  exit_fade() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Ideologies of the {closest_state}th Congress",
    x = "Nominate Dimension 1",
    y = "Nominate Dimension 2"
  )
senate_time

animate(senate_time, duration = 50, fps = 10)

