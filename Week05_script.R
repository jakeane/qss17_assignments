## Data Visualization (GOVT16-QSS17) Fall 2020
## Ggplot2, Part II
##
## Name: John Keane
## Date: 10/19/20

library(tidyverse)
library(gridExtra)
library(grid)
library(hexbin)

# 1. Nightingale
library(HistData)
data("Nightingale")

# (a)
glimpse(Nightingale)
str(Nightingale)

# (b)
night_before <- filter(Nightingale, Date < as.POSIXct("1855-04-01"))
night_after <- filter(Nightingale, Date >= as.POSIXct("1855-04-01"))


night_right <- night_before %>%
  select(Date, Month, Disease.rate, Other.rate, Wounds.rate) %>%
  pivot_longer(
    names_to = "cause",
    values_to = "deaths",
    Disease.rate:Wounds.rate
  ) %>%
  mutate(Month = replace(Month, cause != "Disease.rate", "")) %>%
  ggplot(aes(
    x = factor(Date),
    y = deaths,
    fill = cause
  )) +
  geom_bar(
    alpha = 0.7,
    stat="identity",
    position = "identity",
    width = 1,
    color = "black"
  ) +
  geom_text(
    aes(
      y = deaths + sqrt(deaths) + 150,
      label = Month,
      angle = 105 + -30 * as.numeric(factor(Date))
    ),
  ) +
  scale_fill_manual(
    values = c("#a4cccb", "#444444", "#ffb3a7"),
    labels = c("Disease", "Other", "Wounds")
  ) +
  scale_y_sqrt(limits = c(0, 1250)) +
  coord_polar(start = 3*pi/2) +
  labs(
    title = "April 1855 to March 1856"
  ) +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

night_left <- night_after %>%
  select(Date, Month, Disease.rate, Other.rate, Wounds.rate) %>%
  pivot_longer(
    names_to = "cause",
    values_to = "deaths",
    Disease.rate:Wounds.rate
  ) %>%
  mutate(Month = replace(Month, cause != "Disease.rate", "")) %>%
  ggplot(aes(
    x = factor(Date),
    y = deaths,
    fill = cause
  )) +
  geom_bar(
    alpha = 0.7,
    stat="identity",
    position = "identity",
    width = 1,
    color = "black"
  ) +
  geom_text(
    aes(
      y = deaths + sqrt(deaths) + 175,
      label = Month,
      angle = 105 + -30 * as.numeric(factor(Date))
    ),
  ) +
  scale_fill_manual(
    values = c("#a4cccb", "#444444", "#ffb3a7"),
    labels = c("Disease", "Other", "Wounds")
  ) +
  scale_y_sqrt(limits = c(0, 1250)) +
  coord_polar(start = 3*pi/2) +
  labs(
    title = "April 1855 to March 1856"
  ) +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

grid.arrange(
  night_left,
  night_right,
  nrow = 1,
  top = textGrob(
    "Diagram of the Causes of Mortality in the Army in the East",
    gp=gpar(fontsize=20,font=3)
  )
)


# 2.
wiid <- read_csv("hw2data/WIID_Dec2018.csv")
library(scales)

high_income <- wiid %>%
  filter(incomegroup == "High income") %>%
  group_by(region_un) %>%
  count() %>%
  ungroup() %>%
  mutate(total_obs = sum(n)) %>%
  ggplot(aes(x = factor(total_obs), y = n, fill = region_un)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = percent(n/total_obs)), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#D0B17A", "#F2CEE6", "#2EC4B6", "#FF9F1C", "#E71D36")) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_blank()
  ) +
  labs(
    fill = "Region",
    title = "High Income"
  )

low_income <- wiid %>%
  filter(incomegroup == "Low income") %>%
  group_by(region_un) %>%
  count() %>%
  ungroup() %>%
  mutate(total_obs = sum(n)) %>%
  ggplot(aes(x = factor(total_obs), y = n, fill = region_un)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = percent(n/total_obs)), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#D0B17A", "#F2CEE6", "#2EC4B6")) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Low Income")

grid.arrange(
  low_income,
  high_income,
  nrow = 1,
  top = textGrob(
    "Proportion of High and Low Income Countries by Region",
    gp=gpar(fontsize=20,font=3)
  )
)


# 3.
wiid %>%
  filter(country %in% c("Denmark", "Finland", "Iceland", "Norway", "Sweden")) %>%
  group_by(year, country) %>%
  summarize(avgGini = mean(gini_reported)) %>%
  ggplot(aes(year, avgGini, color = country)) +
  geom_point(aes(alpha = ifelse(country == "Sweden", 1, 0.35))) +
  geom_smooth(aes(alpha = ifelse(country == "Sweden", 0.4, 0.05))) +
  scale_alpha_identity() +
  scale_color_manual(values = c("#F7B538", "#DB7C26", "#D8572A", "#C32F27", "#006aa8")) +
  labs(
    color = "Country",
    x = "Year",
    y = "Gini Coefficient",
    title = "Income inequality in the Nordic Countries"
  )

# 4.
color_palette <- colorRampPalette(c("#0F0326", "#075973", "#9E0559"))
color_palette(3)

wiid %>%
  ggplot(aes(x = gdp_ppp_pc_usd2011, y = gini_reported)) +
  geom_hex(bins = 50) +
  scale_fill_gradientn(colors = color_palette(3)) +
  scale_x_log10() +
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.84)
  ) +
  labs(
    x = "Gross Domestic Product at Purchasing Power Parity per Capita",
    y = "Gini Coefficient",
    title = "Income Inequality vs. Economy"
  )


# 5.
wiid %>%
  filter(year >= 1940) %>%
  mutate(
    africa = ifelse(region_un == "Africa", 1, 0),
    americas = ifelse(region_un == "Americas", 1, 0),
    asia = ifelse(region_un == "Asia", 1, 0),
    europe = ifelse(region_un == "Europe", 1, 0),
    oceania = ifelse(region_un == "Oceania", 1, 0),
  ) %>%
  pivot_longer(names_to = "region", values_to = "is_region", africa:oceania) %>%
  ggplot(aes(x = year, y = gini_reported)) +
  scale_color_identity() +
  scale_alpha_identity() +
  geom_point(size = 0.8, aes(alpha = 0.5, color = if_else(is_region == 0, "grey50", NULL))) +
  geom_point(aes(color = if_else(is_region == 1, "red", NULL))) +
  facet_wrap(~ region)

# 6.
wiid %>%
  filter(year >= 1940) %>%
  mutate(
    africa = ifelse(region_un == "Africa", 1, 0),
    americas = ifelse(region_un == "Americas", 1, 0),
    asia = ifelse(region_un == "Asia", 1, 0),
    europe = ifelse(region_un == "Europe", 1, 0),
    oceania = ifelse(region_un == "Oceania", 1, 0),
  ) %>%
  pivot_longer(names_to = "region", values_to = "is_region", africa:oceania) %>%
  ggplot(aes(x = year, y = gini_reported)) +
  scale_color_identity() +
  scale_alpha_identity() +
  geom_point(size = 0.8, aes(alpha = 0.5, color = if_else(is_region == 0, "grey50", NULL))) +
  geom_point(aes(color = if_else(is_region == 1, "red", NULL))) +
  facet_grid(incomegroup ~ region)
