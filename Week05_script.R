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

# Overview of Nightingale
glimpse(Nightingale)
str(Nightingale)

# Separate data by date
night_before <- filter(Nightingale, Date < as.POSIXct("1855-04-01"))
night_after <- filter(Nightingale, Date >= as.POSIXct("1855-04-01"))


night_right <- night_before %>%
  select(Date, Month, Disease.rate, Other.rate, Wounds.rate) %>%
  pivot_longer(
    names_to = "cause",
    values_to = "deaths",
    Disease.rate:Wounds.rate
  ) %>%
  # So geom_text only shows once
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
      y = deaths + sqrt(deaths) + 250,
      label = Month,
      angle = 105 + -30 * as.numeric(factor(Date))
    ),
  ) +
  scale_fill_manual(
    values = c("#a4cccb", "#444444", "#ffb3a7"),
    labels = c("Disease", "Other", "Wounds")
  ) +
  scale_y_sqrt(limits = c(0, 1350)) +
  coord_polar(start = 3*pi/2) +
  labs(
    title = "April 1854 to March 1855",
    fill = "Cause"
  ) +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
  )

night_left <- night_after %>%
  select(Date, Month, Disease.rate, Other.rate, Wounds.rate) %>%
  pivot_longer(
    names_to = "cause",
    values_to = "deaths",
    Disease.rate:Wounds.rate
  ) %>%
  # So geom_text only shows once
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
  scale_y_sqrt(limits = c(0, 1350)) +
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

# size of each "pie" seems dependent on view width
grid.arrange(
  low_income,
  high_income,
  widths = c(1, 1.4),
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
  geom_point(aes(alpha = ifelse(country == "Sweden", 1, 0.5))) +
  geom_smooth(
    span = 0.6,
    alpha = 0.23,
    aes(fill = ifelse(country == "Sweden", "#fecd00", "#bbbbbb"))
  ) +
  scale_alpha_identity() +
  scale_fill_identity() +
  scale_color_manual(values = c("#FFBD3E", "#FFA04A", "#FF7E52", "#FF6C63", "#006aa8")) +
  labs(
    color = "Country",
    x = "Year",
    y = "Gini Coefficient",
    title = "Income Inequality of Sweden Compared to other Nordic Countries"
  ) +
  theme_minimal()

# 4.
color_palette <- colorRampPalette(c("#0F0326", "#075973", "#9E0559"))

# How to change the 'count' on the legend?
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

# "background" points
all_obs <- wiid %>%
  filter(year >= 1940) %>%
  select(-region_un, -incomegroup)

wiid %>%
  filter(year >= 1940) %>%
  ggplot(aes(x = year, y = gini_reported)) +
  geom_point(data = all_obs, color = "grey60", alpha = 0.5, size = 0.8) +
  geom_point(aes(color = region_un)) +
  facet_wrap(~ region_un) +
  theme_classic() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Income Inequality Across the World",
    x = "Year",
    y = "Gini Coefficient"
  )

# First approach
# 
# wiid %>%
#   filter(year >= 1940) %>%
#   mutate(
#     africa = ifelse(region_un == "Africa", 1, 0),
#     americas = ifelse(region_un == "Americas", 1, 0),
#     asia = ifelse(region_un == "Asia", 1, 0),
#     europe = ifelse(region_un == "Europe", 1, 0),
#     oceania = ifelse(region_un == "Oceania", 1, 0),
#   ) %>%
#   pivot_longer(names_to = "region", values_to = "is_region", africa:oceania) %>%
#   ggplot(aes(x = year, y = gini_reported)) +
#   scale_color_identity() +
#   scale_alpha_identity() +
#   geom_point(size = 0.8, aes(alpha = 0.5, color = if_else(is_region == 0, "grey60", NULL))) +
#   geom_point(aes(color = if_else(is_region == 1, "red", NULL))) +
#   facet_wrap(~ region)

# 6.
wiid %>%
  filter(year >= 1940) %>%
  ggplot(aes(x = year, y = gini_reported)) +
  geom_point(data = all_obs, color = "grey60", alpha = 0.5, size = 0.8) +
  geom_point(aes(color = region_un)) +
  facet_grid(
    factor(
      incomegroup,
      levels = c(
        "High income",
        "Upper middle income",
        "Lower middle income",
        "Low income"
      )
    ) ~ region_un
  ) +
  theme_classic() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Income Inequality Across the World, by Income Group",
    x = "Year",
    y = "Gini Coefficient"
  )

# First approach
#
# wiid %>%
#   filter(year >= 1940) %>%
#   mutate(
#     africa = ifelse(region_un == "Africa", 1, 0),
#     americas = ifelse(region_un == "Americas", 1, 0),
#     asia = ifelse(region_un == "Asia", 1, 0),
#     europe = ifelse(region_un == "Europe", 1, 0),
#     oceania = ifelse(region_un == "Oceania", 1, 0),
#     low_income = ifelse(incomegroup == "Low income", 1, 0),
#     lowmid_income = ifelse(incomegroup == "Lower middle income", 1, 0),
#     upmid_income = ifelse(incomegroup == "Upper middle income", 1, 0),
#     high_income = ifelse(incomegroup == "High income", 1, 0)
#   ) %>%
#   pivot_longer(names_to = "region", values_to = "is_region", africa:oceania) %>%
#   pivot_longer(names_to = "income", values_to = "income_type", low_income:high_income) %>%
#   ggplot(aes(x = year, y = gini_reported)) +
#   scale_color_identity() +
#   scale_alpha_identity() +
#   geom_point(size = 0.8, aes(alpha = 0.5, color = if_else(is_region == 0 | income_type == 0, "grey50", NULL))) +
#   geom_point(aes(color = if_else(is_region == 1 & income_type == 1, "red", NULL))) +
#   facet_grid(income ~ region)
