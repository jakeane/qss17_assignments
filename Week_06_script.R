## Data Visualization (GOVT16-QSS17) Fall 2020
## Ggplot 2, Part III
##
## Name: John Keane
## Date: 10/27/20


# Set up environment
library(tidyverse)
library(ggridges)
library(waffle)
library(purrr)
wiid <- read_csv("hw2data/WIID_Dec2018.csv")


# 1.
color_palette <- colorRampPalette(c("#5b507a", "#1985a1", "#d0a3bf"))

wiid %>%
  ggplot(aes(gini_reported, gdp_ppp_pc_usd2011)) +
  geom_point(color = "grey70", alpha=0.6) +
  stat_density_2d(aes(fill = ..level..), geom="polygon", alpha=0.8) +
  scale_fill_gradientn(colors = color_palette(3)) +
  labs(
    x = "Gini Coefficient",
    y = "GDP",
    title = "Distribution of Income Inequality and GDP",
    fill = "Level"
  ) +
  theme_minimal()

# 2.
wiid %>%
  filter(region_un == "Europe") %>%
  pivot_longer(names_to = "decile", values_to = "income_dec", d1:d10) %>%
  mutate(
    decile = factor(
      decile, 
      levels = c(
        "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10"
      )
    )
  ) %>%
  ggplot(aes(x = income_dec, y = decile, fill = decile)) +
  geom_density_ridges(alpha = 0.8) +
  scale_fill_manual(values = color_palette(10)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    x = "Income (k)",
    y = "Decile",
    title = "Income Distribution in Europe"
  )

# 3.
# Kind of figured something out. Does not work well with tidyverse though
total_income <- wiid %>%
  filter(region_un == "Europe") %>%
  pivot_longer(names_to = "decile", values_to = "income_dec", d1:d10) %>%
  mutate(
    decile = factor(
      decile, 
      levels = c(
        "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10"
      )
    )
  ) %>%
  group_by(decile) %>%
  summarize(totalIncome = sum(income_dec, na.rm = TRUE))

income_waffle <- total_income$totalIncome

names(income_waffle) <- total_income$decile

waffle_palette <- colorRampPalette(c("#3F84E5", "#DA2C38", "#FFEEDB", "#2FBF71", "#153B50"))

waffle(
  income_waffle / 2470,
  rows = 10,
  size=0.6,
  colors = waffle_palette(10),
  title = "Share of Income by Income Decile",
  xlab = "1 Square = $2470"
)


  
# 4.
wiid %>%
  filter(region_un == "Africa") %>%
  ggplot(aes(x = region_un_sub, y = gini_reported, fill = region_un_sub)) +
  geom_violin() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    x = "UN Subregion",
    y = "Gini Coefficient",
    title = "Income Inequality Across Africa"
  )


# 5.
wiid_split <- wiid %>%
  group_by(region_un_sub) %>%
  group_split()

gini_mean <- function(data) {
  data %>%
    mutate(meanGini = mean(gini_reported, na.rm = TRUE))
}


wiid_split %>%
  # There's multiple map functions so just to ensure
  purrr::map(gini_mean) %>%
  bind_rows()  %>%
  mutate(meanGiniSubregion = meanGini) %>%
  group_by(region_un) %>%
  group_split() %>%
  purrr::map(gini_mean) %>%
  bind_rows() %>%
  mutate(meanGiniRegion = meanGini)  %>%
  # region_un inclusion is just for clarity
  group_by(region_un, region_un_sub) %>%
  # mean() just reduces multiple identical observations into 1
  summarize(giniDiff = mean(meanGiniSubregion - meanGiniRegion, na.rm = TRUE))
