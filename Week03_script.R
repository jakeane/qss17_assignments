## Data Visualization (GOVT16-QSS17) Fall 2020
## Ggplot2, Part I (Week 3)
##
## Name: John Keane
## Date: 10/5/20


# Load libraries
library(tidyverse)

# 1. First, let's work with the IMDB dataset

# (a)
movie <- read_csv("hw2data/movie_metadata.csv")
print("movie head:")
head(movie)
print("movie tail:")
tail(movie)

# (b)
ggplot(movie[!is.na(movie$color), ], aes(x = color, y = imdb_score)) +
    geom_point(position = "jitter") +
    labs(
        title = "IMDB Scores of Movies by Color",
        x = "Color",
        y = "IMDB Score"
    ) +
    theme_minimal()

# (c)
ggplot(movie[!is.na(movie$color), ], aes(x = imdb_score, fill = color)) +
    geom_histogram() +
    labs(
        title = "Distribution of IMDB Scores by Color",
        x = "IMDB Score",
        y = "Count",
        fill = "Color"
    ) +
    theme_minimal()

# (d)
ggplot(
    movie[!is.na(movie$color), ],
    aes(
        x = imdb_score,
        y = ..density..,
        fill = color
    )
) +
    geom_histogram(position = "identity", alpha = 0.6) +
    labs(
        title = "Distribution of IMDB Scores by Color",
        x = "IMDB Score",
        y = "Density",
        fill = "Color"
    ) +
    theme_minimal()


# 2. For this problem, load up the approval dataset
approve <- read_csv("hw2data/approval_data.csv")
print("approve head:")
head(approve)
print("approve tail:")
tail(approve)

# (a)
approval_types <- approve %>%
    mutate(yearQrt = year + (0.25 * (qrt - 1))) %>%
    select(yearQrt, econapp, fpapp) %>%
    pivot_longer(names_to = "type", values_to = "value", c(econapp, fpapp))
approval_types

# (b)
ggplot(approval_types, aes(x = yearQrt, y = value, color = type)) +
    geom_line() +
    labs(
        title = "Economic and Foreign Policy Approval of Executive Admin",
        x = "Year",
        y = "Approval Rating",
        color = "Approval Type"
    ) +
    scale_color_discrete(labels = c("Economic", "Foreign")) +
    theme_minimal()

# (c)
ggplot(approval_types, aes(x = yearQrt, y = value, color = type)) +
    geom_line(alpha = 0.4) +
    geom_smooth() +
    labs(
        title = "Economic and Foreign Policy Approval of Executive Admin",
        x = "Year",
        y = "Approval Rating",
        color = "Approval Type"
    ) +
    scale_color_discrete(labels = c("Economic", "Foreign")) +
    theme_minimal()

# (d) does approval rating need to be included? is geom smooth needed?
approve %>%
    mutate(yearQrt = year + (0.25 * (qrt - 1))) %>%
    select(yearQrt, qrtinfl, qrtunem) %>%
    pivot_longer(
        names_to = "type",
        values_to = "value",
        c(qrtinfl, qrtunem)
    ) %>%
    ggplot(aes(x = yearQrt, y = value, color = type)) +
    geom_line(alpha = 0.4) +
    geom_smooth() +
    labs(
        title = "Inflation and Employment Rates Over Time",
        x = "Year",
        y = "Rate",
        color = "Type"
    ) +
    scale_color_discrete(labels = c("Inflation", "Unemployment")) +
    theme_minimal()


# WIID dataset
wiid <- read_csv("hw2data/WIID_Dec2018.csv")
print("WIID head:")
head(wiid)
print("WIID tail:")
tail(wiid)

# (a)
wiid %>%
    filter(country %in% c("Germany", "France", "Italy", "Spain", "Norway")) %>%
    group_by(country) %>%
    summarize(avgGini = mean(gini_reported, na.rm = TRUE)) %>%
    ggplot(aes(x = country, y = avgGini, label = country)) +
    geom_point() +
    geom_text(vjust = -1.0) +
    theme_minimal() +
    labs(
        title = "Average Gini Coefficient by Country",
        x = "Country",
        y = "Average Gini Coefficient"
    )

# (b)
wiid %>%
    filter(region_un == "Asia") %>%
    ggplot(aes(x = gini_reported, y = ..density.., fill = region_un_sub)) +
    geom_density(alpha = 0.3) +
    theme_minimal() +
    labs(
        title = "Income Inequality in Asia",
        y = "Density",
        fill = "UN Sub-Region",
        x = "Gini Coefficient"
    )

# (c) Is this right?
wiid %>%
    filter(region_un == "Europe") %>%
    group_by(country) %>%
    summarize(meanGini = mean(gini_reported, na.rm = TRUE)) %>%
    ggplot(aes(
        y = fct_reorder(country, meanGini, min),
        x = meanGini, label = country
    )) +
    geom_point() +
    theme_minimal() +
    theme(
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 6)
    ) +
    labs(x = "Gini Coefficient", title = "Income Inequality in Europe")

# (d)
africa_gini <- wiid %>%
    filter(region_un == "Africa") %>%
    group_by(country) %>%
    summarize(avgGini = mean(gini_reported, na.rm = TRUE))

africa_gini[nrow(africa_gini) + 1, ] <- list("Average", mean(
    africa_gini$avgGini,
    na.rm = TRUE
))

ggplot(africa_gini, aes(x = country, y = avgGini, fill = country)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    coord_flip() +
    theme_minimal() +
    labs(
        title = "Income Inequality in Africa",
        y = "Gini Coefficient",
        x = "Country"
    )

# (e)
ggplot(wiid, aes(x = gini_reported, y = ..density.., fill = region_un)) +
    geom_histogram(alpha = 0.5) +
    theme_minimal() +
    labs(
        title = "Global Income Inequality",
        x = "Gini Coefficient",
        fill = "UN Region",
        y = "Density"
    )

# (f)
wiid %>%
    filter(region_un == "Americas") %>%
    group_by(region_un_sub, year) %>%
    summarize(medianGini = median(gini_reported, na.rm = TRUE)) %>%
    ggplot(aes(x = year, y = medianGini, color = region_un_sub)) +
    geom_point(alpha = 0.4) +
    geom_smooth() +
    theme_minimal() +
    labs(
        title = "Income Inequality in the Americas",
        x = "Year",
        y = "Gini Coefficient",
        color = "UN Sub-Region"
    )

# (g) stat_summary????
wiid %>%
    filter(region_un_sub == "Western Asia") %>%
    ggplot(aes(x = gini_reported, fill = incomegroup)) +
    geom_dotplot(position = "jitter", alpha = 0.6) +
    stat_summary(fun.data = "median") +
    theme_minimal()