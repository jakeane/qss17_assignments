## Data Visualization (GOVT16-QSS17) Fall 2020
## Intro to Tidyverse, Chapters 1-3
##
## Name: John Keane
## Date: 9/27/20


# 1. Filter

# (a)
library(tidyverse)

# (b)
data("swiss")
print("swiss dataset:")
head(swiss)

# (c)
swiss <- as_tibble(swiss)
print("swiss tibble:")
head(swiss)

# (d)
print("swiss; agriculture less than 50%:")
swiss %>%
    filter(Agriculture < 50)

# (e)
print("swiss; cath above 50%; fert below 70%:")
swiss %>%
    filter(Catholic > 50, Fertility < 70)


# 2. Arrange

# (a)
print("swiss arranged by infant moratility:")
swiss %>%
    arrange(Infant.Mortality)

# (b)
print("swiss arranged by cath in desc order:")
swiss %>%
    arrange(-Catholic)

# (c)
print("swiss; cath below 50%; by fertility")
swiss %>%
    filter(Catholic < 50) %>%
    arrange(Fertility)


# 3. Mutate

# (a)
movie <- read_csv("hw2data/movie_metadata.csv")
print("movie dataset:")
head(movie)

# (b)
print("movie structure:")
str(movie)

# (c)
movie <- movie %>%
    mutate(budget2 = budget / 1000000)
print("head(movie) budget in millions")
movie$budget2[1:6]

# (d)
movie <- movie %>%
    mutate(length = duration / 60)
print("head(movie) length")
movie$length[1:6]

# (e)
first_spectre <- min(which(str_detect(movie$movie_title, "Spectre")))
last_skyfall <- max(which(str_detect(movie$movie_title, "Skyfall")))
movie[first_spectre:last_skyfall,]

# 4. Plots & ggplot2

# (a)
undat <- read_csv("hw2data/WIID_Dec2018.csv")
print("UN data head:")
head(undat)
print("UN data tail:")
tail(undat)

# (b)
undat %>%
    filter(country %in% c("Canada", "United States", "Mexico")) %>%
    ggplot(aes(x = year, y = gini_reported, color = country)) +
    geom_point() +
    labs(
        title = "4b. Gini Coefficent of North American Countries by Year",
        x = "Year",
        y = "Gini Coefficient",
        color = "Country"
    )
# There are different sources within each year reporting on the same statistics
# The same source may do multiple studies at different scales too

# (c)
undat %>%
    filter(country %in% c("Canada", "United States", "Mexico")) %>%
    ggplot(aes(x = year, y = gini_reported, color = country)) +
    geom_line() +
    labs(
        title = "4c. Gini Coefficent of North American Countries by Year",
        x = "Year",
        y = "Gini Coefficient",
        color = "Country"
    )
# It makes it even worse

# (d)
approval <- read_csv("hw2data/approval_data.csv")
print("approval head:")
head(approval)

# (e)
ggplot(approval, aes(x = year, y = approve)) +
    geom_line() +
    labs(
        title = "4e. Presidential Approval Rating by Year",
        x = "Year",
        y = "Approval Rating"
    )
# I'd argue that the issue is different as its not due to multiple sources
# Instead it is due to data being tracked quarterly

# (f)
ggplot(approval, aes(x = tt, y = approve)) +
    geom_line() +
    labs(
        title = "4f. Presidential Approval Rating by Quarter",
        x = "Year",
        y = "Approval Rating"
    )
# This does look more normal

# (g)
ggplot(approval, aes(x = tt)) +
    geom_line(aes(y = approve), color = "red") +
    geom_line(aes(y = favor), color = "blue") +
    labs(
        title = "4g. Presidential Approval and Favorability Rating by Quarter",
        x = "Year",
        y = "Approval/Favorability Rating"
    )

# (h)

# as one hot encoding was used to show current president
# we need to "collapse" the president columns into one variable
# this can be done with pivot_longer,
# where we can create a new columns called pres and flag
# this pres/flag pair will tell us whether or not
# the president was in office during that year
# then we can filter for years where the specified president was in office

# the scales are set to free for better viewability

approval %>%
    pivot_longer(bushjr:carter, names_to = "pres", values_to = "flag") %>%
    filter(flag == 1) %>%
    ggplot(aes(x = tt, y = approve)) +
    geom_line() +
    facet_grid(~ factor(pres, levels = c(
        "carter",
        "reagan",
        "bushsr",
        "clinton",
        "bushjr"
    )),
    scales = "free"
    ) +
    labs(
        title = "4h. Presidential Approval Rating by Quarter",
        x = "Year",
        y = "Approval Rating"
    )


# 5. Basic statistics and summaries with summarize

# (a)
undat %>%
    summarize(
        medGini = median(gini_reported, na.rm = TRUE),
        medQ1 = median(q1, na.rm = TRUE),
        medQ5 = median(q5, na.rm = TRUE)
    )

# (b)
undat %>%
    filter(year == 2005) %>%
    summarize(
        minGini = min(gini_reported, na.rm = TRUE),
        minQ1 = min(q1, na.rm = TRUE),
        minQ5 = min(q5, na.rm = TRUE)
    )

# (c)
undat %>%
    filter(year == 2000, region_un == "Africa") %>%
    summarize(
        maxGini = max(gini_reported, na.rm = TRUE),
        maxd1 = max(d1, na.rm = TRUE),
        maxd10 = max(d10, na.rm = TRUE)
    )

# (d)
undat %>%
    filter(year == 2000, country == "United States") %>%
    select(gini_reported)

# (e)
undat %>%
    filter(year == 2000, country == "United States") %>%
    summarize(meanGini = mean(gini_reported, na.rm = TRUE))

# (f)
undat %>%
    filter(region_un == "Africa") %>%
    group_by(region_un_sub) %>%
    summarize(
        medGini = median(gini_reported, na.rm = TRUE),
        stdGini = sd(gini_reported, na.rm = TRUE)
    )


# 6. Plots

# (a)
undat %>%
    group_by(region_un, year) %>%
    summarize(meanGini = mean(gini_reported, na.rm = TRUE)) %>%
    ggplot(aes(x = year, y = meanGini)) +
    geom_point() +
    facet_grid(~region_un) +
    labs(
        title = "6a. Gini Coefficient of Countries by Year and Region",
        x = "Year",
        y = "Gini Coefficient",
        color = "Region"
    )

# (b)
undat %>%
    group_by(oecd, year) %>%
    summarize(medGini = median(gini_reported)) %>%
    ggplot(aes(x = year, y = medGini, color = oecd)) +
    geom_line() +
    geom_smooth() +
    labs(
        title = "6b. Gini Coefficient of Countries by Year and OECD Status",
        x = "Year",
        y = "Gini Coefficient",
        color = "OECD Status"
    )

# (c)
undat %>%
    filter(year == 2005, region_un_sub == "Central Asia") %>%
    group_by(country) %>%
    summarize(maxGini = max(gini_reported, na.rm = TRUE)) %>%
    ggplot(aes(x = country, y = maxGini, fill = country)) +
    geom_bar(stat = "identity") +
    labs(
        title = "6c. Max Gini Coefficient of Central Asian Countries in 2005",
        x = "Country",
        y = "Gini Coefficient",
        fill = "Country"
    )

# (d)
undat %>%
    filter(year > 1945) %>%
    group_by(region_un, year) %>%
    summarize(medGini = median(gini_reported, na.rm = TRUE)) %>%
    ggplot(aes(x = year, y = medGini, fill = region_un)) +
    geom_bar(stat = "identity") +
    facet_wrap(~region_un) +
    labs(
        title = "6d. Gini Coefficient of Countries by Year and Region",
        x = "Year",
        y = "Gini Coefficient",
        fill = "Region"
    )

# (e)
undat %>%
    ggplot(aes(x = d10, fill = region_un)) +
    geom_histogram() +
    labs(
        title = "6e. Percentage of Income Held by Top 10% by Country",
        x = "Percentage of Income Held by Top 10%",
        y = "Count",
        fill = "Region"
    )

# (f)
undat %>%
    ggplot(aes(x = d10, y = ..density.., fill = region_un)) +
    geom_histogram(alpha = 0.6, position = "identity") +
    labs(
        title = "6f. Percentage of Income Held by Top 10% by Country",
        x = "Percentage of Income Held by Top 10%",
        y = "Count",
        fill = "Region"
    )

# (g) is this by country?
undat %>%
    ggplot(aes(x = region_un, y = q1)) +
    geom_boxplot() +
    labs(
        title = "6g. Percentage of Income Held by Bottom 25% by Country",
        x = "Region",
        y = "Percentage of Income Held by Bottom 25%"
    )

# (h)
undat %>%
    group_by(region_un, incomegroup, country) %>%
    ggplot(aes(
        x = factor(incomegroup, levels = c(
            "Low income",
            "Lower middle income",
            "Upper middle income",
            "High income"
        )),
        y = gini_reported
    )) +
    geom_boxplot() +
    facet_wrap(~region_un) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(
        title = "6h. Gini Coefficient by Country",
        x = "Country Income Level",
        y = "Gini Coefficient"
    )