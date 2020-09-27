## Data Visualization (GOVT16-QSS17) Fall 2020
## Intro to Tidyverse, Chapters 1-3
##
## Name: John Keane
## Date: 9/27/20


# 1. Filter

# (a)
library(tidyverse)
library(stringr)

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

# (e) how the fuuuuck do you compare strings with filter()
# movie %>%
#     filter(movie_title) %>%
#     select(movie_title)


# 4. Plots & ggplot2

# (a)
undat <- read_csv("hw2data/WIID_Dec2018.csv")
print("UN data head:")
head(undat)
print("UN data tail:")
tail(undat)

# (b) need to comment on oddness
undat %>%
    filter(country %in% c("Canada", "United States", "Mexico")) %>%
    ggplot(aes(x = year, y = gini_reported, color = country)) +
    geom_point()

# (c)
undat %>%
    filter(country %in% c("Canada", "United States", "Mexico")) %>%
    ggplot(aes(x = year, y = gini_reported, color = country)) +
    geom_line()
# It makes it even worse

# (d)
approval <- read_csv("hw2data/approval_data.csv")
print("approval head:")
head(approval)

# (e)
ggplot(approval, aes(x = year, y = approve)) +
    geom_line()

# (f)