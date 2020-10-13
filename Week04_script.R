## Data Visualization (GOVT16-QSS17) Fall 2020
## Working in Tidyverse
##
## Name: John Keane
## Date: 10/13/20

library(tidyverse)
library(skimr)

movie <- read_csv("hw2data/movie_metadata.csv")

# 1. Some Counting, Summarizing, and Filtering Options with your IMDB Movies Data

# (a)
skim(movie)

# (b)
movie %>%
  distinct(language)

# (c)
movie %>%
  count(language) %>%
  arrange(-n)

# (d)
movie %>%
  count(language, color) %>%
  arrange(-n)

# (e)
vector1 <- 1:10
vector1
vector2 <- c(1, 4, 11)
vector2

# (f)
vector1 %in% vector2
vector2 %in% vector1

# (g)
movie %>%
  filter(language %in% c("French", "Spanish", "Hindi", "Mandarin")) %>%
  ggplot(aes(language, fill = language)) +
  geom_bar() +
  labs(
    title = "Foreign Movies by Language",
    x = "Language",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )


# 2. Pulling Apart and Creating String/Character Variables

# (a)
# *downloads stringr cheatsheet*

# (b)
movie %>%
  count(genres) %>%
  arrange(-n)
# 914 genres????

# (c)
movie %>%
  mutate(action = str_detect(genres, "Action")) %>%
  count(action)

# (d)
movie %>%
  mutate(animation = str_detect(genres, "Animation")) %>%
  count(animation)

# (e)
color_pallete <- colorRampPalette(c("red", "blue"))

movie %>%
  mutate(
    comedy = str_detect(genres, "Comedy") & !str_detect(genres, "Drama"),
    drama = !str_detect(genres, "Comedy") & str_detect(genres, "Drama"),
    comedy_drama = str_detect(genres, "Comedy") & str_detect(genres, "Drama"),
  ) %>%
  pivot_longer(names_to = "comdram", values_to = "type", comedy:comedy_drama) %>%
  filter(type) %>%
  group_by(title_year, comdram) %>%
  summarize(avgRating = mean(imdb_score, na.rm = TRUE)) %>%
  ggplot(aes(title_year, avgRating, color = comdram)) +
  geom_smooth() +
  geom_point(alpha = 0.2) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(
    values = color_pallete(3),
    labels = c("Comedy", "Comedy/Drama", "Drama")) +
  labs(
    title = "IMDB Ratings Over Time by Genre",
    x = "Year",
    y = "Rating",
    color = "Genre"
  )


# 3. New IMDB Database

# (a)
imdb <- read_tsv(
  "hw4data/imdb.tsv",
  # Add \\N to accomodate empty values
  na = c("", " ", "NA", "N/A", "\\N"),
  # Reassert as numeric
  col_types = cols(
    endYear = col_number()
  ),
  # Change quote delimeter to allow quotes in df values
  quote = ""
)
head(imdb)
ratings <- read_tsv("hw4data/ratings.tsv")
head(ratings)

# (b)
imdb %>%
  count(genres) %>%
  arrange(-n)

# (c)
new_movies <- full_join(imdb, ratings, by = "tconst")
head(new_movies)

# (d)
new_movies %>%
  filter(is.na(averageRating)) %>%
  count(titleType) %>%
  arrange(-n)

# (e)
new_movies %>%
  filter(
    !is.na(averageRating),
    titleType %in% c("short", "movie")
  ) %>%
  group_by(titleType, startYear) %>%
  summarize(
    rating25 = quantile(averageRating, .25, na.rm = TRUE),
    rating50 = quantile(averageRating, .5, na.rm = TRUE),
    rating75 = quantile(averageRating, .75, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = startYear, color = titleType)) +
  geom_smooth(aes(y = rating50), span = 0.3) +
  geom_segment(
    aes(xend = startYear, y = rating25, yend = rating75),
    alpha = 0.4
  ) +
  scale_color_manual(
    values = c("#e1ad01", "#0000cd"),
    labels = c("Full-Length", "Short")
  ) +
  theme_classic() +
  theme(legend.position = c(0.85, 0.15)) +
  labs(
    title = "Movie Performaces Over Time (From IMDB Ratings)",
    y = "IMDB Rating",
    x = "Year",
    color = "Movie Type"
  )
