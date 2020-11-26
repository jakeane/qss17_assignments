## Data Visualization (GOVT16-QSS17) Fall 2020
## Lab 3: Election Edition
##
## Name: John Keane
## Date: 11/26/20

# Set up environment
library(tidyverse)
library(ggridges)
library(lubridate)

# This file is massive (800mb compressed), so I will send it to you via Dropbox
tweets <- read_delim("lab3data/uselection_tweets.csv", ";")

# Not sure if these are used
library(zoo)
library(scales)

# Exploration

tweet_base <- tweets %>%
  mutate(day = as.Date(`Created-At`, format = "%m/%d/%y")) %>%
  filter(
    # Focus around election timeframe
    day >= as.Date("2020-10-01"),
    day <= as.Date("2020-11-10"),
    
    !is.na(`Scoring String`), # Has sentiment tags
    Language == "en", # Filter out non-Americans
    `Retweet-Count` >= 3, # Filter out spam/bot/unpopular tweets
    PartyName %in% c("Republicans", "Democrats") # Remove 'neither' and 'both'
  )

# Find most common sentiment tags
tags <- tweet_base %>%
  count(`Scoring String`) %>%
  arrange(-n)

# Get frequency of certain sentiment tags
# This is how I found frequently occuring emotional tags
tweet_base %>%
  filter(
    str_detect(`Scoring String`, "fraud")
  ) %>%
  ungroup() %>%
  count()

# Interesting tags
#  - fraud: heavy GOP, 44K results
#  - cheat: party mixed, 5K results
#  - steal: party mixed, 8K results
#  - won: party mixed, 80K results
#  - fake: party mixed, 7K results
#  - stole: republican heavy, 4K results
#  - absentee: Republican heavy, 5K results
#  - crim: Democrat heavy, 9K results
#  - law: Republican heavy, 12K results
#  - god: Republican lean, 8K results
#  - los: party mixed, 58K results
#  - happy: Republican heavy, 7K results
#  - sad: Party mixed, 4K results


# With a collection of happy and sad emotional sentiments found,
# I wanted to see which was more prevalent at a given period of time
# with respect to party. As in, there were more positive tweets than
# negative or vice versa. To account that some sentiments are stronger
# than others, tweets are weighted as such

happy_sent <- tweet_base %>%
  filter(
    # Focus on election week
    day >= as.Date("2020-11-01"),
    day <= as.Date("2020-11-09"),
    # Ensure tweets are strictly positive
    Negativity <= 0.1
  ) %>%
  mutate(
    # Create custom score to tweets
    # If sentiment tag is found, add its respective sentiment score
    happy = if_else(str_detect(`Scoring String`, "happy"), 0.69, 0),
    excit = if_else(str_detect(`Scoring String`, "excited"), 0.36, 0),
    glad = if_else(str_detect(`Scoring String`, "glad"), 0.51, 0),
    thrilled = if_else(str_detect(`Scoring String`, "thrilled"), 0.49, 0),
    joy = if_else(str_detect(`Scoring String`, "[^a-z]joy"), 0.72, 0),
    pleased = if_else(str_detect(`Scoring String`, "pleased"), 0.49, 0),
    pos_sent = happy + excit + glad + thrilled + joy + pleased,
    # Get day/hour of tweet
    time = parse_date_time(`Created-At`, "%m/%d/%y %H:%M %p"),
    timehour = as.POSIXct(gsub('.{5}$', '00:00 UTC', time))
  ) %>%
  group_by(timehour, PartyName) %>%
  summarise(pos = sum(pos_sent)) %>%
  # Have 9 hour rolling average of sentiment for smooth over variation
  group_by(PartyName) %>%
  mutate(ra_pos = rollmean(pos, k=6, fill=0, align="right"))

sad_sent <- tweet_base %>%
  filter(
    # Focus on election week
    day >= as.Date("2020-11-01"),
    day <= as.Date("2020-11-09"),
    # Ensure tweets are strictly negative
    Positivity <= 0.1
  ) %>%
  mutate(
    # Create custom score to tweets
    # If sentiment tag is found, add its respective sentiment score
    sad = if_else(str_detect(`Scoring String`, "sad"), 0.54, 0),
    upset = if_else(str_detect(`Scoring String`, "upset"), 0.41, 0),
    mad = if_else(str_detect(`Scoring String`, "mad"), 0.56, 0),
    worried = if_else(str_detect(`Scoring String`, "worried"), 0.31, 0),
    stressed = if_else(str_detect(`Scoring String`, "stressed"), 0.36, 0),
    anger = if_else(str_detect(`Scoring String`, "[^a-z]anger[^a-z]"), 0.69, 0),
    neg_sent = sad + upset + mad + worried + stressed + anger,
    # Get day/hour of tweet
    time = parse_date_time(`Created-At`, "%m/%d/%y %H:%M %p"),
    timehour = as.POSIXct(gsub('.{5}$', '00:00 UTC', time))
  ) %>%
  group_by(timehour, PartyName) %>%
  summarise(neg = sum(neg_sent)) %>%
  # Have 9 hour rolling average of sentiment for smooth over variation
  group_by(PartyName) %>%
  mutate(ra_neg = rollmean(neg, k=6, fill=0, align="right"))


election_emotions <- inner_join(happy_sent, sad_sent) %>%
  mutate(net = (ra_pos - ra_neg) / (ra_pos + ra_neg)) %>% # Determine net positivity/negativity in sentiment
  ggplot(aes(x = timehour, y = net, color = PartyName)) +
  geom_hline(yintercept = 0, color = "grey70", linetype = "solid") + # Show positivity/negativity border
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2020-11-03 19:00:00"), linetype = "dashed") + # Vote reports start
  geom_vline(xintercept = as.POSIXct("2020-11-04 8:17:00"), linetype = "dashed") + # Biden takes lead in WI(6:51) MI (9:17)
  geom_vline(xintercept = as.POSIXct("2020-11-06 6:41:00"), linetype = "dashed") + # Biden takes lead in GA (4:31) and PA (8:51) 
  geom_vline(xintercept = as.POSIXct("2020-11-07 9:24:00"), linetype = "dashed") + # Biden projected winner
  scale_color_manual(values = c("#0015BC", "#FF0000")) +
  ylim(c(-1, 1)) +
  coord_cartesian(xlim = c(as.POSIXct("2020-11-03 6:00:00"), as.POSIXct("2020-11-08 6:00:00"))) +
  scale_x_datetime( # Show time in 6 hour breaks, add day when it's a new day
    date_breaks = "6 hours",
    labels = function(timehour) { # Allows conditional formatting for datetime
      
      # If its midnight, show full date, otherwise just show hour
      date_format <- if_else(
        hour(timehour) == 0,
        "%b%e - %I %p",
        # I legitamately think there is an error in R's software
        # For some reason, at least one of the 'formats' neets to be blank ("")
        # Otherwise, an error will raise
        # So, I have the last entry be blank via this if_else()
        if_else(
          timehour <= as.POSIXct("2020-11-08 11:00:00"),
          "%I %p",
          "" # Add a datetime format here to see the error
        )
      )
      
      # Convert times to custom format
      formatted <- format(
        timehour,
        format = date_format
      )
      
      # Set last entry to proper format
      formatted[length(formatted) - 1] <- format(
        as.POSIXct(formatted[length(formatted) - 1]),
        format = "%I %p"
      )
      
      formatted # return
    }
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    plot.caption = element_text(color = "grey40"),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "in")
  ) +
  labs(
    title = "Emotional Sentiment During the 2020 Election by Political Party",
    subtitle = "Determined by election-related tweets",
    x = "Date",
    y = "Emotional Sentiment\n(6-hour rolling average)",
    color = "Party Affiliation",
    caption = "Source: IBRAHIM SABUNCU. (2020). \"USA Nov.2020 Election 20 Mil. Tweets (with Sentiment and Party Name Labels) Dataset.\""
  ) +
  annotate(
    "text",
    x = as.POSIXct("2020-11-03 12:00:00"),
    y = 0.18,
    label = "Vote reports\nbegin",
    size = 2.75
  ) +
  annotate(
    "curve",
    x = as.POSIXct("2020-11-03 12:00:00"),
    y = 0.27,
    xend = as.POSIXct("2020-11-03 17:00:00"),
    yend = 0.45,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    curvature = -0.35
  ) +
  annotate(
    "text",
    x = as.POSIXct("2020-11-04 17:00:00"),
    y = 0.57,
    label = "Biden takes leads\nin Wisconsin\nand Michigan",
    size = 2.75
  ) +
  annotate(
    "curve",
    x = as.POSIXct("2020-11-04 17:00:00"),
    y = 0.43,
    xend = as.POSIXct("2020-11-04 11:00:00"),
    yend = 0.25,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    curvature = -0.4
  ) +
  annotate(
    "text",
    x = as.POSIXct("2020-11-06 17:00:00"),
    y = -0.38,
    label = "Biden takes leads\n in Georgia and\nPennsylvania",
    size = 2.75
  ) +
  annotate(
    "curve",
    x = as.POSIXct("2020-11-06 17:00:00"),
    y = -0.25,
    xend = as.POSIXct("2020-11-06 8:00:00"),
    yend = -0.1,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    curvature = 0.45
  ) +
  annotate(
    "text",
    x = as.POSIXct("2020-11-07 17:30:00"),
    y = 0.42,
    label = "Biden projected\nelection winner",
    size = 2.75
  ) +
  annotate(
    "curve",
    x = as.POSIXct("2020-11-07 17:30:00"),
    y = 0.5,
    xend = as.POSIXct("2020-11-07 11:00:00"),
    yend = 0.65,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    curvature = 0.45
  ) +
  annotate(
    "text",
    x = as.POSIXct("2020-11-03 4:30:00"),
    y = 0.05,
    label = "Happy",
    size = 2.75,
    color = "grey60",
    hjust = 0.5
  ) +
  annotate(
    "text",
    x = as.POSIXct("2020-11-03 4:30:00"),
    y = -0.04,
    label = "Sad",
    size = 2.75,
    color = "grey60",
    hjust = 0.8
  ) +
  annotate(
    "text",
    x = as.POSIXct("2020-11-05 10:00:00"),
    y = -0.54,
    label = "No explicit events,\npotentially positive\nspeculation around\nbattleground states",
    size = 2.5,
    color = "grey50"
  ) +
  annotate(
    "curve",
    x = as.POSIXct("2020-11-05 10:00:00"),
    y = -0.37,
    xend = as.POSIXct("2020-11-05 3:00:00"),
    yend = -0.1,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    curvature = 0.34,
    color = "grey50"
  ) +
  annotate(
    "text",
    x = as.POSIXct("2020-11-08 4:30:00"),
    y = -0.55,
    label = "No explicit events,\npossibly related\nto voter fraud\n speculation",
    size = 2.5,
    color = "grey50"
  ) +
  annotate(
    "curve",
    x = as.POSIXct("2020-11-08 4:30:00"),
    y = -0.38,
    xend = as.POSIXct("2020-11-07 22:00:00"),
    yend = -0.2,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    curvature = 0.34,
    color = "grey50"
  )

if (!file.exists("lab3figures")) {
  dir.create("lab3figures")
}

ggsave(election_emotions, width = 9, height = 6, filename = "lab3figures/election_emotions.pdf")
ggsave(election_emotions, width = 9, height = 6, filename = "lab3figures/election_emotions.png")


############### OTHER EXPLORATION/ATTEMPTS ##################

tweet_octnov <- tweets %>%
  mutate(day = as.Date(`Created-At`, format = "%m/%d/%y")) %>%
  filter(
    # Focus around election timeframe
    day >= as.Date("2020-10-01"),
    !is.na(`Scoring String`), # Has sentiment tags
    Language == "en", # Filter out non-Americans
    `Retweet-Count` >= 3, # Filter out spam/bot/unpopular tweets
    PartyName %in% c("Republicans", "Democrats") # Remove 'neither' and 'both'
  )

fraud_tweets <- tweets_octnov %>%
  mutate(
    time = parse_date_time(`Created-At`, "%m/%d/%y %H:%M %p"),
    timehour = as.POSIXct(gsub('.{5}$', '00:00 UTC', time))
  ) %>%
  mutate(
    fraud = str_detect(`Scoring String`, "fraud"),
    cheat = str_detect(`Scoring String`, "cheat"),
    fake = str_detect(`Scoring String`, "fake"),
    stole = str_detect(`Scoring String`, "stole"),
    law = str_detect(`Scoring String`, "law"),
    won = str_detect(`Scoring String`, "won"),
    god = str_detect(`Scoring String`, "god"),
    happy = str_detect(`Scoring String`, "happy"),
    sad = str_detect(`Scoring String`, "sad")
  ) %>%
  group_by(day, PartyName) %>%
  summarise(
    total_fraud = sum(fraud),
    total_cheat = sum(cheat),
    total_fake = sum(fake),
    total_stole = sum(stole),
    total_law = sum(law),
    total_won = sum(won),
    total_god = sum(god),
    total_happy = sum(happy),
    total_sad = sum(sad)
  )

happy_tweets <- tweet_base %>%
  filter(
    day >= as.Date("2020-11-01"),
    day <= as.Date("2020-11-09"),
    str_detect(`Scoring String`, "happy") |
      str_detect(`Scoring String`, "excited") |
      str_detect(`Scoring String`, "glad") |
      #      str_detect(`Scoring String`, "amazing") |
      #      str_detect(`Scoring String`, "wow") |
      #      str_detect(`Scoring String`, "wonderful") |
      str_detect(`Scoring String`, "thrilled") |
      str_detect(`Scoring String`, "[^a-z]joy") |
      str_detect(`Scoring String`, "pleased"),
    Negativity <= 0.1
  ) %>%
  group_by(day, PartyName) %>%
  count() %>%
  rename(pos = n)

sum(happy_tweets$pos)

sad_tweets <- tweet_base %>%
  filter(
    day >= as.Date("2020-11-01"),
    day <= as.Date("2020-11-09"),
    str_detect(`Scoring String`, "sad") |
      str_detect(`Scoring String`, "upset") |
      str_detect(`Scoring String`, "mad") |
      #      str_detect(`Scoring String`, "bad") |
      str_detect(`Scoring String`, "worried") |
      str_detect(`Scoring String`, "stressed") |
      str_detect(`Scoring String`, "[^a-z]anger[^a-z]"),
    Positivity <= 0.1
  ) %>%
  group_by(day, PartyName) %>%
  count() %>%
  rename(neg = n)
  
  
inner_join(happy_sent, sad_sent) %>%
  mutate(net = (pos - neg) / (pos + neg)) %>%
  ggplot(aes(x = timehour, color = PartyName)) +
  geom_line(aes(y = net)) +
  geom_vline(xintercept = as.POSIXct("2020-11-03 19:00:00"), linetype = "dashed") + 
  geom_vline(xintercept = as.Date("2020-11-07"), linetype = "dashed")



inner_join(happy_tweets, sad_tweets) %>%
  mutate(net = (pos - neg) / (pos + neg), total = pos + neg) %>%
  ggplot(aes(x = day, color = PartyName)) +
  geom_line(aes(y = net)) +
  geom_vline(xintercept = as.Date("2020-11-03"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-11-07"), linetype = "dashed")


tweet_base %>%
  ungroup() %>%
  filter(
    day >= as.Date("2020-11-01"),
    day <= as.Date("2020-11-09")
  ) %>%
  count()


winning_tweets <- tweet_base %>%
  filter(
    day >= as.Date("2020-11-01"),
    day <= as.Date("2020-11-09"),
    str_detect(`Scoring String`, "win") | str_detect(`Scoring String`, "won") | str_detect(`Scoring String`, "wins"),
    Negativity <= 0.1
  ) %>%
  group_by(day, PartyName) %>%
  count()

fraud_tweets <- tweet_base %>%
  filter(
    day >= as.Date("2020-11-01"),
    day <= as.Date("2020-11-09"),
    str_detect(`Scoring String`, "fraud"),
    Positivity <= 0.1
  ) %>%
  group_by(day, PartyName) %>%
  count()

legal_tweets <- tweet_base %>%
  filter(
    day >= as.Date("2020-11-01"),
    day <= as.Date("2020-11-09"),
    str_detect(`Scoring String`, "legal") | str_detect(`Scoring String`, "law")
  ) %>%
  group_by(day, PartyName) %>%
  count()

emotion_tweets <- tweet_base %>%
  filter(
    day >= as.Date("2020-11-01"),
    day <= as.Date("2020-11-09"),
    str_detect(`Scoring String`, "proud") | str_detect(`Scoring String`, "love"),
    Negativity <= 0.1
  ) %>%
  group_by(day, PartyName) %>%
  count()



pos_tweets <- tweet_base %>%
  filter(
    day >= as.Date("2020-11-01"),
    day <= as.Date("2020-11-09"),
    Score >= 0.5,
    Negativity <= 0.1
  ) %>%
  group_by(day, PartyName) %>%
  count()

sum(pos_tweets$n)

neg_tweets <- tweet_base %>%
  filter(
    day >= as.Date("2020-11-01"),
    day <= as.Date("2020-11-09"),
    Score <= -0.5,
    Positivity <= 0.1
  ) %>%
  group_by(day, PartyName) %>%
  count()

happy_tweets %>%
  ggplot(aes(x = day, color = PartyName)) +
  geom_line(aes(y = pos)) +
  geom_vline(xintercept = as.Date("2020-11-03"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-11-07"), linetype = "dashed")

winning_tweets %>%
  ggplot(aes(x = day, color = PartyName)) +
  geom_line(aes(y = n)) +
  geom_vline(xintercept = as.Date("2020-11-03"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-11-07"), linetype = "dashed")

fraud_tweets %>%
  ggplot(aes(x = day, color = PartyName)) +
  geom_line(aes(y = n)) +
  geom_vline(xintercept = as.Date("2020-11-03"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-11-07"), linetype = "dashed")

legal_tweets %>%
  ggplot(aes(x = day, color = PartyName)) +
  geom_line(aes(y = n)) +
  geom_vline(xintercept = as.Date("2020-11-03"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-11-07"), linetype = "dashed")

emotion_tweets %>%
  ggplot(aes(x = day, color = PartyName)) +
  geom_line(aes(y = n)) +
  geom_vline(xintercept = as.Date("2020-11-03"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-11-07"), linetype = "dashed")

sad_tweets %>%
  ggplot(aes(x = day, color = PartyName)) +
  geom_line(aes(y = neg)) +
  geom_vline(xintercept = as.Date("2020-11-03"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-11-07"), linetype = "dashed")

pos_tweets %>%
  ggplot(aes(x = day, color = PartyName)) +
  geom_line(aes(y = n)) +
  geom_vline(xintercept = as.Date("2020-11-03"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-11-07"), linetype = "dashed")

neg_tweets %>%
  ggplot(aes(x = day, color = PartyName)) +
  geom_line(aes(y = n)) +
  geom_vline(xintercept = as.Date("2020-11-03"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-11-07"), linetype = "dashed")


election <- read_csv("https://raw.githubusercontent.com/alex/nyt-2020-election-scraper/master/battleground-state-changes.csv")

election %>%
  mutate(
    day = as.Date(timestamp),
    biden_vote = if_else(leading_candidate_name == "Biden", leading_candidate_votes, trailing_candidate_votes),
    trump_vote = if_else(leading_candidate_name == "Trump", leading_candidate_votes, trailing_candidate_votes)
  ) %>%
  group_by(day, state) %>%
  summarize(
    biden_votes = max(biden_vote),
    trump_votes = max(trump_vote),
    vote_total = biden_votes + trump_votes
  ) %>%
  mutate(
    biden_diff = biden_votes - trump_votes,
    biden_margin = biden_diff / vote_total
  ) %>%
  group_by(day) %>%
  summarize(day_margin = sum(biden_margin))
  


unique(tweets$PartyName)
unique(tweets$Language)



tweets_octnov %>%
  group_by(day, PartyName) %>%
  summarize(
    pos_sent = weighted.mean(Positivity, `Retweet-Count`, na.rm = TRUE),
    neg_sent = weighted.mean(Negativity, `Retweet-Count`, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = day, color = PartyName)) +
  geom_line(aes(y = pos_sent)) +
  geom_line(aes(y = -neg_sent))

part_sent <- tweets_octnov %>%
#  filter(day >= as.Date("2020-09-02")) %>%
  mutate(
    time = parse_date_time(`Created-At`, "%m/%d/%y %H:%M %p"),
    timehour = as.POSIXct(gsub('.{5}$', '00:00 UTC', time))
  ) %>%
  group_by(timehour, PartyName) %>%
  summarize(sent = weighted.mean(Score, `Retweet-Count`, na.rm = TRUE))

tweets_octnov %>%
  filter(
    day >= as.Date("2020-11-01"),
    day <= as.Date("2020-11-10")
  ) %>%
  mutate(
    time = parse_date_time(`Created-At`, "%m/%d/%y %H:%M %p")
  ) %>%
  ggplot(aes(x = time, y = Score, color = PartyName)) +
  geom_smooth()

part_sent %>%
  ggplot(aes(x = timehour, y = sent, color = PartyName)) +
  geom_smooth()


election %>%
  mutate(
    datehour = as.POSIXct(gsub('.{5}$', '00:00', timestamp)),
    date = as.Date(timestamp),
    biden_share = if_else(leading_candidate_name == "Biden", leading_candidate_partition, trailing_candidate_partition),
    biden_vote = if_else(leading_candidate_name == "Biden", leading_candidate_votes, trailing_candidate_votes),
    trump_vote = if_else(leading_candidate_name == "Trump", leading_candidate_votes, trailing_candidate_votes),
    vote_count = biden_vote + trump_vote
  ) %>%
  group_by(state) %>%
  mutate(
    biden_vote_total = sum(biden_vote),
    trump_vote_total = sum(trump_vote),
    total_biden_share = biden_vote_total / (biden_vote_total + trump_vote_total),
    relative_share = biden_share - total_biden_share
  ) %>%
  group_by(date) %>%
  mutate(
    day_rel_share = weighted.mean(relative_share, biden_vote + trump_vote)
  ) %>%
  ungroup() %>%
  filter(date <= as.Date("2020-11-20")) %>%
  ggplot(aes(x = relative_share, y = as.factor(date), fill = day_rel_share)) +
  geom_density_ridges(aes(height=..density.., weight = vote_count), stat = "density") +
  xlim(c(-1, 1))



totals <- election %>%
  mutate(
    biden_vote = if_else(leading_candidate_name == "Biden", leading_candidate_votes, trailing_candidate_votes),
    trump_vote = if_else(leading_candidate_name == "Trump", leading_candidate_votes, trailing_candidate_votes)
  ) %>%
  group_by(state) %>%
  mutate(
    biden_vote_total = sum(biden_vote),
    trump_vote_total = sum(trump_vote),
    biden_share = biden_vote_total / (biden_vote_total + trump_vote_total)
  ) %>%
  select(state, biden_vote_total, trump_vote_total)
  

dates <- day(election$timestamp)
unique(dates)


