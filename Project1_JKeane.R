## Data Visualization (GOVT16-QSS17) Fall 2020
## Lab 1: COVID Edition
##
## Name: John Keane
## Date: 10/26/20


# Set up environment
library(tidyverse)
convpoll <- read_csv("projectData/conventionPoll.csv")

# Explore data
glimpse(convpoll)

# I know this lab was supposed to be more COVID themed, but I found this
# investigation on the Republican Convention very interesting. I hope
# it makes a similar impression on you.

# I first want to see engagement in the convention based on opinion of main candidates
convpoll %>%
  filter(Q2b %in% c("Yes", "No"), Q1_1 != "Skipped") %>%
  group_by(Q1_1) %>%
  summarize(engagement = mean(Q2b == "Yes"),
            count = n())
# It seems that those who hold an unfavorable opinion of Trump were *more* likely to follow coverage
# However, what about actually watching it? People may have watched coverage simply to watch
# their preferred news source provide their opinion
convpoll %>%
  filter(Q2a != "Skipped", Q1_1 != "Skipped") %>%
  group_by(Q1_1) %>%
  summarize(watched = mean(Q2a %in% c("Very little", "Some of it", "A great deal")),
            count = n())
# As I assumed, people who favor Trump were more likely to actually watch the convention
# However, strong opinion on Trump did not seem to affect general engagement.
# Lets repeat the same process based on Biden's favorability
convpoll %>%
  filter(Q2b %in% c("Yes", "No"), Q1_2 != "Skipped") %>%
  group_by(Q1_2) %>%
  summarize(engagement = mean(Q2b == "Yes"),
            count = n())
# These results seem very similar to Trump's favorability, within margin of error
convpoll %>%
  filter(Q2a != "Skipped", Q1_2 != "Skipped") %>%
  group_by(Q1_2) %>%
  summarize(watched = mean(Q2a %in% c("Very little", "Some of it", "A great deal")),
            count = n())
# In this case, it seems the main descrepency is that more people don't know their opinion on Biden
# Makes sense as he was not president the past 4 years


# There seems to be varying levels of engagement when it comes to watching the conventions.
# Some party lines can be drawn here, and that shows in the difference in actually watching.
# Watching the convention as opposed to just watching news coverage could lead to different
# views on the convention. News coverage could show merely the highlights, which can cause
# a different impact from watching an event in its entirity.

conv_approve <- convpoll %>%
  filter(
    QPID %in% c("A Democrat", "An Independent", "A Republican"),
    Q2a != "Skipped",
    Q2b == "Yes" || Q2a %in% c("Very little", "Some of it", "A great deal"),
    Q3 != "Skipped"
  ) %>%
  mutate(
    watched = Q2a %in% c("Very little", "Some of it", "A great deal"),
    QPID = factor(QPID, levels = c("A Democrat", "An Independent", "A Republican"))
  ) %>%
  group_by(QPID, watched) %>%
  summarize(approval = mean(Q3 == "Approve"),
            count = n())
conv_approve
# This is an interesting find. Regardless of party, people who watched the convention had a more
# favorable opinion of the convention itself. Could this be due to news coverage obscuring the
# event?


# What about the perception of criticism on Democrats? Watching just news coverage as opposed to
# watching the event could lead to different 
conv_balance <- convpoll %>%
  filter(
    QPID %in% c("A Democrat", "A Republican", "An Independent"),
    Q2a != "Skipped",
    Q2b == "Yes" || Q2a %in% c("Very little", "Some of it", "A great deal"),
    Q4 != "Skipped"
  ) %>%
  mutate(
    watched = Q2a %in% c("Very little", "Some of it", "A great deal"),
    QPID = factor(QPID, levels = c("A Democrat", "An Independent", "A Republican"))
  ) %>%
  group_by(QPID, watched) %>%
  summarize(fair = mean(Q4 == "Maintained the right balance"),
            count = n())
conv_balance
# This is really interesting as well, we find incredibly similar conclusions as the last
# analysis.


# As both of these findings are quite interesting in their own right, I will plot both in a very
# similar fashion. I really want to highlight the *difference* in opinion from those who watched
# the convention and those who just watched the respective news coverage, especially the fact
# that the difference is really similar between parties.

# Before I dive into this, a potential criticism of this analysis is that those who view
# Republicans more favorably would watch the convention. Maybe a Democrat who is not
# "anti-Republican" is more likely to watch.

# I first will filter for those who were engaged in the convention. This means either they
# watched news coverage OR they watched at *least* a little of the convention. I also filter
# out those who skipped questions as those are not helpful observations. In order to determine if
# someone "watched" the convention, they must watch at least a little of the convention. I then
# group by party and "watched", and calculate their respective opinions.

# To actually plot this, I will use geom_point to draw the plot points, and then use
# geom_segment to connect the points by political party. I may also use an arrow to help
# highlight the fact that the trend is in the same direction for all parties. But that may
# suggest some sort of progression over time, where in reality there are just two
# distinct groups.

# It is worth considering using proportions to some extent, as I think the difference between
# 0.0122 and 0.0784 is quite dramatic, but it will may lead to misleading visuals.

plot_approve <- conv_approve %>%
  select(-count) %>%
  pivot_wider(names_from = "watched", values_from = "approval") %>%
  rename("not_watched" = `FALSE`, "watched" = `TRUE`)

plot_balance <- conv_balance %>%
  select(-count) %>%
  pivot_wider(names_from = "watched", values_from = "fair") %>%
  rename("not_watched" = `FALSE`, "watched" = `TRUE`)

comp_plot <- function(data, label, fname) {
  ggplot(data, aes(y = QPID)) +
  geom_segment(aes(x = not_watched, xend = watched, yend = QPID)) +
  geom_point(aes(x = watched, color = "Yes"), size = 3) +
  geom_point(aes(x = not_watched, color = "No"), size = 3) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_discrete(labels = c("Democrat", "Independent", "Republican")) +
  scale_color_manual(values = c("#fc9f5b", "#177e89")) +
  theme_minimal() +
  theme(
    legend.position = c(0.7, 0.25),
    axis.text.y = element_text(angle = 45)
  ) +
  labs(
    y = "",
    x = label,
    title = paste(label, "in 2020 Republican Convention"),
    color = "Watched Convention",
    caption = "ABC News/Ipsos Poll: 2020 Coronavirus Wave 20"
  ) +
  ggsave(width=7, height=4, filename = fname)
}

if (!file.exists("figures")) {
  dir.create("figures")
}
comp_plot(plot_approve, "Approval of Content", "figures/approval.pdf")
comp_plot(plot_balance, "Perceived Balance in Partisan Rhetoric", "figures/balance.pdf")

# To elaborate on these plots, their goal is to show this pattern where
# those who watched the convention, regardless of party, viewed the
# convention more favorably than those who did not. This is acheived with
# the geom_segment(). The geom_point() layers help the viewer understand
# what the endpoints of the segment represent.

# As for the message of the plots themselves, what does it mean? I have two
# theories on this. Either (a) those who decide to watch the convention,
# regardless of party, have relatively positive views towards the Republican
# party, or (b) news coverage showed the highlights of the convention, which
# I would argue contains very heavy rhetoric. This would skew the rhetoric of
# the convention as a whole, as there were likely many mundane moments throughout.
