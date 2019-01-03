library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)

tidytuesday <- read_rds("tidytuesday_tweets.rds")

# What users get the most retweets and favorites?

tidytuesday %>%
  group_by(screen_name, followers_count) %>%
  summarise(retweet_total = sum(retweet_count),
            favorite_total = sum(favorite_count)) %>%
  arrange(desc(favorite_total)) %>% 
  View()
  

# How has the popularity of #TidyTuesday changed since it's inception?

tidytuesday$created_at <- ymd_hms(tidytuesday$created_at)
tidytuesday$week <- week(tidytuesday$created_at)

# When did David Robinson start his screencast?
tidytuesday %>%
  filter(screen_name == "drob") %>%
  View()

tidytuesday %>%
  filter(week == 30) %>%
  View()

tidy_by_week <- tidytuesday %>%
  group_by(week) %>%
  summarise(retweet_total = sum(retweet_count),
            favorite_total = sum(favorite_count)) %>%
  arrange(week)

tidy_by_week %>%
  mutate(week = week - 13) %>%
  ggplot(aes(x=week, y=favorite_total)) +
  annotate("text", x=29, y=1225, 
           label="David Robinson's first screencast") +
  annotate("text", x=17, y=40,
           label="#rstats p-hackathon challenge") +
  geom_line(size=1.5) +
  scale_x_continuous(breaks = c(1,10,20,30,38)) +
  labs(y="", x="",
       title = "#TidyTuesday popularity over time",
       subtitle = "Total Twitter favorites each week",
       caption = "TidyTuesday 01/01/2019, source:rtweet") +
  theme_fivethirtyeight()


