library(tidyverse)
library(ggthemes)

# Get the Data

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# View songs by decade

rankings %>% 
  mutate(decade = year - year %% 10) %>%
  count(decade) %>%
  ggplot(aes(x = decade, y = n)) + 
  geom_bar(stat = "identity") + 
  labs(x = "",
       y = "Number of Songs",
       title = "The 90s are the most popular decade for hip hop",
       caption = "Data source: BBC, by: @trevin_flick") + 
  coord_flip() + 
  theme_fivethirtyeight()

ggsave("decades.png")

# spread of years for each critic sorted by variance

polls %>%
  ggplot(aes(x = reorder(critic_name, year, FUN=var), y = year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "",
       y = "",
       title = "A lot critics pick songs among the same era",
       subtitle = "30/107 picked songs within a 10-year window",
       caption = "Data source: BBC, by: @trevin_flick") 

ggsave("critics_spread.png")







############ EXTRA STUFF


# Range of year song is released

summary(rankings$year)

# min 1979
# median 1999
# mean 2000
# max 2019

# based on the plot provided
# show year vs. points

rankings %>% 
  ggplot(aes(x = year, y = points)) + 
  geom_point() +
  labs(title = "Best Hip Hop Songs of All Time")
theme_fivethirtyeight()


# critic role counts

polls %>%
  distinct(critic_name, critic_rols) %>%
  count(critic_rols) %>%
  View()

# variance of years for each critic

polls %>% 
  group_by(critic_name) %>% 
  summarise(dev = sd(year)) %>% 
  ggplot(aes(dev)) +
  geom_density()

# years spread for each critic

polls %>%
  group_by(critic_name) %>%
  mutate(range = max(year) - min(year)) %>%
  distinct(critic_name, range) %>%
  View()
