library(tidyverse)
library(lubridate)
library(ggthemes)

imdb <- read_csv("IMDb_Economist_tv_ratings.csv")

# Shows with the most seasons
top_30 <- imdb %>%
  group_by(title) %>%
  summarise(n = n(),
            avg_rating = mean(av_rating)) %>%
  arrange(desc(n)) %>% 
  head(30)

# How are shows rated?
summary(imdb$av_rating)

# Shows with only one season
one_and_done <- imdb %>%
  group_by(title) %>%
  filter(n() == 1, seasonNumber == 1)

# Shows with multiple seasons
multiple_seasons <- imdb %>%
  group_by(title) %>%
  filter(n() > 1)

binded_data <- rbind(one_and_done, multiple_seasons)

# Plot how many seasons each show has
binded_data %>%
  count(title) %>%
  ggplot(aes(n)) +
  geom_bar() +
  theme_fivethirtyeight() +
  labs(x = "Number of seasons", y="",
       title = "Most dramas only have one season")
  

binded_data$seasons <- c(rep("one",nrow(one_and_done)),
                         rep("multiple",nrow(multiple_seasons)))

# Plot shows with one season compared to multiple seasons
binded_data %>%
  ggplot(aes(av_rating, fill = seasons)) +
  geom_density(alpha = 0.4) +
  labs(x = "Average rating", y = "",
       subtitle = "Comparing dramas with only one season to dramas with multiple",
       caption = "TidyTuesday 01/08/2019, source:IMDB") +
  theme_fivethirtyeight()

rating_by_show <- binded_data %>%
  group_by(title) %>%
  summarise(seasons = n(),
            avg_rating = mean(av_rating),
            avg_share = mean(share))

rating_by_show %>%
  ggplot(aes(seasons, avg_rating)) +
  geom_boxplot(aes(group = cut_width(seasons, 1))) +
  labs(y = "Average Rating", x = "Number of seasons",
       title = "Dramas with fewer seasons show more variability in avg. rating") +
  theme_economist()
  
  

