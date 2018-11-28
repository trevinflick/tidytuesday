library(readr)
library(dplyr)
library(forcats)
library(stringr)
library(ggplot2)
library(ggthemes)

#### CLEANING THE DATA ####
bridges <- read_csv("baltimore_bridges.csv")

bridges %>% count(responsibility, sort = TRUE)
bridges <- bridges %>%
  mutate(responsibility = fct_lump(responsibility, n = 4))

bridges$vehicles <- as.numeric(str_replace_all(bridges$vehicles, " vehicles", ""))


#### EXPLORING DATA WITH PLOTS ####

# avg_daily_traffic and vehicles are same column

bridges %>% ggplot(aes(avg_daily_traffic)) + geom_histogram()

bridges %>% ggplot(aes(vehicles)) + geom_histogram()

bridges %>% ggplot(aes(yr_built)) + geom_histogram()

bridges %>% filter(yr_built < 1900) %>% View()

# two extreme outliers for improvement costs $300,000,000
bridges %>% filter(total_improve_cost_thousands < 38000) %>%
  ggplot(aes(avg_daily_traffic, total_improve_cost_thousands)) + geom_point()

bridges %>%
  ggplot(aes(yr_built, avg_daily_traffic, color = bridge_condition)) + 
  geom_point(aes(fill=bridge_condition)) +
  scale_y_log10()

bridges %>%
  ggplot(aes(yr_built, stat(count), fill=bridge_condition)) + 
  geom_density(alpha = 0.6, position = "stack") +
  scale_fill_manual(values = c("#ffffbf", "#91bfdb", "#fc8d59"), 
                    breaks=c("Poor","Fair","Good"),
                    name="Bridge Condition") +
  labs(x="", y="",
       title = "The State of Maryland Bridges",
       subtitle = "Year built factors into condition",
       caption = "TidyTuesday 11/27/18, source:Federal Highway Administration") +
  theme_fivethirtyeight()


