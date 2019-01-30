library(tidyverse)
library(gganimate)
library(ggthemes)

milk <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/state_milk_production.csv")
cheese <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/clean_cheese.csv")

cheese %>% 
  summarise_at(vars(2:13), sum, na.rm = TRUE) %>%
  gather(cheese, amount) %>%
  arrange(desc(amount))

## Make an 'other' category

cheese$Other <- rowSums(cheese[,c("Other Dairy Cheese", "Muenster", "Blue", "Brick",
                                  "Swiss", "Cream and Neufchatel")], na.rm = TRUE)

cheese %>% 
  select(Year,
         Cheddar,
         "American" = "American Other",
         Mozzarella,
         "Italian" = "Italian other",
         "Processed" = "Processed Cheese",
         "Spreads" = "Foods and spreads",
         Other) %>%
  gather(cheese, amount, -Year) %>%
  ggplot(aes(x = Year, y = amount, group = cheese)) +
  geom_path() +
  geom_text(aes(label = cheese), 
            nudge_x = 1,
            nudge_y = 0.3) +
  labs(x = "", y = "Average Consumption in Pounds per Person",
       title = "What's causing Mozzarella's rise in consumption?",
       subtile = "Average American cheese consumption 1970-2017",
       caption = "TidyTuesday 01/29/19 source:USDA") +
  theme_light() +
  transition_reveal(along = Year) +
  ease_aes('linear')

anim_save("cheese.gif", last_animation())
