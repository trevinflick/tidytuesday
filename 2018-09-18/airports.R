library(dplyr)
library(forcats)
library(gganimate)

airports <- read.csv("us-airports.csv")

# airports %>% 
#  filter(hub_type == "Large") %>%
#  ggplot(aes(year, passengers, group = loc_id)) +
#  geom_line()
  
# states <- airports %>%
#  group_by(year, state) %>%
#  dplyr::summarise(count = n())

airports %>%
  filter(!is.na(state)) %>%
  ggplot(aes(x = fct_infreq(state))) + geom_bar() +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1)) +
  labs(title = "Year: {frame_time}",
       x = "", y = "number of airports", caption = "TidyTuesday 9/18/18") +
  transition_time(year) +
  ease_aes('linear')

animate(p, interval = 2)
