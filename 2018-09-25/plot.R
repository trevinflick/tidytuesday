library(ggplot2)
library(dplyr)
library(ggthemes)

table_3$TICt <- gsub("\\$", "", table_3$TICt)
table_3$TICt <- as.numeric(gsub(",", "", table_3$TICt))

table_3$meanGDP <- gsub("\\$", "", table_3$meanGDP)
table_3$meanGDP <- as.numeric(gsub(",", "", table_3$meanGDP))

table_3 %>%
  ggplot(aes(log(meanGDP), log(TICt), label = Country)) +
  geom_point(alpha = 0.5,
             color = ifelse(table_3$propGDP > 0.25, "red", "black")) +
  geom_text_repel(data = filter(table_3, log(TICt) > 11 | log(TICt) < 2 | propGDP > 0.25),
                  segment.color = "grey50",
                  segment.size = 0.5) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  ylab('Total Invasion Cost [log scale]') + 
  xlab('GDP [log scale]') +
  labs(caption = "TidyTuesday 9/25/18, source:Paini et al, 2016")


