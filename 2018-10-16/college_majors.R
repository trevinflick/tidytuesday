library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)

# read in the data
all_ages <- read_csv("all-ages.csv")
grad_students <- read_csv("grad-students.csv")
majors_list <- read_csv("majors-list.csv")
recent_grads <- read_csv("recent-grads.csv")
women_stem <- read_csv("women-stem.csv")

# drop missing values
recent_grads <- recent_grads %>%
  drop_na(Total)

# earnings by major category
recent_grads %>%
  group_by(Major_category) %>%
  summarize(
    count = n(),
    earnings = mean(Median),
    Max = max(Median)
  )

# filter out major categories with < 8 majors
big_category <- recent_grads %>%
  filter(Major_category != "Interdisciplinary" &
           Major_category != "Communications & Journalism" &
           Major_category != "Law & Public Policy" &
           Major_category != "Industrial Arts & Consumer Services") 

# plot
big_category %>%
  ggplot(aes(x = reorder(Major_category, -Median, mean),
             y = Median)
         ) +
  geom_dotplot(binaxis = "y", stackdir = "center", 
               dotsize = 0.5) +
  stat_summary(fun.y=mean, geom="point", shape = 18,
               size=3, color="red") +
  geom_text(data = filter(big_category, Major == "PETROLEUM ENGINEERING"),
            aes(label = "Petroleum Engineering"), size = 3, 
            hjust = -0.05, vjust = 0.3) +
  geom_text(data = filter(big_category, Major == "PETROLEUM ENGINEERING"),
            aes(label = "($110,000)"), size = 3, 
            hjust = -0.55, vjust = 2.5) +
  geom_text(data = filter(big_category, Major_category == "Arts" &
                            Median == 50000),
            aes(label = "Miscellaneous Fine Arts"), size = 3, 
            hjust = -0.05, vjust = 0.3) +
  geom_text(data = filter(big_category, Major_category == "Physical Sciences" &
                            Median == 62000),
            aes(label = "Astronomy and Astrophysics"), size = 3, 
            hjust = -0.05, vjust = 0.3) +
  labs(x = "", y = "",
       title = "Earnings for Recent College Grads",
       caption = "TidyTuesday 10/16/18, source:fivethirtyeight") +
  theme_fivethirtyeight() +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 60, hjust = 1)) 









