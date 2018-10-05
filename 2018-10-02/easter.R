library(tidyverse)
library(readr)
library(ggplot2)
library(ggthemes)


us_births <- readr::read_csv("us_births_2000-2014.csv")
easter_dates <- readr::read_csv("easter_dates.csv")

easter_dates <- easter_dates %>%
  unclass() %>%
  split(names(.)[seq(length(.) / 5)]) %>%
  map_df(simplify) %>%
  select(month_day = "Easter Date", year = "Years") %>%
  arrange(year)

easter_dates <- easter_dates %>%
  separate(month_day, c("month", "date_of_month"), sep = " ")

months <- c("January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December")

easter_dates$month <- match(easter_dates$month, months)

easter_dates <- easter_dates %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         date_of_month = as.numeric(date_of_month)) %>%
  unite(day, c("year", "month", "date_of_month"), sep = "-")


week_before <- apply(easter_dates, 1, function(x) {seq.Date(as.Date(x['day']), by = "-1 day", length.out = 14)})

week_after <- apply(easter_dates, 1, function(x) {seq.Date(as.Date(x['day']) + 1, by = "+1 day", length.out = 14)})

easter <- data.frame(week_before, week_after) %>%
  unclass() %>%
  split(names(.)[seq(length(.) / 30)]) %>%
  map_df(simplify) %>%
  select(day = "X1")

easter$day <- as.Date(easter$day, origin = "1970-01-01")

easter <- easter %>%
  separate(day, c("year", "month", "date_of_month"), sep = "-") %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         date_of_month = as.numeric(date_of_month)) %>%
  arrange(year, month, date_of_month)

easter <- left_join(easter, us_births, by = c("year", "month", "date_of_month"))

easter$day <- rep(c(1:28), 15)

easter_births <- easter %>%
  group_by(day) %>%
  summarise(
    avg_births = mean(births)
  ) 

# avg birth non easter sundays
easter_births %>%
  filter(day == 7 | day == 21 | day == 28) %>%
  summarise(
    avg_births = mean(avg_births)
  )

easter_births %>%
  ggplot(aes(day, avg_births)) +
  geom_point() +
  geom_point(data = filter(easter_births, day == 14), color = "yellow") +
  geom_text(data = filter(easter_births, day == 14), label = "Easter", 
            nudge_x = 2, nudge_y = -100) +
  geom_hline(yintercept = 7285, color = "red") +
  geom_line() +
  geom_text(aes( 7, 7285, label = "non-Easter Sunday avg.", vjust = 1.2), 
            color = "red", size = 3) +
  labs(x = "", y = "births", title = "Fewer babies are born on Easter",
       subtitle = "U.S. births: two weeks before and after Easter \n(average births 2000-2014)") +
  labs(caption = "TidyTuesday 10/02/18, source:Fivethirtyeight") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())











