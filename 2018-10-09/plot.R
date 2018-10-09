# install rstan and rethinking packages

# install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)
# install.packages(c("coda","mvtnorm","devtools","loo"))
# library(devtools)
# devtools::install_github("rmcelreath/rethinking")

library(rstan)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(rethinking)
library(ggthemes)

turnout <- read_csv("voter_turnout.csv")

turnout$pct <- turnout$votes / turnout$eligible_voters

# create new column if year is a midterm
midterm <- c(1982, 1986, 1990, 1994, 1998, 2002, 2006, 2010, 2014)
turnout$midterm <- ifelse(turnout$year %in% midterm, 1, 0)

us <- turnout %>%
  filter(state == "United States") %>%
  select(year, us_pct = pct)

turnout <- left_join(turnout, us, by = "year") %>%
  filter(year >= 1998)

# find the states with na values
state_na <- turnout %>%
  filter(is.na(votes))

# lag variable
turnout <- turnout %>%
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(last_vote = lag(pct, n = 2))

# let's impute the missing data for minnesota
minnesota <- turnout %>%
  filter(state == "Minnesota")

# prep data
data_list <- list(
  usa = minnesota$us_pct,
  last_vote = minnesota$last_vote,
  pct = minnesota$pct
)

# model for missing data
m <- map2stan(
  alist(
    usa ~ dnorm(mu,sigma),
    mu <- a + bP*pct +bL*last_vote,
    pct ~ dnorm(nu,sigma_N),
    last_vote ~ dnorm(nu,sigma_N),
    a ~ dnorm(0,100),
    bP ~ dnorm(0,10),
    bL ~ dnorm(0,10),
    nu ~ dnorm(0.5,1),
    sigma_N ~ dcauchy(0,1),
    sigma ~ dcauchy(0,1)
  ),
  data = data_list, iter = 1e4, chains = 2
)

# extract imputed means
# precis(m, depth=2)

# drop columns from data frame
turnout <- turnout %>%
  select(year, state, pct)

imputed <- tibble(
  year = c(2000, 2002, 2004),
  state = c("imp", "imp", "imp"),
  votes = c(2458303, NA, 2842912),
  eligible_voters = c(3506432, 3518184, 3609185),
  imputed_mean = c(NA, 0.57, NA),
  imputed_std = c(NA, 0.05, NA)
)

imputed$pct <- imputed$votes / imputed$eligible_voters
imputed <- imputed %>% replace_na(list(pct = 0.57))

imputed <- imputed %>%
  select(year, state, pct)

turnout <- rbind(turnout, imputed)

turnout %>%
  ggplot(aes(x = year, y = pct, group = state)) +
  geom_line(alpha = 0.15) +
  geom_line(data = filter(turnout, state == "imp"), aes(x = year, y = pct), color = "blue", size = 1, linetype = 3) +
  geom_line(data = filter(turnout, state == "United States"), aes(x = year, y = pct), color = "black", size = 1) +
  geom_line(data = filter(turnout, state == "Minnesota"), aes(x = year, y = pct), color = "blue", size = 1) + 
  scale_x_continuous(limits = c(1998,2016), breaks = c(2000, 2004, 2008, 2012)) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  geom_text(data = filter(turnout, year == 2014 & state == "United States"),
            aes(label = state, x = year + 1.45, y = pct), size = 3.5, color = "black") +
  geom_text(data = filter(turnout, year == 2014 & state == "Minnesota"),
            aes(label = state, x = year + 1.25, y = pct), size = 3.5, color = "blue") +
  annotate(geom = "text", x = 2012, y = 0.80, label = "Presidential election", size = 3.5) +
  annotate(geom = "text", x = 2010, y = 0.27, label = "Midterm election", size = 3.5) +
  labs(x = "", y = "", title = "United States Voter Turnout: 1998-2014",
       subtitle = "Minnesota turnout imputed for 2002 election",
       caption = "TidyTuesday 10/09/18, source:data.world") +
  theme_fivethirtyeight()
  
  





  



