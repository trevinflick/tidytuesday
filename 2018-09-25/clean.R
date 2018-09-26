library(dplyr)
library(readr)
library(purrr)
library(tidyr)

table1 <- read_csv("table1.csv")
table2 <- read_csv("table2.csv")
table3 <- read_csv("table3.csv")
table4 <- read_csv("table4.csv")
table6 <- read_csv("table6.csv")

# clean table1

table_1 <- table1 %>%
  unclass() %>%
  split(names(.)[seq(length(.) / 3)]) %>%
  map_df(simplify) %>%
  drop_na() %>%
  select(Rank, everything())

write_csv(table_1, "table_1.csv")

# clean table2

table2 <- table2[-1, ]
  
table_2 <- table2 %>%
  separate(X1, into = c("Rank", "Country"), sep = " ", extra = "merge") %>%
  separate(X4, into = c("Rank_1", "Country_1"), sep = " ", extra = "merge") %>%
  separate(X7, into = c("Rank_2", "Country_2"), sep = " ", extra = "merge") %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  unclass() %>%
  split(names(.)[seq(length(.) / 3)]) %>%
  map_df(simplify) %>%
  drop_na() %>%
  select(Rank, Country, TICt = "TICt  (millions")

write_csv(table_2, "table_2.csv")


# clean table3

table3 <- table3[-1, ]

table_3 <- table3 %>%
  separate(X1, into = c("Rank", "Country"), sep = " ", extra = "merge") %>%
  separate(X6, into = c("Rank_1", "Country_1"), sep = " ", extra = "merge") %>%
  separate(X11, into = c("Rank_2", "Country_2"), sep = " ", extra = "merge") %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  unclass() %>%
  split(names(.)[seq(length(.) / 3)]) %>%
  map_df(simplify) %>%
  drop_na() %>%
  select(Rank, Country, TICt = "TICt  (millions", meanGDP = "X4", propGDP = "proportion of")

write_csv(table_3, "table_3.csv")

# clean table4

table_4 <- table4 %>%
  separate("Rank Country", into = c("Rank", "Country"), sep = " ", extra = "merge") %>%
  separate("Rank Country_1", into = c("Rank_1", "Country_1"), sep = " ", extra = "merge") %>%
  separate("Rank Country_2", into = c("Rank_2", "Country_2"), sep = " ", extra = "merge") %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  unclass() %>%
  split(names(.)[seq(length(.) / 3)]) %>%
  map_df(simplify) %>%
  drop_na() %>%
  select(Rank, Country, TICs = "TICs  (millions US$)")

write_csv(table_4, "table_4.csv")

# clean table6

table6 <- table6[-1, ]

table_6 <- table6 %>%
  separate("maximum reported Species", into = c("max_impact_pct", "Country"), sep = " ", extra = "merge") %>%
  separate("maximum reported Species_1", into = c("max_impact_pct_1", "Country_1"), sep = " ", extra = "merge") %>%
  unclass() %>%
  split(names(.)[seq(length(.) / 3)]) %>%
  map_df(simplify) %>%
  drop_na() %>%
  select(species = Species, max_impact_pct)

write_csv(table_6, "table_6.csv")














