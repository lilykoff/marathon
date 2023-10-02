library(here)
library(tidyverse)
library(readr)
library(lubridate)


# london 
# manually fix ties 
london_2014  <- read_csv(here("data/london_2014.csv")) %>% 
  mutate(place = `Place gender`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = `Finish time`) %>% 
  filter(place <= 500) %>% 
  group_by(place) %>% 
  mutate(n = n()) %>% 
  filter(n == 2) %>% 
  mutate(
    rank = min_rank(time),
    sex = ifelse(rank == 1, "M", "W")) %>% 
  select(place, country, sex, year, time, age = Category, Name)

london_2014_ties  <- read_csv(here("data/london_2014.csv")) %>% 
  mutate(place = `Place gender`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = `Finish time`) %>% 
  filter(place <= 500) %>% 
  group_by(place) %>% 
  mutate(n = n()) %>% 
  filter(n != 2) 

london_2014_ties$sex <- c("W", "W", "M", "M", "W", "M", "W", "M",
                          "M", "W", "W", "M")
london_2014 <-
  london_2014 %>% 
  bind_rows(london_2014_ties %>% select(place, country, sex, year, time, Name, age = Category))



london_2015 <- read_csv(here("data/london_2015.csv")) %>% 
  mutate(place = `Place gender`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = FINISH) %>% 
  filter(place <= 500) %>% 
  group_by(place) %>% 
  mutate(n = n()) %>% 
  filter(n == 2) %>% 
  mutate(
    rank = min_rank(time),
    sex = ifelse(rank == 1, "M", "W")) %>% 
  select(place, country, sex, year, time, age = Category, Name)

london_2015_ties  <- read_csv(here("data/london_2015.csv")) %>% 
  mutate(place = `Place gender`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = FINISH) %>% 
  filter(place <= 500) %>% 
  group_by(place) %>% 
  mutate(n = n()) %>% 
  filter(n != 2) 

london_2015_ties$sex <- c("W", "M", "M", "W", "W", "M", 
                          "M", "W", "W", "W", "M", "M", "M", 
                          "M", "W", "W")
london_2015 <-
  london_2015 %>% 
  bind_rows(london_2015_ties %>% select(place, country, sex,Name, year, time, age = Category))


london_2016 <- read_csv(here("data/london_2016.csv")) %>% 
  mutate(place = `Place gender`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = FINISH) %>% 
  filter(place <= 500) %>% 
  group_by(place) %>% 
  mutate(n= n()) %>% 
  filter(n == 2) %>% 
  mutate(
    rank = min_rank(time),
    sex = ifelse(rank == 1, "M", "W")) %>% 
  select(place, country, sex, year, time, age = Category, Name)

london_2016_ties <- read_csv(here("data/london_2016.csv")) %>% 
  mutate(place = `Place gender`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = FINISH) %>% 
  filter(place <= 500) %>% 
  group_by(place) %>% 
  mutate(n= n()) %>% 
  filter(n != 2) 


london_2017 <- read_csv(here("data/london_2017.csv")) %>% 
  mutate(place = `Place gender`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = Finish) %>% 
  filter(place <= 500) %>% 
  group_by(place) %>% 
  mutate(
    n = n()
  ) %>% 
  filter(n == 2) %>% 
  mutate(
    rank = min_rank(time),
    sex = ifelse(rank == 1, "M", "W")) %>% 
  select(place, country, sex, year, time, age = Category, Name)

london_2017_ties <- read_csv(here("data/london_2017.csv")) %>% 
  mutate(place = `Place gender`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = Finish) %>% 
  filter(place <= 500) %>% 
  group_by(place) %>% 
  mutate(
    n = n()
  ) %>% 
  filter(n != 2)

london_2017_ties$sex <- c("M", "W", "W", "M", "M", "W", "M", "W")

london_2017 <-
  london_2017 %>% 
  bind_rows(london_2017_ties %>% select(place, country,Name, sex, year, time, age = Category))


london_2018 <- read_csv(here("data/london_2018.csv")) %>% 
  mutate(place = `Place gender`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = Finish) %>% 
  filter(place <= 500) %>% 
  group_by(place) %>% 
  mutate(n = n()) %>% 
  filter(n == 2) %>% 
  mutate(
    rank = min_rank(time),
    sex = ifelse(rank == 1, "M", "W")) %>% 
  select(place, country, sex, year, time, age = Category, Name)

london_2018_ties <- read_csv(here("data/london_2018.csv")) %>% 
  mutate(place = `Place gender`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = Finish) %>% 
  filter(place <= 500) %>% 
  group_by(place) %>% 
  mutate(n = n()) %>% 
  filter(n != 2) 

london_2018_ties$sex <- c("M", "W", "W", "M", "M", "M", "W", "W")

london_2018 <-
  london_2018 %>% 
  bind_rows(london_2018_ties %>% select(place, country, sex,Name, year, time, age = Category))

london_2019 <- read_csv(here("data/london_2019.csv")) %>% 
  mutate(place = `Place P`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = hms::as_hms(gsub(".*Finish", "", Time)),
         age = gsub(".*Category", "", Division)) %>% 
  filter(place <= 500) %>% 
  group_by(place) %>% 
  mutate(n = n()) %>% 
  filter(n == 2) %>% 
  mutate(
    rank = min_rank(time),
    sex = ifelse(rank == 1, "M", "W")) %>% 
  select(place, country, sex, year = Year, time, age, Name)


london_2019_ties <- read_csv(here("data/london_2019.csv")) %>% 
  mutate(place = `Place P`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = hms::as_hms(gsub(".*Finish", "", Time)),
         age = gsub(".*Category", "", Division)) %>% 
  filter(place <= 500) %>% 
  group_by(place) %>% 
  mutate(n = n()) %>% 
  filter(n != 2) 

london_2019_ties$sex <- c("M", "W", "M", "W", "M", "W",
                          "M", "W", "M", "W", "M", "W", 
                          "W", "M", "M", "W")

london_2019 <-
  london_2019 %>% 
  bind_rows(london_2019_ties %>% select(place, country, sex, Name,year = Year, time, age))


london_2021 <- read_csv(here("data/london_2021_2023.csv")) %>% 
  mutate(place = `Place P`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = hms::as_hms(gsub(".*Finish", "", Time)),
         age = gsub(".*Category", "", Division)) %>% 
  filter(place <= 500 & Year == 2021) %>% 
  group_by(place, Year) %>% 
  mutate(n = n()) %>% 
  filter(n == 2) %>% 
  mutate(
    rank = min_rank(time),
    sex = ifelse(rank == 1, "M", "W")) %>% 
  select(place, country, sex, year = Year, time, age, Name)

london_2021_ties <- read_csv(here("data/london_2021_2023.csv")) %>% 
  mutate(place = `Place P`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = hms::as_hms(gsub(".*Finish", "", Time)),
         age = gsub(".*Category", "", Division)) %>% 
  filter(place <= 500 & Year == 2021) %>% 
  group_by(place, Year) %>% 
  mutate(n = n()) %>% 
  filter(n != 2) 

london_2022 <- read_csv(here("data/london_2021_2023.csv")) %>% 
  mutate(place = `Place P`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = hms::as_hms(gsub(".*Finish", "", Time)),
         age = gsub(".*Category", "", Division)) %>% 
  filter(place <= 500 & Year == 2022) %>% 
  group_by(place, Year) %>% 
  mutate(n = n()) %>% 
  filter(n == 2) %>% 
  mutate(
    rank = min_rank(time),
    sex = ifelse(rank == 1, "M", "W")) %>% 
  select(place, country, sex, year = Year, time, age, Name)

london_2022_ties <- read_csv(here("data/london_2021_2023.csv")) %>% 
  mutate(place = `Place P`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = hms::as_hms(gsub(".*Finish", "", Time)),
         age = gsub(".*Category", "", Division)) %>% 
  filter(place <= 500 & Year == 2022) %>% 
  group_by(place, Year) %>% 
  mutate(n = n()) %>% 
  filter(n != 2) 

london_2023 <- read_csv(here("data/london_2021_2023.csv")) %>% 
  mutate(place = `Place P`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = hms::as_hms(gsub(".*Finish", "", Time)),
         age = gsub(".*Category", "", Division)) %>% 
  filter(place <= 500 & Year == 2023) %>% 
  group_by(place, Year) %>% 
  mutate(n = n()) %>% 
  filter(n == 2) %>% 
  mutate(
    rank = min_rank(time),
    sex = ifelse(rank == 1, "M", "W")) %>% 
  select(place, country, sex, year = Year, time, age, Name)

london_2023_ties <- read_csv(here("data/london_2021_2023.csv")) %>% 
  mutate(place = `Place P`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = hms::as_hms(gsub(".*Finish", "", Time)),
         age = gsub(".*Category", "", Division)) %>% 
  filter(place <= 500 & Year == 2023) %>% 
  group_by(place, Year) %>% 
  mutate(n = n()) %>% 
  filter(n != 2) %>% 
  group_by(place) %>% 
  mutate(rank = min_rank(time)) %>% 
  filter(rank != 3) %>% 
  mutate(
    sex = ifelse(rank == 1, "M", "W"))

london_2023 <-
  london_2023 %>% 
  bind_rows(london_2023_ties %>%  select(place, country, Name, sex, year = Year, time, age))

london_tidy <-
  bind_rows(
    london_2014,
    london_2015,
    london_2016,
    london_2017,
    london_2018,
    london_2019,
    london_2021,
    london_2022,
    london_2023
  ) %>% 
  mutate(race = "London") %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    lastname = ifelse(year < 2019, sub("\\,.*", "", stringr::str_split(Name, " ")[[1]][2]),
                      sub("\\,.*", "", stringr::str_split(Name, " ")[[1]][1])),
    firstname = ifelse(year < 2019, stringr::str_split(Name, " ")[[1]][3], 
                       stringr::str_split(Name, " ")[[1]][2])) %>% 
  ungroup() %>% 
  select(-Name)

write.csv(london_tidy, "data/london_tidy.csv")
