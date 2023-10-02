library(here)
library(tidyverse)
library(readr)

chi_2022 <- 
  read_csv(here("data/chi_2022.csv")) %>% 
  mutate(
    sex = ifelse(Sex == "Female", "W", "M"),
    country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
    age = gsub(".*Division", "", Division),
    place = `Place P`,
    time = hms::as_hms(gsub(".*Finish", "", Time)),
    lastname = sub("\\,.*", "", Name),
    firstname = stringr::str_trim(sub(".*\\,", "", sub("\\(.*", "", Name)))) %>% 
  select(sex, age, place, time, year = Year, lastname, firstname)

chi_2014_2021 <- 
  read_csv(here("data/chi_2014_2021.csv")) %>% 
  filter(Year != "Results: 2021 / all") %>% 
  mutate(
    sex = ifelse(Sex == "Men", "M", "W"),
    country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
    age = gsub(".*Division", "", Division),
    year = as.numeric(gsub("\\/ Marathon.*", "", gsub(".*Results: ", "", Year))),
    place = `Place P`,
    time = hms::as_hms(gsub(".*Finish", "", Time)), 
    lastname = sub("\\,.*", "", Name),
    firstname = stringr::str_trim(sub(".*\\,", "", sub("\\(.*", "", Name)))) %>% 
  group_by(year, sex) %>% 
  arrange(place) %>% 
  ungroup() %>% 
  select(sex, country, age, year, place, time, lastname, firstname)

chi_2014_2021_fix <- 
  read_csv(here("data/chi_2014_2021_fix.csv")) %>% 
  filter(Year != "Results: 2021 / all") %>% 
  mutate(
    sex = ifelse(Sex == "Men", "M", "W"),
    country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
    age = gsub(".*Division", "", Division),
    year = as.numeric(gsub("\\/ Marathon.*", "", gsub(".*Results: ", "", Year))),
    place = `Place P`,
    time = hms::as_hms(gsub(".*Finish", "", Time)), 
    lastname = sub("\\,.*", "", Name),
    firstname = stringr::str_trim(sub(".*\\,", "", sub("\\(.*", "", Name)))) %>% 
  group_by(year, sex) %>% 
  arrange(place) %>% 
  ungroup() %>% 
  select(sex, country, age, year, place, time, lastname, firstname)


chi_tidy <-
  bind_rows(chi_2014_2021, chi_2014_2021_fix, chi_2022) %>% 
  mutate(race = "Chicago")

write.csv(chi_tidy, "data/chi_tidy.csv")