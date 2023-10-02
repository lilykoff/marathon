library(here)
library(tidyverse)
library(readr)


boston_2012_2017 <-
  read_csv(here("data/boston_2012_2017.csv")) 

boston_2012_2017_tidy <-
  boston_2012_2017 %>% 
  select(c(time = 'Finish Net',
           place = 'Place Gender',
           sex,
           year, 
           Name)) %>% 
  mutate(country = gsub("\\).*", "", gsub(".*\\(", "", Name))) %>% 
  rowwise() %>% 
  mutate(
    lastname = sub("\\,.*", "", stringr::str_split(Name, " ")[[1]][2]),
    firstname = stringr::str_split(Name, " ")[[1]][3]) %>% 
  ungroup() %>% 
  select(-Name)

boston_2018_2023 <-
  read_csv(here("data/boston_2018_2023.csv")) 

boston_2018_2023_tidy <- 
  boston_2018_2023 %>% 
  group_by(Sex, Year) %>% 
  arrange(Place) %>% 
  ungroup() %>% 
  mutate(
    time = hms::as_hms(gsub(".*Net", "", Time)),
    year = as.numeric(gsub(".* | ", "", Year)),
    sex = ifelse(Sex == "Women", "W", "M"),
    country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
    lastname = sub("\\,.*", "", Name),
    firstname = stringr::str_trim(sub(".*\\,", "", sub("\\(.*", "", Name)))
  ) %>% 
  select(place = Place, time, year, sex, country, lastname, firstname)

boston_tidy <-
  bind_rows(boston_2018_2023_tidy, boston_2012_2017_tidy) %>% 
  mutate(race = "Boston")

write.csv(boston_tidy, "data/boston_tidy.csv") 