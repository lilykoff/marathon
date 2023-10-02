library(here)
library(tidyverse)
library(readr)

berlin_2014_2022 <-
  read_csv(here("data/berlin_2021.csv")) %>%
  mutate(year = 2021) %>%
  bind_rows(read_csv(here("data/berlin_2022.csv")) %>%
              mutate(year = 2022)) %>%
  bind_rows(read_csv(here("data/berlin_2019.csv")) %>%
              mutate(year = 2019)) %>%
  bind_rows(read_csv(here("data/berlin_2018.csv")) %>%
              mutate(year = 2018)) %>%
  bind_rows(read_csv(here("data/berlin_2017.csv")) %>%
              mutate(year = 2017)) %>%
  bind_rows(read_csv(here("data/berlin_2015.csv")) %>%
              mutate(year = 2015)) %>%
  bind_rows(read_csv(here("data/berlin_2016.csv")) %>%
              mutate(year = 2016)) %>%
  bind_rows(read_csv(here("data/berlin_2014.csv")) %>%
              mutate(year = 2014)) %>%
  select(-c(Certificate, Video, Photos, 'BIB#', Finish)) %>%
  rename(time = Net) %>%
  group_by(year, Sex) %>%
  arrange(time) %>%
  mutate(place = row_number()) %>%
  ungroup()

berlin_tidy_2014_2022 <-
  berlin_2014_2022 %>%
  select(
    place,
    country = Nation,
    sex = Sex,
    age = AC,
    time, 
    year,
    lastname = Lastname,
    firstname = Firstname
  ) %>% 
  distinct()

## for 2014, 15, 16, 17, 19, there isn't enough W data due to the way I scraped, so we get that and merge it in 

berlin_tidy_2014_2022_filt <-
  berlin_tidy_2014_2022 %>% 
  filter(!(year == 2014 & sex == "W")) %>% 
  filter(!(year == 2015 & sex == "W")) %>% 
  filter(!(year == 2016 & sex == "W")) %>% 
  filter(!(year == 2017 & sex == "W")) %>% 
  filter(!(year == 2019 & sex == "W"))

         
berlin_2016_w <-
  read_csv(here("data/berlin_2016_w.csv")) %>% 
  filter(Sex == "W") %>% 
  rename(time = Net) %>% 
  arrange(time) %>% 
  mutate(place = row_number(), 
         year = 2016) %>% 
  select(
    place,
    country = Nation,
    sex = Sex,
    age = AC,
    time,
    year,
    lastname = Lastname,
    firstname = Firstname
  ) %>% 
  distinct()


berlin_2019_w <-
  read_csv(here("data/berlin_2019_w.csv")) %>% 
  filter(Sex == "W") %>% 
  rename(time = Net) %>% 
  arrange(time) %>% 
  mutate(place = row_number(), 
         year = 2019) %>% 
  select(
    place,
    country = Nation,
    sex = Sex,
    age = AC,
    time,
    year,
    lastname = Lastname,
    firstname = Firstname
  ) %>% 
  distinct()

berlin_2017_w <-
  read_csv(here("data/berlin_2017_w.csv")) %>% 
  filter(Sex == "W") %>% 
  rename(time = Net) %>% 
  arrange(time) %>% 
  mutate(place = row_number(), 
         year = 2017) %>% 
  select(
    place,
    country = Nation,
    sex = Sex,
    age = AC,
    time,
    year,
    lastname = Lastname,
    firstname = Firstname
  ) %>% 
  distinct()

berlin_2014_w <-
  read_csv(here("data/berlin_2014_w.csv")) %>% 
  filter(Sex == "W") %>% 
  rename(time = Net) %>% 
  arrange(time) %>% 
  mutate(place = row_number(), 
         year = 2014) %>% 
  select(
    place,
    country = Nation,
    sex = Sex,
    age = AC,
    time,
    year,
    lastname = Lastname,
    firstname = Firstname
  ) %>% 
  distinct()

berlin_2015_w <-
  read_csv(here("data/berlin_2015_w.csv")) %>% 
  filter(Sex == "W") %>% 
  rename(time = Net) %>% 
  arrange(time) %>% 
  mutate(place = row_number(), 
         year = 2015) %>% 
  select(
    place,
    country = Nation,
    sex = Sex,
    age = AC,
    time,
    year,
    lastname = Lastname,
    firstname = Firstname
  ) %>% 
  distinct()

berlin_tidy_2014_2022_filt <-
  berlin_tidy_2014_2022 %>% 
  filter(!(year == 2014 & sex == "W")) %>% 
  filter(!(year == 2015 & sex == "W")) %>% 
  filter(!(year == 2016 & sex == "W")) %>% 
  filter(!(year == 2017 & sex == "W")) %>% 
  filter(!(year == 2019 & sex == "W")) %>% 
  bind_rows(berlin_2014_w) %>% 
  bind_rows(berlin_2015_w) %>% 
  bind_rows(berlin_2016_w) %>% 
  bind_rows(berlin_2017_w) %>% 
  bind_rows(berlin_2019_w) 

berlin_tidy_2014_2022_filt %>% 
  filter(place <= 500) %>% 
  group_by(sex, year) %>% 
  summarize(max = max(place))

berlin_tidy <-
  berlin_tidy_2014_2022_filt %>% 
  filter(sex != "X") %>% 
  mutate(race = "Berlin")

write.csv(berlin_tidy, "data/berlin_tidy.csv")
