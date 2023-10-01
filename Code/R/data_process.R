library(here)
library(tidyverse)
library(readr)
library(lubridate)

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
  mutate(Net = hms(Net)) %>%
  rename(Time = Net) %>%
  group_by(year, Sex) %>%
  arrange(Time) %>%
  mutate(place = row_number()) %>%
  ungroup()

berlin_tidy_2014_2022 <-
  berlin_2014_2022 %>%
  select(
    place,
    country = Nation,
    sex = Sex,
    age = AC,
    time = Time,
    year,
    lastname = Lastname,
    firstname = Firstname
  ) %>% 
  distinct()
           

# berlin_2014_2019 <-
#   read_csv(here("data/archive/Berlin_Marathon_data_1974_2019.csv")) %>%
#   filter(YEAR >= 2014) %>%
#   mutate(TIME = hms(TIME)) %>%
#   group_by(YEAR, GENDER) %>%
#   arrange(TIME) %>%
#   mutate(place = row_number()) %>%
#   filter(place <= 500)
# 
# berlin_tidy_2014_2019 <-
#   berlin_2014_2019 %>%
#   ungroup() %>%
#   select(c(
#     year = YEAR,
#     country = COUNTRY,
#     age = AGE,
#     sex = GENDER,
#     place,
#     time = TIME
#   )) %>%
#   mutate(sex = ifelse(sex  == "male", "M", "W"))
# 
# berlin_joined <-
#   full_join(berlin_tidy_2014_2019,
#             berlin_tidy_2014_2022,
#             by = c("year", "sex", "place", "time")) %>% 
#   mutate(
#     age = ifelse(is.na(age.x), age.y, age.x),
#     country = ifelse(is.na(country.y), country.x, country.y)
#   ) %>% 
#   select(-c(age.x, country.x, country.y, age.y))
# 
# berlin_joined %>%
#   filter(place <= 500) %>% 
#   distinct() %>% 
#   group_by(year, sex) %>% 
#   summarize(n())

berlin_tidy <-
  berlin_tidy_2014_2022 %>% 
  mutate(race = "Berlin")
         
write.csv(berlin_tidy, "data/berlin_tidy.csv")


boston_2012_2017 <-
  read_csv(here("data/boston_2012_2017.csv")) 

boston_2012_2017_tidy <-
  boston_2012_2017 %>% 
  select(c(time = 'Finish Net',
           place = 'Place Gender',
           sex,
           year, 
           Name)) %>% 
  mutate(time = hms(time),
         country = gsub("\\).*", "", gsub(".*\\(", "", Name))) %>% 
  rowwise() %>% 
  mutate(
         lastname = sub("\\,.*", "", stringr::str_split(Name, " ")[[1]][2]),
         firstname = stringr::str_split(Name, " ")[[1]][3]) %>% 
  ungroup() %>% 
  select(-Name)

boston_2018_2023 <-
  read_csv(here("data/boston_2018_2023.csv")) # what is going on 

boston_2018_2023_tidy <- 
  boston_2018_2023 %>% 
  group_by(Sex, Year) %>% 
  arrange(Place) %>% 
  ungroup() %>% 
  mutate(
    time = hms(gsub(".*Net", "", Time)),
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

# london 
# manually fix ties 
london_2014  <- read_csv(here("data/london_2014.csv")) %>% 
  mutate(place = `Place gender`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = lubridate::hms(`Finish time`)) %>% 
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
         time = lubridate::hms(`Finish time`)) %>% 
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
         time = lubridate::hms(FINISH)) %>% 
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
         time = lubridate::hms(FINISH)) %>% 
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
         time = lubridate::hms(FINISH)) %>% 
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
         time = lubridate::hms(FINISH)) %>% 
  filter(place <= 500) %>% 
  group_by(place) %>% 
  mutate(n= n()) %>% 
  filter(n != 2) 
  

london_2017 <- read_csv(here("data/london_2017.csv")) %>% 
  mutate(place = `Place gender`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = lubridate::hms(Finish)) %>% 
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
         time = lubridate::hms(Finish)) %>% 
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
         time = lubridate::hms(Finish)) %>% 
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
         time = lubridate::hms(Finish)) %>% 
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
         time = hms(gsub(".*Finish", "", Time)),
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
         time = hms(gsub(".*Finish", "", Time)),
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
           time = hms(gsub(".*Finish", "", Time)),
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
         time = hms(gsub(".*Finish", "", Time)),
         age = gsub(".*Category", "", Division)) %>% 
  filter(place <= 500 & Year == 2021) %>% 
  group_by(place, Year) %>% 
  mutate(n = n()) %>% 
  filter(n != 2) 

london_2022 <- read_csv(here("data/london_2021_2023.csv")) %>% 
  mutate(place = `Place P`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = hms(gsub(".*Finish", "", Time)),
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
         time = hms(gsub(".*Finish", "", Time)),
         age = gsub(".*Category", "", Division)) %>% 
  filter(place <= 500 & Year == 2022) %>% 
  group_by(place, Year) %>% 
  mutate(n = n()) %>% 
  filter(n != 2) 

london_2023 <- read_csv(here("data/london_2021_2023.csv")) %>% 
  mutate(place = `Place P`,
         country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
         time = hms(gsub(".*Finish", "", Time)),
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
         time = hms(gsub(".*Finish", "", Time)),
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
    lastname = sub("\\,.*", "", stringr::str_split(Name, " ")[[1]][2]),
    firstname = stringr::str_split(Name, " ")[[1]][3]) %>% 
  ungroup() %>% 
  select(-Name)

write.csv(london_tidy, "data/london_tidy.csv")

chi_2022 <- 
  read_csv(here("data/chi_2022.csv")) %>% 
  mutate(
    sex = ifelse(Sex == "Female", "W", "M"),
    country = gsub("\\).*", "", gsub(".*\\(", "", Name)),
    age = gsub(".*Division", "", Division),
    place = `Place P`,
    time = hms(gsub(".*Finish", "", Time)),
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
    time = hms(gsub(".*Finish", "", Time)), 
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
    time = hms(gsub(".*Finish", "", Time)), 
    lastname = sub("\\,.*", "", Name),
    firstname = stringr::str_trim(sub(".*\\,", "", sub("\\(.*", "", Name)))) %>% 
  group_by(year, sex) %>% 
  arrange(place) %>% 
  ungroup() %>% 
  select(sex, country, age, year, place, time, lastname, firstname)
# 
# chi_2014_2021 %>% 
#   group_by(sex, year) %>% 
#   summarize(n = n())
# 
# chi_2014_2021_fix %>% 
#   group_by(sex, year) %>% 
#   summarize(n = n())

chi_tidy <-
  bind_rows(chi_2014_2021, chi_2014_2021_fix, chi_2022) %>% 
  mutate(race = "Chicago")

write.csv(chi_tidy, "data/chi_tidy.csv")


berlin <- read_csv(here("data/berlin_tidy.csv"),
                   col_types = cols(...1 = col_skip()))
chi <- read_csv(here("data/chi_tidy.csv"),
                col_types = cols(...1 = col_skip()))
boston <- read_csv(here("data/boston_tidy.csv"),
                   col_types = cols(...1 = col_skip()))
london <- read_csv(here("data/london_tidy.csv"),
                   col_types = cols(...1 = col_skip()))


all_tidy <-
  bind_rows(berlin, chi, boston, london) %>% 
  mutate(time = hms(time))

all_tidy %>% 
  group_by(race) %>% 
  summarize(min_year = min(year))


write.csv(all_tidy, "data/all_tidy.csv")

all_tidy %>% 
  group_by(year, sex, race) %>% 
  summarize(n())

top500_2014_2023 <-
  all_tidy %>% filter(year >= 2014, place <= 500)

write.csv(top500_2014_2023, "data/top_500_tidy_2014_2023.csv")  

top500_2014_2023_deident <-
  top500_2014_2023 %>% 
  select(-c(lastname, firstname))

write.csv(top500_2014_2023_deident, "data/top_500_tidy_2014_2023_deident.csv")  
