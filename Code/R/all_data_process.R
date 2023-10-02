library(here)
library(tidyverse)
library(readr)

berlin <- read_csv(here("data/berlin_tidy.csv"),
                   col_types = cols(...1 = col_skip()))
chi <- read_csv(here("data/chi_tidy.csv"),
                col_types = cols(...1 = col_skip()))
boston <- read_csv(here("data/boston_tidy.csv"),
                   col_types = cols(...1 = col_skip()))
london <- read_csv(here("data/london_tidy.csv"),
                   col_types = cols(...1 = col_skip()))


all_tidy <-
  bind_rows(berlin, chi, boston, london) 

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
