library(here)
library(tidyverse)
library(readr)
library(lubridate)
library(scales)
theme_set(theme_light())
data <- read_csv(here("data/top_500_tidy_2014_2023.csv"),
                 col_types = cols(...1 = col_skip())) %>% 
          mutate(time = hms(time),
                 seconds = period_to_seconds(time))
data %>% 
  filter(race == "Berlin") %>% 
  filter(place >= 50) %>% 
  ggplot(aes(x = year, y = seconds, group = year))+
  geom_boxplot() + 
  facet_wrap(.~sex, scales = "free_y")+
  scale_x_continuous(breaks=seq(2014, 2022))+
  labs(x = "Year")+
  theme(axis.text.x = element_text(angle = 45, vjust = .5))

data %>% 
  filter(race == "Chicago") %>% 
  filter(place >= 50) %>% 
  ggplot(aes(x = year, y = seconds, group = year))+
  geom_boxplot() + 
  facet_wrap(.~sex, scales = "free_y")+
  scale_x_continuous(breaks=seq(2014, 2022))+
  labs(x = "Year")+
  theme(axis.text.x = element_text(angle = 45, vjust = .5))

data %>% 
  filter(race == "Boston") %>% 
  filter(place >= 50) %>% 
  ggplot(aes(x = year, y = seconds/3600, group = year))+
  geom_boxplot() + 
  facet_wrap(.~sex, scales = "free_y")+
  scale_x_continuous(breaks=seq(2014, 2023))+
  labs(x = "Year", y = "Hours", title = "Boston")+
  theme(axis.text.x = element_text(angle = 45, vjust = .5)) 



data %>% 
  filter(race == "London") %>% 
  filter(place >= 50 & place <= 250) %>% 
  mutate(
    min = seconds/60
  ) %>% 
  ggplot(aes(x = year, y = min, group = year)) +
  geom_boxplot() + 
  facet_wrap(.~sex, scales = "free_y")+
  scale_x_continuous(breaks=seq(2014, 2023))+
  labs(x = "Year", y = "Minutes", title = "London Marathon",
       subtitle = "Places 50 through 250")+
  theme(axis.text.x = element_text(angle = 45, vjust = .5))+
  scale_y_continuous(breaks=seq(120, 200, 5),
                     labels = c("2H 00M", 
                                "2H 05M",
                                "2H 10M", 
                                "2H 15M", 
                                "2H 20M",
                                "2H 25M",
                                "2H 30M",
                                "2H 35M",
                                "2H 40M",
                                "2H 45M",
                                "2H 50M",
                                "2H 55M",
                                "3H 00M",
                                "3H 05M",
                                "3H 10M",
                                "3H 15M",
                                "3H 20M"))


# missing berlin 2019 women 
