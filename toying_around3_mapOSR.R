library(tidyverse)
library(hrbrthemes)

mapOSR <- rio::import("https://zenodo.org/record/6491891/files/mapOSR_data_V5_9_3_220419_coded_clean.csv") %>%
  mutate(year = `Publication Year`)  %>%
  dplyr::select(year, Discipline_agric:Discipline_hum)
  
mapOSR_l <- mapOSR %>%
  pivot_longer(2:8, names_to = "discipline", values_to = "values") %>%
  dplyr::filter(discipline!="Discipline_nonspecificdisc" & year != 2021) %>%
  dplyr::mutate(discipline = case_when(
                                discipline == "Discipline_agric" ~ "Agricultural Sciences",
                                discipline == "Discipline_engtech" ~ "Engineering & Technology",
                                discipline == "Discipline_hum" ~ "Humanities",
                                discipline == "Discipline_med" ~ "Medical & Health Sciences",
                                discipline == "Discipline_natscie" ~ "Natural Sciences",
                                discipline == "Discipline_socscie" ~ "Social Sciences")) %>%
  group_by(discipline, year) %>%
  mutate(ysum = sum(values, na.rm = T))

ggplot(mapOSR_l, aes(x=year, y=values)) +
  # geom_smooth(aes(y=ysum), alpha=0.5) +
  stat_summary(geom = "line", fun = sum, alpha = .5, size = 1.5) +
  stat_summary(geom = "point", fun = sum, alpha = .5, size = 2) +
  ylab("number of publications on open science") +
  theme_ipsum_pub() +
  facet_wrap( ~ discipline)
  
