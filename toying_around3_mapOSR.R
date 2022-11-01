library(tidyverse)
library(hrbrthemes)

mapOSR <- rio::import("https://zenodo.org/record/6491891/files/mapOSR_data_V5_9_3_220419_coded_clean.csv") %>%
  mutate(year = `Publication Year`)  %>%
  dplyr::select(year, Discipline_agric:Discipline_hum)
  
mapOSR_l <- mapOSR %>%
  pivot_longer(2:8, names_to = "discipline", values_to = "values") %>%
  dplyr::filter(discipline!="Discipline_nonspecificdisc" & year != 2021)
  

ggplot(mapOSR_l, aes(x=year, y=values)) +
  stat_summary(geom = "point", fun = sum) +
  stat_summary(geom = "line", fun = sum) +
  theme_ipsum_pub() +
  facet_wrap( ~ discipline)
  
