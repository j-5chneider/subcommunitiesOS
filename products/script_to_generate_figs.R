library(tidyverse)
library(hrbrthemes)
library(tidyLPA)


## FIGURE 1
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
  stat_summary(geom = "line", fun = sum, 
               # alpha = .5, 
               color = "darkgrey",
               size = 1.5) +
  stat_summary(geom = "point", fun = sum, 
               # alpha = .5, 
               color = "#293133",
               size = 2) +
  ylab("number of publications on open science") +
  theme_ipsum_pub() +
  theme(plot.background = element_rect(fill="white")) +
  facet_wrap( ~ discipline)

ggsave("products/Fig1.eps", width = 16, height=12, units = "cm", dpi = 800, scale = 1.4)


## FIGURE 2
osc <- rio::import("https://zenodo.org/record/6834569/files/osc_data.csv") %>%
  dplyr::mutate(disc = disc_broad) %>%
  dplyr::select(-disc_broad)

osc_lpa <- osc %>%
  dplyr::select(osp_oso,
                osp_sof,
                osp_cod,
                osp_pre,
                osp_oer,
                osp_cit,
                osp_prp,
                osp_sco,
                osp_opr,
                osp_mat,
                osp_dat,
                osp_oap,
                osp_met) %>%
  mutate(across(osp_oso:osp_met, as.numeric)) %>%
  estimate_profiles(4) 

lpa_data <- get_data(osc_lpa)[,c(16:20)]
lpa_data <- cbind(osc, lpa_data) # bind with original data set

lpa_data_p <- lpa_data %>%
  dplyr::summarize(across(osp_prp:osp_cit, mean), .by = Class) %>%
  pivot_longer(2:14, names_to = "variables", values_to = "values") %>%
  mutate(variables = factor(variables, levels=c(
    "osp_oso",
    "osp_sof",
    "osp_cod",
    "osp_pre",
    "osp_oer",
    "osp_cit",
    "osp_prp",
    "osp_sco",
    "osp_opr",
    "osp_mat",
    "osp_dat",
    "osp_oap",
    "osp_met"
  )))

ggplot(lpa_data_p, aes(x=variables, y=values, color=as.factor(Class))) +
  geom_hline(yintercept = 2.5, color="lightgrey", linetype = "dashed") +
  stat_summary(aes(y=values, group=as.factor(Class)), 
               geom = "line", fun = mean, size=1.5) +
  stat_summary(aes(shape=as.factor(Class)), geom = "point", fun = mean, size=3.5) +
  scale_y_continuous(limits = c(1,4), expand=c(0,0), breaks=c(1,2,3,4)) +
  scale_x_discrete(labels=c("Open Source",
                            "Open Software ",
                            "Open Code",
                            "Preregistration",
                            "OER",
                            "Citizen\nScience",
                            "Public\nProject Plan",
                            "Science\nCommunication",
                            "Open Peer\nReview",
                            "Open\nMaterials",
                            "Open Data",
                            "Open Access\nPublication",
                            "Open\nMethodology")) +
  scale_color_viridis_d(option="viridis") +
  labs(color="Profile", shape = "Profile", x="", y="Value") +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "bottom")


ggsave("products/Fig2.eps", width = 16, height=14, units = "cm", dpi = 800)









