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


ggsave("3_analyses/Fig2.eps", width = 16, height=14, units = "cm", dpi = 800)









