library(tidyverse)
library(formr)
library(viridis)
library(tidyLPA)
library(nnet)
library(kableExtra)
library(generics)
library(emmeans)


formr_connect(keyring = "formr_juergen")

osc <- formr_raw_results(survey_name = 'osc') %>%
  dplyr::filter(!str_detect(session, 'XXX') & !is.na(session) & !is.na(ended))

osc <- osc[-c(1:12),]

# # model for later lca
# f <- with(osc, cbind(osp_cit,
#                      osp_prp,
#                      osp_pre,
#                      osp_met,
#                      osp_sof,
#                      osp_mat,
#                      osp_cod,
#                      osp_dat,
#                      osp_oer,
#                      osp_opr,
#                      osp_oap,
#                      osp_oso,
#                      osp_sco) ~ 1)


# match data from discipline to data set
osc_0 <- formr_raw_results(survey_name = 'osc_0') %>%
  dplyr::select(session, disc) %>%
  dplyr::mutate(disc = str_sub(disc, 1, 3))
osc <- left_join(osc, osc_0, by = "session")




osc %>%
  dplyr::select(osp_cit,
                osp_prp,
                osp_pre,
                osp_met,
                osp_sof,
                osp_mat,
                osp_cod,
                osp_dat,
                osp_oer,
                osp_opr,
                osp_oap,
                osp_oso,
                osp_sco) %>%
  estimate_profiles(1:8) %>%
  compare_solutions()

# "osp_cit" ~ "Citizen Science",
# "osp_cod" ~ "Open Code",
# "osp_dat" ~ "Open Data",
# "osp_mat" ~ "Open Materials",
# "osp_met" ~ "Open Methodology",
# "osp_oap" ~ "Open Access",
# "osp_oer" ~ "OER",
# "osp_opr" ~ "Open Peer Review",
# "osp_oso" ~ "Open Source",
# "osp_pre" ~ "Preregistration",
# "osp_prp" ~ "Public Project Plan",
# "osp_sco" ~ "Science Communication",
# "osp_sof" ~ "Open Software"



osc_lpa <- osc %>%
  dplyr::select(osp_oso,
                osp_sof,
                osp_cod,
                osp_pre,
                osp_oer,
                osp_cit,
                osp_prp,
                osp_opr,
                osp_sco,
                osp_oap,
                osp_mat,
                osp_dat,
                osp_met) %>%
  estimate_profiles(5) 

osc_lpa%>%
  plot_profiles(ci = NULL,
                sd = F,
                add_line = T,
                rawdata = F)


lpa_data <- get_data(osc_lpa)[,c(16:21)]
lpa_data <- cbind(osc, lpa_data)



# fit_multinom <- multinom(Class ~ para + disc, lpa_data)
# tidy(fit_multinom) %>%
#   kbl() %>%
#   kable_styling()


############################################################################# #
####  PREDICTORS  #############################################################
############################################################################# #

# predicting conditional probability to be in class 1
fit_predict_class1 <- lm(scale(CPROB1) ~ as.factor(para) + as.factor(disc), data = lpa_data)
summary(fit_predict_class1)
# pairwise comparisons
fit_predict_class1_em <- emmeans(fit_predict_class1, "para")  
pwpm(fit_predict_class1_em)
fit_predict_class1_em <- emmeans(fit_predict_class1, "disc")  
pwpm(fit_predict_class1_em)

# predicting conditional probability to be in class 2
fit_predict_class2 <- lm(scale(CPROB2) ~ as.factor(para) + as.factor(disc), data = lpa_data)
summary(fit_predict_class2)
# pairwise comparisons
fit_predict_class2_em <- emmeans(fit_predict_class2, "para")
pwpm(fit_predict_class2_em)
fit_predict_class2_em <- emmeans(fit_predict_class2, "disc")  
pwpm(fit_predict_class2_em)

# predicting conditional probability to be in class 3
fit_predict_class3 <- lm(scale(CPROB3) ~ as.factor(para) + as.factor(disc), data = lpa_data)
summary(fit_predict_class3)
# pairwise comparisons
fit_predict_class3_em <- emmeans(fit_predict_class3, "para")  
pwpm(fit_predict_class3_em)
fit_predict_class3_em <- emmeans(fit_predict_class3, "disc")  
pwpm(fit_predict_class3_em)

# predicting conditional probability to be in class 4
fit_predict_class4 <- lm(scale(CPROB4) ~ as.factor(para) + as.factor(disc), data = lpa_data)
summary(fit_predict_class4)
# pairwise comparisons
fit_predict_class4_em <- emmeans(fit_predict_class4, "para")  
pwpm(fit_predict_class4_em)
fit_predict_class4_em <- emmeans(fit_predict_class4, "disc")  
pwpm(fit_predict_class4_em)

# predicting conditional probability to be in class 5
fit_predict_class5 <- lm(scale(CPROB5) ~ as.factor(para) + as.factor(disc), data = lpa_data)
summary(fit_predict_class5)
# pairwise comparisons
fit_predict_class5_em <- emmeans(fit_predict_class5, "para")  
pwpm(fit_predict_class5_em)
fit_predict_class5_em <- emmeans(fit_predict_class5, "disc")  
pwpm(fit_predict_class5_em)




## Model summaries: tables
modelsummary::modelsummary(list(fit_predict_class1, 
                                fit_predict_class2, 
                                fit_predict_class3, 
                                fit_predict_class4, 
                                fit_predict_class5))

stargazer::stargazer(fit_predict_class1, 
                     fit_predict_class2, 
                     fit_predict_class3, 
                     fit_predict_class4, 
                     fit_predict_class5,
                     type = "text",
                     # star.cutoffs = c(.05, .01, .001),
                     report = "vcsp")

sjPlot::tab_model(fit_predict_class1, 
                  fit_predict_class2, 
                  fit_predict_class3, 
                  fit_predict_class4, 
                  fit_predict_class5)



############################################################################# #
####  PLOTS       #############################################################
############################################################################# #

### donut plots ###############################################################

### disc ###
lpa_disc <- lpa_data %>%
  group_by(Class, disc) %>%
  dplyr::summarize(disc = disc[1],
                   disc_n = n()) %>%
  ungroup()

disc_N <- lpa_disc %>%
  group_by(Class) %>%
  summarize(N = sum(disc_n))

lpa_disc <- lpa_disc %>%
  mutate(fraction = case_when(
    Class == 1 ~ disc_n/disc_N$N[1],
    Class == 2 ~ disc_n/disc_N$N[2],
    Class == 3 ~ disc_n/disc_N$N[3],
    Class == 4 ~ disc_n/disc_N$N[4],
    Class == 5 ~ disc_n/disc_N$N[5])) %>%
  group_by(Class) %>%
  mutate(ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(disc, "\n", round(fraction * 100), "%"))



# plot for class 1
ggplot(lpa_disc%>%filter(Class==1), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=disc)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=disc), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Disciplines in class 1")


# plot for class 2
ggplot(lpa_disc%>%filter(Class==2), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=disc)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=disc), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  ggtitle("Disciplines in class 2")

# plot for class 3
ggplot(lpa_disc%>%filter(Class==3), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=disc)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=disc), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  ggtitle("Disciplines in class 3")

# plot for class 4
ggplot(lpa_disc%>%filter(Class==4), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=disc)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=disc), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  ggtitle("Disciplines in class 4")

# plot for class 5
ggplot(lpa_disc%>%filter(Class==5), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=disc)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=disc), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  ggtitle("Disciplines in class 5")







### disc ###
lpa_para <- lpa_data %>%
  group_by(Class, para) %>%
  dplyr::summarize(para = para[1],
                   para_n = n()) %>%
  ungroup()

para_N <- lpa_para %>%
  group_by(Class) %>%
  summarize(N = sum(para_n))

lpa_para <- lpa_para %>%
  mutate(fraction = case_when(
    Class == 1 ~ para_n/para_N$N[1],
    Class == 2 ~ para_n/para_N$N[2],
    Class == 3 ~ para_n/para_N$N[3],
    Class == 4 ~ para_n/para_N$N[4],
    Class == 5 ~ para_n/para_N$N[5])) %>%
  group_by(Class) %>%
  mutate(ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(para, "\n", round(fraction * 100), "%"))



# plot for class 1
tmp1 <-
ggplot(lpa_para%>%filter(Class==1), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=para)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=para), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Paradigms in class 1")


# plot for class 2
tmp2 <-
ggplot(lpa_para%>%filter(Class==2), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=para)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=para), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  ggtitle("Paradigms in class 2")

# plot for class 3
tmp3 <-
ggplot(lpa_para%>%filter(Class==3), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=para)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=para), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  ggtitle("Paradigms in class 3")

# plot for class 4
tmp4 <-
ggplot(lpa_para%>%filter(Class==4), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=para)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=para), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  ggtitle("Paradigms in class 4")

# plot for class 5
ggplot(lpa_para%>%filter(Class==5), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=para)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=para), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  ggtitle("Paradigms in class 5")



### bar plots #################################################################

# count
ggplot(lpa_data, aes(x = disc, fill = as.factor(Class))) +
  geom_bar(position = "dodge")


# percent
ggplot(lpa_data, aes(x = disc, fill = as.factor(Class))) +
  geom_bar(position = "dodge")



### sankey plots ##############################################################

library(ggsankey)

tmp <- lpa_data %>%
  ggsankey::make_long(disc, Class)


ggplot(tmp, aes(x = x, 
                next_x = next_x, 
                node = node, 
                next_node = next_node,
                fill = factor(node))) +
  geom_sankey() +
  theme_sankey(base_size = 16)

