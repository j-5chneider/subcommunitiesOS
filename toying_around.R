library(xfun)
library(here)
library(tidyverse)
library(poLCA)
library(ggridges)
library(formr)
library(viridis)
library(ggdist)
library(ggpubr)
library(naniar)
library(psych)
library(kableExtra)

formr_connect(keyring = "formr_juergen")

osc <- formr_raw_results(survey_name = 'osc') %>%
  dplyr::filter(!str_detect(session, 'XXX') & !is.na(session) & !is.na(ended))

osc <- osc[-c(1:12),]

# # model for later lca
# f <- with(osc, cbind(osp_cit,
#                          osp_prp,
#                          osp_pre,
#                          osp_met,
#                          osp_sof,
#                          osp_mat,
#                          osp_cod,
#                          osp_dat,
#                          osp_oer,
#                          osp_opr,
#                          osp_oap,
#                          osp_oso,
#                          osp_sco) ~ 1)


# match data from discipline to data set
osc_0 <- formr_raw_results(survey_name = 'osc_0') %>%
  dplyr::select(session, disc) %>%
  dplyr::mutate(disc = str_sub(disc, 1, 3))
osc <- left_join(osc, osc_0, by = "session")



## CHECK MISSINGS #############################################################
# delete log variables
osc_miss <- osc %>%
  dplyr::select(osp_prp:disc, -finished)

vis_miss(osc_miss)

############################################################################# #
##### Descriptive Analyses  ###################################################
############################################################################# #

descr <- describe(osc_miss[,1:13])

descr %>%
  round(3) %>%
  kbl() %>%
  kable_styling()

table(osc_miss$para)
table(osc_miss$disc)


############################################################################# #
#####     LCA          ########################################################
############################################################################# #

# run a sequence of models with 1-7 classes and print out the model with the lowest BIC
# model for lca
f <- with(osc, cbind(osp_cit,
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
                     osp_sco) ~ 1)


max_II <- -100000
min_bic <- 100000
entropy<-function (p) sum(-p*log(p)) # defining entropy function

# generate empty data set for model fit results
lca_models_fit <- data.frame(classes = numeric(),
                            bic     = numeric(),
                            aic     = numeric(),
                            Chisq   = numeric(),
                            Gsq     = numeric(),
                            LL      = numeric(),
                            entropy = numeric())

for(i in 1:6){
  lc <- poLCA(f, osc, 
              nclass=i, 
              maxiter=3000, 
              tol=1e-5,
              na.rm=FALSE,  
              nrep=10, 
              verbose=TRUE, 
              calc.se=TRUE)
  
  if(lc$bic < min_bic){  # check if bic of current model is lower than lowest bic so far
    min_bic <- lc$bic    # save bic of current model as lowest bic
    LCA_best_model<-lc   # save current model as best model
  }
  
  # calculating the model's entropy by hand
  # adapted from Daniel Oberski (slide 66)
  # http://daob.nl/wp-content/uploads/2015/07/ESRA-course-slides.pdf
  error_prior <- entropy(lc$P) # class proportions of model
  error_post <- mean(apply(lc$posterior,1, entropy),na.rm = TRUE)
  model_entropy <- round(((error_prior-error_post) / error_prior),3)
  
  # saving the model's fit indices
  lca_models_fit <- lca_models_fit %>%
    add_row(classes = i,
            bic     = lc$bic,
            aic     = lc$aic,
            Chisq   = lc$Chisq,
            Gsq     = lc$Gsq,
            LL      = lc$llik,
            entropy = model_entropy)
}


# extracting best model
LCA_best_model

# saving best model
osc <- data.frame(osc, class = LCA_best_model$predclass)

# fit indices of all models
lca_models_fit %>%
  round(3) %>%
  kbl() %>%
  kable_styling()


#####     LCA - PLOTTING  #####################################################
# plotting density of participants overall as ridges
osc_plot <- osc %>%
  # dplyr::select(-c(disc, para)) %>%   # as was in preregistration
  dplyr::select(-c(finished, para, session, created, modified, ended, expired, disc)) %>%  # deviation from prereg: now added more columns to exclude
  pivot_longer(1:13, names_to = "names", values_to = "value") %>%
  mutate(names = case_when(
    names == "osp_cit" ~ "Citizen Science",
    names == "osp_cod" ~ "Open Code",
    names == "osp_dat" ~ "Open Data",
    names == "osp_mat" ~ "Open Materials",
    names == "osp_met" ~ "Open Methodology",
    names == "osp_oap" ~ "Open Access",
    names == "osp_oer" ~ "OER",
    names == "osp_opr" ~ "Open Peer Review",
    names == "osp_oso" ~ "Open Source",
    names == "osp_pre" ~ "Preregistration",
    names == "osp_prp" ~ "Public Project Plan",
    names == "osp_sco" ~ "Science Communication",
    names == "osp_sof" ~ "Open Software"
  ))

# means and SDs overall
osc_plot_msd <- osc_plot %>%
  group_by(names) %>%
  dplyr::summarize(mean = mean(value),
                   sd = sd(value))

osc_plot_msd[order(osc_plot_msd$mean, decreasing = T),"names"]

osc_plot$names <- factor(osc_plot$names, levels = c("Open Methodology",
                                                    "Open Access",
                                                    "Open Data",
                                                    "Open Materials",
                                                    "Science Communication",
                                                    "Open Code",
                                                    "Open Peer Review",
                                                    "Public Project Plan",
                                                    "Open Software",
                                                    "OER",
                                                    "Preregistration",
                                                    "Citizen Science",
                                                    "Open Source"))



# plotting density within items over all classes
ggplot(osc_plot, aes(x=value, y=names, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 0.9, bandwidth = .5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, color = "red", fill = "red") +
  scale_fill_viridis_c(values = c(.25,.75), option = "D") +
  scale_x_continuous(limits=c(1,4), expand=c(0,0), breaks = c(1,2,3,4),
                     labels = c("not applicable\nat all", "", "", "highly\napplicable")) +
  theme_ridges()



# plotting density of participants in each class as ridges
ggplot(osc_plot, aes(x=value, y=names, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 0.9, bandwidth = .5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, color = "red", fill = "red") +
  scale_fill_viridis_c(values = c(.25,.75), option = "D") +
  scale_x_continuous(limits=c(1,4), expand=c(0,0), breaks = c(1,2,3,4),
                     labels = c("not applicable at all", "", "", "highly applicable")) +
  xlab("") +
  ylab("") +
  theme_ridges() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=1)) +
  facet_wrap(~class, labeller = "label_both")


############################################################################# #
#####     LCA - with covariates     ###########################################
############################################################################# #

f3_cov <-  with(osc, cbind(osp_cit,
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
                           osp_sco) ~ disc + para)

lca_with_covariates <- poLCA(f3_cov, osc, 
                             nclass=3,     # 3 classes as resulting from analyses above
                             maxiter=3000, 
                             tol=1e-5,
                             na.rm=FALSE,  
                             nrep=10, 
                             verbose=TRUE, 
                             calc.se=TRUE)
  




### donut plots ############### #
# disc
osc_disc <- osc %>%
  group_by(class, disc) %>%
  summarize(disc = disc[1],
            disc_n = n()) %>%
  ungroup()

disc_N <- osc_disc %>%
  group_by(class) %>%
  summarize(N = sum(disc_n))

osc_disc <- osc_disc %>%
  mutate(fraction = case_when(
    class == 1 ~ disc_n/disc_N$N[1],
    class == 2 ~ disc_n/disc_N$N[2],
    class == 3 ~ disc_n/disc_N$N[3])) %>%
  group_by(class) %>%
  mutate(ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(disc, "\n", round(fraction * 100), "%"))



# plot for class 1
ggplot(osc_disc%>%filter(class==1), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=disc)) +
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
ggplot(osc_disc%>%filter(class==2), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=disc)) +
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
ggplot(osc_disc%>%filter(class==3), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=disc)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=disc), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  ggtitle("Disciplines in class 3")




# para
osc_para <- osc %>%
  group_by(class, para) %>%
  summarize(para = para[1],
            para_n = n()) %>%
  ungroup()

para_N <- osc_para %>%
  group_by(class) %>%
  summarize(N = sum(para_n))

osc_para <- osc_para %>%
  mutate(fraction = case_when(
    class == 1 ~ para_n/para_N$N[1],
    class == 2 ~ para_n/para_N$N[2],
    class == 3 ~ para_n/para_N$N[3])) %>%
  group_by(class) %>%
  mutate(ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(para, "\n", round(fraction * 100), "%"))

#plot for class 1
ggplot(osc_para%>%filter(class==1), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=para)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=para), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Paradigms in class 1")

#plot for class 2
ggplot(osc_para%>%filter(class==2), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=para)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=para), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Paradigms in class 2")

#plot for class 3
ggplot(osc_para%>%filter(class==3), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=para)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=para), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Paradigms in class 2")

### Weitere Plots: 
# Mittelwertunterschiede (dann inkl. Boxplots)

# Cleveland Dotplots ##################################################
# create long data for dots
osc_mwPoint <- osc %>%
  dplyr::select(osp_prp:osp_cit, class) %>%
  pivot_longer(1:13, names_to = "variable", values_to = "value") %>%
  mutate(names = case_when(
    names == "osp_cit" ~ "Citizen Science",
    names == "osp_cod" ~ "Open Code",
    names == "osp_dat" ~ "Open Data",
    names == "osp_mat" ~ "Open Materials",
    names == "osp_met" ~ "Open Methodology",
    names == "osp_oap" ~ "Open Access",
    names == "osp_oer" ~ "OER",
    names == "osp_opr" ~ "Open Peer Review",
    names == "osp_oso" ~ "Open Source",
    names == "osp_pre" ~ "Preregistration",
    names == "osp_prp" ~ "Public Project Plan",
    names == "osp_sco" ~ "Science Communication",
    names == "osp_sof" ~ "Open Software"
  ))

osc_mwPoint$variable <- factor(osc_mwPoint$variable, 
                               # levels=c("osp_met", "osp_oap", "osp_dat", "osp_mat",
                               #          "osp_sco", "osp_cod", "osp_opr", "osp_prp",
                               #          "osp_sof", "osp_oer", "osp_pre", "osp_cit", "osp_oso"))
                               levels=c("osp_met", "osp_dat", "osp_cod", "osp_mat",
                                        "osp_oap","osp_sof", "osp_opr", "osp_prp",
                                        "osp_sco", "osp_oer", "osp_oso", "osp_cit", "osp_pre"))
                               # levels=c("osp_sco", "osp_met", "osp_oap",
                               #          "osp_sof", "osp_mat", "osp_opr",
                               #          "osp_prp", "osp_cit", "osp_dat", 
                               #          "osp_pre", "osp_cod", "osp_oer", "osp_oso"))
                               

# create means for lines
osc_mwLines <- osc %>%
  dplyr::select(osp_prp:osp_cit, class) %>%
  group_by(class) %>%
  dplyr::summarize(across(everything(), mean)) %>%
  pivot_longer(2:14, names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  dplyr::summarize(min = min(value),
                   max = max(value))

osc_mwLines$variable <- factor(osc_mwLines$variable, 
                               # levels=c("osp_met", "osp_oap", "osp_dat", "osp_mat",
                               #          "osp_sco", "osp_cod", "osp_opr", "osp_prp",
                               #          "osp_sof", "osp_oer", "osp_pre", "osp_cit", "osp_oso"))
                               levels=c("osp_met", "osp_dat", "osp_cod", "osp_mat",
                                        "osp_oap","osp_sof", "osp_opr", "osp_prp",
                                        "osp_sco", "osp_oer", "osp_oso", "osp_cit", "osp_pre"))
                               # levels=c("osp_sco", "osp_met", "osp_oap",
                               #          "osp_sof", "osp_mat", "osp_opr",
                               #          "osp_prp", "osp_cit", "osp_dat", 
                               #          "osp_pre", "osp_cod", "osp_oer", "osp_oso"))

                               


# create cleveland plot
osc_mwPoint %>%
  ggplot(aes(x=value, y=variable, color=as.factor(class))) +
    geom_vline(xintercept = 2.5, color = "lightgrey", alpha = .5, size = 1.5, linetype="dotted") +
    geom_segment(data=osc_mwLines, aes(x=min, xend=max, y=variable, yend=variable), 
                 color="lightgrey", size=3, alpha = .6) +
    stat_summary(geom="point", fun="mean", size = 6) +
    scale_colour_viridis_d() +
    scale_x_continuous(limits=c(1,4), expand=c(0,0), breaks = c(1,2,3,4),
                       labels = c("not applicable at all", "", "", "highly applicable")) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    xlab("") +
    ylab("")



# Co-Occurance der Praktiken (insgesamt? innerhalb der Klassen?)
# -> dann müsste sowohl die Occurance als auch nicht-occurance geplottet werden, weil sonst Praktiken, die mit mehreren anderen (statt nur wenigen anderen) Praktiken co-occuren mehr Platz erhalten
# http://download.gsb.bund.de/BIB/global_flow/

# Cord Diagram #############################################################
# https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html
# https://www.r-graph-gallery.com/chord-diagram-interactive.html
osc_cord <- osc %>%
  mutate(across(starts_with("osp_"), ~ ifelse(. <= 2, 0, 1)))

osc_cord_m <- as.matrix(osc_cord[6:18])
osc_cord_m_cr <- crossprod(osc_cord_m)
diag(osc_cord_m_cr) <- 0

# Build the chord diagram:
library(chorddiag)
chorddiag(osc_cord_m_cr)

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/chord_interactive.html"))


# Heatmap ##################################################################
heatmap(osc_cord_m_cr, col = heat.colors(256, alpha = 1),
        Colv = NA, Rowv = NA, xlab="", ylab="", scale="row")

############ #
# zweiter Versuch einer Heatmap
osc_cord_m_cr2 <- as.data.frame(osc_cord_m_cr)
osc_cord_m_cr2$osp1 <- names(osc_cord_m_cr2)

osc_cord_m_cr2 <- osc_cord_m_cr2 %>%
  pivot_longer(1:13, names_to = "osp2", values_to = "value")

ggplot(osc_cord_m_cr2, aes(osp1, osp2, fill= value)) + 
  geom_tile()


# Corrplot #################################################################
library(corrplot)

par(mfrow = c(1, 3))

corrplot(cor(osc[osc$class == 1, 6:18]), method = 'square', 
                   order = 'FPC', type = 'lower', diag = FALSE)
corrplot(cor(osc[osc$class == 2, 6:18]), method = 'square', 
                   order = 'FPC', type = 'lower', diag = FALSE)
corrplot(cor(osc[osc$class == 3, 6:18]), method = 'square', 
                   order = 'FPC', type = 'lower', diag = FALSE)

par(mfrow = c(1, 1))

corrplot(cor(osc[6:18]), method = 'square', 
         order = 'FPC', type = 'lower', diag = FALSE)

# library(ggpubr)
# ggarrange(corrplot(cor1, method = 'square', 
#                    order = 'FPC', type = 'lower', diag = FALSE),
#           corrplot(cor(osc[osc$class == 2, 6:18]), method = 'square', 
#                    order = 'FPC', type = 'lower', diag = FALSE),
#           corrplot(cor(osc[osc$class == 3, 6:18]), method = 'square', 
#                    order = 'FPC', type = 'lower', diag = FALSE),
#           ncol = 3,
#           nrow = 1)




#
# Share von Total participants > disciplines > Classes < research paradigms < Total participants
# diese Chart von beiden Seiten (in einer Chart)
# https://i.redd.it/87iujcxcyhd71.png

############# INFERENCE STATISTICS ############################

library(BayesFactor)
osc$para <- as.factor(osc$para)
osc$disc <- as.factor(osc$disc)

summary(aov(osp_prp ~ para, data = osc))
anovaBF(osp_prp ~ para, data = osc)

summary(aov(osp_pre ~ para, data = osc))
anovaBF(osp_pre ~ para, data = osc)

summary(aov(osp_met ~ para, data = osc))
anovaBF(osp_met ~ para, data = osc)

summary(aov(osp_sof ~ para, data = osc))
anovaBF(osp_sof ~ para, data = osc)

summary(aov(osp_mat ~ para, data = osc))
anovaBF(osp_mat ~ para, data = osc)

summary(aov(osp_cod ~ para, data = osc))
anovaBF(osp_cod ~ para, data = osc)

summary(aov(osp_dat ~ para, data = osc))
anovaBF(osp_dat ~ para, data = osc)

summary(aov(osp_oer ~ para, data = osc))
anovaBF(osp_oer ~ para, data = osc)

summary(aov(osp_opr ~ para, data = osc))
anovaBF(osp_opr ~ para, data = osc)

summary(aov(osp_oap ~ para, data = osc))
anovaBF(osp_oap ~ para, data = osc)

summary(aov(osp_oso ~ para, data = osc))
anovaBF(osp_oso ~ para, data = osc)

summary(aov(osp_sco ~ para, data = osc))
anovaBF(osp_sco ~ para, data = osc)

summary(aov(osp_cit ~ para, data = osc))
anovaBF(osp_cit ~ para, data = osc)





summary(aov(osp_prp ~ disc, data = osc))
anovaBF(osp_prp ~ disc, data = osc)

summary(aov(osp_pre ~ disc, data = osc))
anovaBF(osp_pre ~ disc, data = osc)

summary(aov(osp_met ~ disc, data = osc))
anovaBF(osp_met ~ disc, data = osc)

summary(aov(osp_sof ~ disc, data = osc))
anovaBF(osp_sof ~ disc, data = osc)

summary(aov(osp_mat ~ disc, data = osc))
anovaBF(osp_mat ~ disc, data = osc)

summary(aov(osp_cod ~ disc, data = osc))
anovaBF(osp_cod ~ disc, data = osc)

summary(aov(osp_dat ~ disc, data = osc))
anovaBF(osp_dat ~ disc, data = osc)

summary(aov(osp_oer ~ disc, data = osc))
anovaBF(osp_oer ~ disc, data = osc)

summary(aov(osp_opr ~ disc, data = osc))
anovaBF(osp_opr ~ disc, data = osc)

summary(aov(osp_oap ~ disc, data = osc))
anovaBF(osp_oap ~ disc, data = osc)

summary(aov(osp_oso ~ disc, data = osc))
anovaBF(osp_oso ~ disc, data = osc)

summary(aov(osp_sco ~ disc, data = osc))
anovaBF(osp_sco ~ disc, data = osc)

summary(aov(osp_cit ~ disc, data = osc))
anovaBF(osp_cit ~ disc, data = osc)


############# PLOTTING BY ... ############################
############# DISCIPLINE      ###

os_var <- names(osc)[6:18]

for (varlist in os_var) {

  p1 <- osc %>%
        ggplot(aes(x= .[,varlist], y = para, fill = para, color = para)) +
          stat_halfeye(adjust = 1.5, point_colour = NA) +
          stat_summary(fun = "mean", geom = "point", shape = 25,
                       color = "#293133", fill = "white", size = 3,
                       position = position_nudge(y=.08)) +
          scale_color_viridis_d() +
          scale_fill_viridis_d() +
          xlab(varlist)
  
  print(p1)

}


for (varlist in os_var) {
  
  p2 <- osc %>%
    ggplot(aes(x= .[,varlist], fill = para, color = para)) +
    geom_bar() +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    xlab(case_when(
      varlist == "osp_cit" ~ "Citizen Science",
      varlist == "osp_cod" ~ "Open Code",
      varlist == "osp_dat" ~ "Open Data",
      varlist == "osp_mat" ~ "Open Materials",
      varlist == "osp_met" ~ "Open Methodology",
      varlist == "osp_oap" ~ "Open Access",
      varlist == "osp_oer" ~ "OER",
      varlist == "osp_opr" ~ "Open Peer Review",
      varlist == "osp_oso" ~ "Open Source",
      varlist == "osp_pre" ~ "Preregistration",
      varlist == "osp_prp" ~ "Public Project Plan",
      varlist == "osp_sco" ~ "Science Communication",
      varlist == "osp_sof" ~ "Open Software"
    )) +
    facet_grid(rows =  para ~ disc) +
    theme_light()     # DIESER PLOT + DANN NOCH "RANDSUMMEN"
  
  p3 <- osc %>%
    ggplot(aes(x= .[,varlist], fill = para, color = para)) +
    geom_bar() +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    xlab(case_when(
      varlist == "osp_cit" ~ "Citizen Science",
      varlist == "osp_cod" ~ "Open Code",
      varlist == "osp_dat" ~ "Open Data",
      varlist == "osp_mat" ~ "Open Materials",
      varlist == "osp_met" ~ "Open Methodology",
      varlist == "osp_oap" ~ "Open Access",
      varlist == "osp_oer" ~ "OER",
      varlist == "osp_opr" ~ "Open Peer Review",
      varlist == "osp_oso" ~ "Open Source",
      varlist == "osp_pre" ~ "Preregistration",
      varlist == "osp_prp" ~ "Public Project Plan",
      varlist == "osp_sco" ~ "Science Communication",
      varlist == "osp_sof" ~ "Open Software"
    )) +
    xlab("") +
    ylab("") +
    facet_grid(rows =  para ~ .) +
    theme_light() +
    theme(
      strip.text.y = element_blank()
    )
  
  p4 <- osc %>%
    ggplot(aes(x= .[,varlist])) +
    geom_bar() +
    # scale_color_viridis_d() +
    # scale_fill_viridis_d() +
    xlab(case_when(
      varlist == "osp_cit" ~ "Citizen Science",
      varlist == "osp_cod" ~ "Open Code",
      varlist == "osp_dat" ~ "Open Data",
      varlist == "osp_mat" ~ "Open Materials",
      varlist == "osp_met" ~ "Open Methodology",
      varlist == "osp_oap" ~ "Open Access",
      varlist == "osp_oer" ~ "OER",
      varlist == "osp_opr" ~ "Open Peer Review",
      varlist == "osp_oso" ~ "Open Source",
      varlist == "osp_pre" ~ "Preregistration",
      varlist == "osp_prp" ~ "Public Project Plan",
      varlist == "osp_sco" ~ "Science Communication",
      varlist == "osp_sof" ~ "Open Software"
    )) +
    xlab("") +
    ylab("") +
    facet_grid(rows =  . ~ disc) +
    theme_light() +
    theme(
      strip.text.x = element_blank()
    )
  
  p0 <- ggplot() +
          theme_void()# empty plot for placeholder in ggarrange
  
  ggarrange(p3, p2, 
            p0, p4, 
            ncol=2,
            nrow=2,
            align = "hv",
            widths = c(1,5),
            heights = c(4,1.5),
            common.legend = TRUE)
  
}

