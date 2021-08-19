library(formr)
library(tidyverse)
library(truncnorm)
library(ggridges)

formr_connect(email = 'juergen.schneider@uni-tuebingen.de', 
              rstudioapi::askForPassword("DemJuergenseinSuperpasswort") 
) # find password in video https://www.youtube.com/watch?v=dQw4w9WgXcQ


# get surveys
osc_raw <- formr_results(survey_name = 'osc') %>%
  dplyr::select(-c(session, created, modified, ended, expired))

# define 2 vars as character
osc_raw$disc <- as.character(osc_raw$disc)
osc_raw$para <- as.character(osc_raw$para)


# generate toy data
osc_toy <- osc_raw %>%
  add_row(osp_cit = round(rtruncnorm(300, 1, 4, 2.5, 1)),
          osp_prp = round(rtruncnorm(300, 1, 4, 2.5, 1)),
          osp_pre = round(rtruncnorm(300, 1, 4, 2.5, 1)),
          osp_met = round(rtruncnorm(300, 1, 4, 2.5, 1)),
          osp_sof = round(rtruncnorm(300, 1, 4, 2.5, 1)),
          osp_mat = round(rtruncnorm(300, 1, 4, 2.5, 1)),
          osp_cod = round(rtruncnorm(300, 1, 4, 2.5, 1)),
          osp_dat = round(rtruncnorm(300, 1, 4, 2.5, 1)),
          osp_oer = round(rtruncnorm(300, 1, 4, 2.5, 1)),
          osp_opr = round(rtruncnorm(300, 1, 4, 2.5, 1)),
          osp_oap = round(rtruncnorm(300, 1, 4, 2.5, 1)),
          osp_oso = round(rtruncnorm(300, 1, 4, 2.5, 1)),
          osp_sco = round(rtruncnorm(300, 1, 4, 2.5, 1)),
          disc = rep(c("nat", "eng", "med", "agr", "soc", "hum"), 50),
          para = rep(c("qual", "quan", "mixe", "none"), 75))




################################################################### #
#### CLUSTER ANALYSIS                                            ####
################################################################### #

# Determine number of clusters
wss <- (nrow(osc_toy%>%dplyr::select(-c(disc, para)))-1)*sum(apply(osc_toy%>%dplyr::select(-c(disc, para)),2,var))
for (i in 2:15) wss[i] <- sum(kmeans(osc_toy%>%dplyr::select(-c(disc, para)),
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


# K-Means Cluster Analysis
fit <- kmeans(osc_toy%>%dplyr::select(-c(disc, para)), 2) # 2 cluster solution
# get cluster means
aggregate(osc_toy%>%dplyr::select(-c(disc, para)),by=list(fit$cluster),FUN=mean)
# append cluster assignment
osc_toy <- data.frame(osc_toy, cluster = fit$cluster) 


#### Plot
library(factoextra)
fviz_cluster(fit, 
             data = osc_toy%>%dplyr::select(-c(cluster, disc, para)),
             #palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)



## donut plot
# summarize data
osc_toy_sum <- osc_toy %>%
  dplyr::select(disc, para, cluster) %>%
  group_by(cluster) %>%
  summarize()

# Compute percentages
osc_toy$fraction <- osc_toy$count / sum(osc_toy$count)

# Compute the cumulative percentages (top of each rectangle)
osc_toy$ymax <- cumsum(osc_toy$fraction)

# Compute the bottom of each rectangle
osc_toy$ymin <- c(0, head(osc_toy$ymax, n=-1))

# Compute label position
osc_toy$labelPosition <- (osc_toy$ymax + osc_toy$ymin) / 2

# Compute a good label
osc_toy$label <- paste0(osc_toy$category, "\n value: ", osc_toy$count)

# Make the plot
ggplot(osc_toy, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=6) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

################################################################### #
#### CLUSTER ANALYSIS in high dimensions                         ####
################################################################### #
# taken from http://www.sthda.com/english/wiki/wiki.php?id_contents=7940

library(dbscan)

# finding out optimal value for eps (epsilon)
dbscan::kNNdistplot(osc_toy%>%dplyr::select(-c(disc, para)), k =  2)
abline(h = 4.2, lty = 2)

# for (i in seq(2.5, 5, by=.1)) {
#   tmp <- dbscan(osc_toy%>%dplyr::select(-c(disc, para)),
#                 eps = i)
#   
#   print(table(tmp$cluster)) 
# }

dbscan(osc_toy%>%dplyr::select(-c(disc, para)),
       minPts = 2,
       eps = 4.2)

################################################################### #
#### LATENT CLASS ANALYSIS                                       ####
################################################################### #
# https://statistics.ohlsen-web.de/latent-class-analysis-polca/
# Whereabouts of people in the classes when changing class number:
# https://statistics.ohlsen-web.de/whereabouts-lca/

library(poLCA)

# define function
f <- with(osc_toy, cbind(osp_cit,
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
                         osp_sco) ~ 1) # or ~ disc + para if with class predictors
                                       # BUT: predictors influence the outcome as well


# run a sequence of models with 1-10 classes and print out the model with the lowest BIC
max_II <- -100000
min_bic <- 100000
for(i in 2:7){
  lc <- poLCA(f, osc_toy, 
              nclass=i, 
              maxiter=3000, 
              tol=1e-5, 
              na.rm=FALSE,  
              nrep=10, 
              verbose=TRUE, 
              calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model<-lc
  }
}    	

# extracting best model
LCA_best_model
plot(LCA_best_model)

# extracting assignment to one of the classes
LCA_best_model$predclass

# append class to data set
osc_toy <- data.frame(osc_toy, class = LCA_best_model$predclass)


# plotting density of participants in each class as ridges
osc_toy_plot <- osc_toy %>%
  dplyr::select(-c(disc, para)) %>%
  pivot_longer(1:13, names_to = "names", values_to = "value")

ggplot(osc_toy_plot, aes(x=value, y=names, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 0.9, bandwidth = .5) +
  scale_fill_viridis_c(values = c(.25,.75), option = "D") +
  scale_x_continuous(limits = c(1,4), breaks = c(1,2,3,4)) +
  theme_ridges() +
  facet_wrap(~class)


### donut plots ############### #
# disc
osc_toy_disc <- osc_toy %>%
  group_by(class, disc) %>%
  summarize(disc = disc[1],
            disc_n = n()) %>%
  ungroup()

disc_N <- osc_toy_disc %>%
  group_by(class) %>%
  summarize(N = sum(disc_n))

osc_toy_disc <- osc_toy_disc %>%
  mutate(fraction = case_when(
                        class == 1 ~ disc_n/disc_N$N[1],
                        class == 2 ~ disc_n/disc_N$N[2])) %>%
  group_by(class) %>%
  mutate(ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(disc, "\n share: ", round(fraction * 100), "%"))



# plot for class 1
ggplot(osc_toy_disc%>%filter(class==1), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=disc)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=disc), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

# plot for class 2
ggplot(osc_toy_disc%>%filter(class==2), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=disc)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=disc), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")




# para
osc_toy_para <- osc_toy %>%
  group_by(class, para) %>%
  summarize(para = para[1],
            para_n = n()) %>%
  ungroup()

para_N <- osc_toy_para %>%
  group_by(class) %>%
  summarize(N = sum(para_n))

osc_toy_para <- osc_toy_para %>%
  mutate(fraction = case_when(
    class == 1 ~ para_n/para_N$N[1],
    class == 2 ~ para_n/para_N$N[2])) %>%
  group_by(class) %>%
  mutate(ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(para, "\n share: ", round(fraction * 100), "%"))

#plot for class 1
ggplot(osc_toy_para%>%filter(class==1), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=para)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=para), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

#plot for class 2
ggplot(osc_toy_para%>%filter(class==2), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=para)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=para), size=6) + # x here controls label position (inner / outer)
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")
