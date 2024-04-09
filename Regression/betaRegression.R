library(DHARMa)
library(glmmTMB)
library(dplyr)
 
load('standardized10000samples_xy 1.RData')
####Fit the model####
###########################
##Test the poisson model###
###########################
data.sampled<-sample_n(data.sampled, 1000)
 
hist(data.sampled$MF_singleT_0.8)
 
min(data.sampled$MF_av)
max(data.sampled$MF_av)
data.sampled$MF_av[which(data$MF_av == 0)]<-0.0001
data.sampled$MF_av[which(data$MF_av == 1)]<-0.9999
 
data.sampled$pos <- numFactor(data.sampled$x, data.sampled$y)
data.sampled$group <- factor(rep(1, nrow(data.sampled)))
 
 
pois <-glmmTMB(MF_singleT_0.8 ~ latitude + coast + cover +elevation + eastness + type + 
                        northness +relative_elevation + slope + exp(pos + 0 | group), 
                         family = "poisson", data = data.sampled)
 
beta <-glmmTMB(MF_av ~ coast + cover +elevation + eastness +
           northness +relative_elevation + slope + exp(pos + 0 | group), 
           family=beta_family(link="logit"),data = data.sampled)