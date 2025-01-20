library(DHARMa)
library(glmmTMB)
library(dplyr)
library(mgcv)
library(mgcViz)

load("./Data/10000samples_orig_MF_9predicts.RData")
readRDS("I:/DATA/output/macro_diff.RDS")
#### Fit the model####
###########################
## Test the poisson model###
###########################

# extract 1000 samples.
# data_1000 <- sample_n(data_sample.mf02, 1000)
# class(data_1000$MF_av)
# min(data_1000$MF_av)
# max(data_1000$MF_av)
# data_1000$MF_av <- as.numeric(unlist(data_1000$MF_av))
# data_1000$MF_singleT <- as.numeric(unlist(data_1000$MF_singleT))
# class(data_1000$MF_singleT)
# data_1000$MF_av[which(data_1000$MF_av == 0)] <- 0.0001
# data_1000$MF_av[which(data_sampled$MF_av == 1)] <- 0.9999
# mfav <- unique(data_1000$MF_av)

# use 10,000 data sample
predict_s$MF_av <- as.numeric(unlist(predict_s$MF_av))
predict_s$MF_0.8T <- as.numeric(unlist(predict_s$MF_0.8T))
predict_s <- as_tibble(predict_s)
predict_s

# data_sampled$pos <- numFactor(data_sampled$x, data_sampled$y)
# data_sampled$group <- factor(rep(1, nrow(data_sampled)))
class(predict_s$type)
predict_s$type[which(predict_s$type == 1)] <- "Broadleaved forest"
predict_s$type[which(predict_s$type == 2)] <- "Coniferous forest"
predict_s$type <- as.factor(predict_s$type)

hist(predict_s$MF_0.8T)
hist(predict_s$MF_av)

#### Poisson model####
# Poisson WITHOUT intercept.
mod_pois_no_intercept<- gam(
  MF_0.8T ~ northness * cover * slope +
    cover * type +
    coast +
    elevation +
    relative_elevation +
    TWI +
    s(x, y, bs = "gp", m = 2) - 1,
  family = poisson(link = "log"),
  data = predict_s
)
summary(mod_pois_no_intercept)
simulationOutput_pois <- simulateResiduals(fittedModel = mod_pois_no_intercept)
plot(simulationOutput_pois)
testDispersion(simulationOutput_pois) # overdispersion
par(mfrow = c(1,2))
plotResiduals(simulationOutput_pois, predict_s$relative_elevation)
plotResiduals(simulationOutput_pois, predict_s$elevation)
plotResiduals(simulationOutput_pois, predict_s$type)
# Overdispersion caused by elevation and forest types?

#### Use 'gam()' for faster 
####beta regression accounting spatial autocorrelation.
# Let the minimum value be 0.0001 and the maximum value be 0.9999.
predict_s$MF_av[which(predict_s$MF_av == 0)] <- 0.0001
predict_s$MF_av[which(predict_s$MF_av == 1)] <- 0.9999

# beta WITHOUT intercept.
mod_beta <- gam(
  MF_av ~ northness * cover * slope +
    cover * type +
    coast +
    elevation +
    relative_elevation +
    TWI +
    s(x, y, bs = "gp", m = 2) - 1,
  family = betar(link = "logit"),
  data = predict_s
)
summary(mod_beta)
simulationOutput_beta <- simulateResiduals(fittedModel = mod_beta)
plot(simulationOutput_beta)
testDispersion(simulationOutput_beta) # overdispersion
testZeroInflation(simulationOutput_beta)
plotResiduals(simulationOutput_beta, predict_s$relative_elevation)
plotResiduals(simulationOutput_beta, predict_s$elevation)
plotResiduals(simulationOutput_beta, predict_s$type)
plotResiduals(simulationOutput_beta, predict_s$slope)

# plot the interaction.
library(ggeffects)
cover_type <- predict_response(
    mf_model, c("cover", "type"),
    margin = "mean_mode"
)
cover_type
plot(cover_type)

slo_north_cov <- predict_response(
    mf_model, c("cover", "northness [-1, 0, 1]", "slope[0.0, 45.0]"),
    margin = "mean_mode" # !Check if "mean_mode" is corret here.
)
slo_north_cov
plot(slo_north_cov)