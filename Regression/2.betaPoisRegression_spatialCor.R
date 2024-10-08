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
predict_s$MF_av[which(predict_s$MF_av == 0)] <- 0.0001
predict_s$MF_av[which(predict_s$MF_av == 1)] <- 0.9999
# data_sampled$pos <- numFactor(data_sampled$x, data_sampled$y)
# data_sampled$group <- factor(rep(1, nrow(data_sampled)))
class(predict_s$type)
predict_s$type[which(predict_s$type == 1)] <- "Broadleaved forest"
predict_s$type[which(predict_s$type == 2)] <- "Coniferous forest"
predict_s$type <- as.factor(predict_s$type)

#### Poisson model####
# Poisson WITHOUT intercept.
mod_pois <- gam(
  MF_singleT ~ type + coast + cover + elevation + eastness +
    northness + relative_elevation + slope + TWI +
    s(x, y, bs = "gp", m = 2) - 1,
  family = poisson(link = "log"),
  data = data_sample.mf02
)
summary(mod_pois)
simulationOutput <- simulateResiduals(fittedModel = mod_pois)
plot(simulationOutput)
testOutliers(simulationOutput, type = "bootstrap") # outliers are not significant.
plotResiduals(simulationOutput, form = data_1000$coast)
plotResiduals(simulationOutput, form = data_1000$cover) # Looks better
plotResiduals(simulationOutput, form = data_sampled$elevation)
plotResiduals(simulationOutput, form = data_sampled$eastness)
plotResiduals(simulationOutput, form = data_sampled$northness)
plotResiduals(simulationOutput, form = data_sampled$relative_elevation)
plotResiduals(simulationOutput, form = data_sampled$slope)

# Poisson with intercept
mod_pois_intercep <- gam(
  MF_0.8T ~ type + cover + coast + elevation + relative_elevation +
    slope + eastness + northness + TWI +
    s(x, y, bs = "gp", m = 2),
  family = poisson(link = "log"),
  data = predict_s
)
summary(mod_pois_intercep)
simulationOutput <- simulateResiduals(fittedModel = mod_pois_intercep)
plot(simulationOutput)
save(mod_pois_intercep,
  file = "I:/DATA/output/MF_origi/Poisson_MF_singleT_orig.RData"
)

#### Use 'gam()' for faster beta regression accounting spatial autocorrelation.####
# beta WITHOUT intercept.
mod_beta <- gam(
  MF_av ~ type + coast + cover + elevation + eastness +
    northness + relative_elevation + slope + TWI +
    s(x, y, bs = "gp", m = 2) - 1,
  family = betar(link = "logit"),
  data = data_1000
)
summary(mod_beta)
simulationOutput <- simulateResiduals(fittedModel = mod_beta)
plot(simulationOutput)

# beta with intercept
mod_beta_intercep <- gam(
  MF_av ~   type + cover + coast + elevation + relative_elevation +
    slope + eastness + northness + TWI +
    s(x, y, bs = "gp", m = 2),
  family = betar(link = "logit"),
  data = predict_s
)
summary(mod_beta_intercep)
simulationOutput <- simulateResiduals(fittedModel = mod_beta_intercep)
plot(simulationOutput)
save(mod_beta_intercep, file = "I:/DATA/output/MF_origi/MF_avBeta_orig.RData")

#### Test the predictor saperately####
# for cover
mod_cover <- gam(
  MF_av ~ type + coast + s(cover) + elevation + eastness +
    northness + relative_elevation + slope + TWI +
    s(x, y, bs = "gp", m = 2) - 1,
  family = betar(link = "logit"),
  data = data_sampled
)
summary(mod_cover)
simulationOutput <- simulateResiduals(fittedModel = mod_cover)
plot(simulationOutput)

# For elevation
mod_elevation <- gam(
  MF_av ~ type + coast + cover + s(elevation) + eastness +
    northness + relative_elevation + slope + TWI +
    s(x, y, bs = "gp", m = 2) - 1,
  family = betar(link = "logit"),
  data = data_sampled
)
summary(mod_elevation)
simulationOutput <- simulateResiduals(fittedModel = mod_elevation)
plot(simulationOutput)

# eastness
mod_east <- gam(
  MF_av ~ type + coast + cover + elevation + s(eastness) +
    northness + relative_elevation + slope + TWI +
    s(x, y, bs = "gp", m = 2) - 1,
  family = betar(link = "logit"),
  data = data_sampled
)
summary(mod_east)
simulationOutput <- simulateResiduals(fittedModel = mod_east)
plot(simulationOutput)

# for northness
mod_north <- gam(
  MF_av ~ type + coast + cover + elevation + eastness +
    s(northness) + relative_elevation + slope + TWI +
    s(x, y, bs = "gp", m = 2) - 1,
  family = betar(link = "logit"),
  data = data_sampled
)
summary(mod_north)
simulationOutput <- simulateResiduals(fittedModel = mod_north)
plot(simulationOutput)

# reletive elevation
mod_re_elev <- gam(
  MF_av ~ type + coast + cover + elevation + eastness +
    northness + s(relative_elevation) + slope + TWI +
    s(x, y, bs = "gp", m = 2) - 1,
  family = betar(link = "logit"),
  data = data_sampled
)
summary(mod_re_elev)
simulationOutput <- simulateResiduals(fittedModel = mod_re_elev)
plot(simulationOutput)

# slope
mod_slope <- gam(
  MF_av ~ type + coast + cover + elevation + eastness +
    northness + relative_elevation + s(slope) + TWI +
    s(x, y, bs = "gp", m = 2) - 1,
  family = betar(link = "logit"),
  data = data_sampled
)
summary(mod_slope)
simulationOutput <- simulateResiduals(fittedModel = mod_slope)
plot(simulationOutput)
