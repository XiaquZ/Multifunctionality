library(ggeffects)
library(magrittr)
library(mgcv)
library()
# load the model
#load("I:/DATA/output/MF/models/MF_avBeta_interc.rda")
data_sampled <- data.sampled

load("I:/DATA/output/MF/standardized10000samples_xy.RData")
# change min and max values for beta regression model.
min(data_sampled$MF_av)
max(data_sampled$MF_av)
data_sampled$MF_av[which(data_sampled$MF_av == 0)] <- 0.0001
data_sampled$MF_av[which(data_sampled$MF_av == 1)] <- 0.9999
mfav <- unique(data_sampled$MF_av)
# To fixed error that predictors become matrix instead of 'numeric' after scaled
data_sampled$coast <- c(scale(data_sampled$coast))
data_sampled$cover <- c(scale(data_sampled$cover))
data_sampled$elevation <- c(scale(data_sampled$elevation))
data_sampled$eastness <- c(scale(data_sampled$eastness))
data_sampled$northness <- c(scale(data_sampled$northness))
data_sampled$relative_elevation <- c(scale(data_sampled$relative_elevation))
data_sampled$slope <- c(scale(data_sampled$slope))
data_sampled$TWI <- c(scale(data_sampled$TWI))
class(data_sampled)
# beta with intercept
mod_beta_intercep <- gam(
    MF_av ~ type + coast + cover + elevation + eastness +
        northness + relative_elevation + slope + TWI +
        s(x, y, bs = "gp", m = 2),
    family = betar(link = "logit"),
    data = data_sampled
)
summary(mod_beta_intercep)

##Use forest type = 2 as reference in the model for marginal effects.
#mod_beta_ref2 <- gam(
#    MF_av ~ relevel(type, ref = 2) + coast + cover + elevation + eastness +
#        northness + relative_elevation + slope + TWI +
#        s(x, y, bs = "gp", m = 2),
#    family = betar(link = "logit"),
#    data = data_sampled
#)
#summary(mod_beta_ref2)
####Create panel plots for marginal effects for each predictor.####

#forest type
dat <- predict_response(mod_beta_intercep, "type", margin = "empirical")
dat_02 <- predict_response(mod_beta_intercep, "type")
plot(dat)
plot(dat, one_plot = TRUE)
plot(dat_02)
##change type ref=2
#dat_03 <- predict_response(mod_beta_ref2, "type")
#plot(dat_03)
#coast
dat_coast <- predict_response(mod_beta_intercep, "coast")
dat_coast
plot(dat_coast)
#dat_coast02 <- predict_response(mod_beta_ref2, "coast")
##Error: in relevel.factor(type, ref = 2) : ref = 2 must be in 1L:1

#forest cover
dat_cover <- predict_response(mod_beta_intercep, "cover")
dat_cover
plot(dat_cover, colors = "blue")

#elevation
dat_elevation <- predict_response(mod_beta_intercep, "elevation")
dat_elevation
plot(dat_elevation)

#eastness
dat_eastness <- predict_response(mod_beta_intercep, "eastness")
dat_eastness
plot(dat_eastness)

#northness
dat_northness <- predict_response(mod_beta_intercep, "northness")
dat_northness
plot(dat_northness)

#relative_elevation
dat_relalevation <- predict_response(mod_beta_intercep, "relative_elevation")
dat_relalevation
plot(dat_relalevation)

#slope
dat_slope <- predict_response(mod_beta_intercep, "slope")
dat_slope
plot(dat_slope)

#TWI
dat_twi <- predict_response(mod_beta_intercep, "TWI")
dat_twi
plot(dat_twi)
