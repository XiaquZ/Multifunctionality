library(ggeffects)
library(magrittr)
library(mgcv)
library()
# load the model
# load("I:/DATA/output/MF/models/MF_avBeta_interc.rda")


load("I:/DATA/output/MF/standardized10000samples_xy.RData")
data_sampled <- data.sampled
# change min and max values for beta regression model.
min(data_sampled$MF_av)
max(data_sampled$MF_av)
data_sampled$MF_av[which(data_sampled$MF_av == 0)] <- 0.0001
data_sampled$MF_av[which(data_sampled$MF_av == 1)] <- 0.9999
levels(data_sampled$type)[1] <- "Broadleaved forest"
levels(data_sampled$type)[2] <- "Coniferous forest"
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
head(data_sampled)
save(data_sampled, file = "I:/DATA/output/MF/10000samples_forBetaR.rda")
# beta with intercept
mod_beta_intercep <- gam(
    MF_av ~ type + coast + cover + elevation + eastness +
        northness + relative_elevation + slope + TWI +
        s(x, y, bs = "gp", m = 2),
    family = betar(link = "logit"),
    data = data_sampled
)
summary(mod_beta_intercep)
save(mod_beta_intercep,
  file = "I:/DATA/output/MF/models/MF_avBeta_interc_v2.rda"
)
## Use forest type = 2 as reference in the model for marginal effects.
# mod_beta_ref2 <- gam(
#    MF_av ~ relevel(type, ref = 2) + coast + cover + elevation + eastness +
#        northness + relative_elevation + slope + TWI +
#        s(x, y, bs = "gp", m = 2),
#    family = betar(link = "logit"),
#    data = data_sampled
# )
# summary(mod_beta_ref2)

#### Create panel plots for marginal effects for each predictor.####

# forest type
# dat <- predict_response(mod_beta_intercep, "type", margin = "empirical") ##Take a long time
dat_type <- predict_response(mod_beta_intercep, "type")
dat_type
plot(dat_type)
save(dat_type, file = "I:/DATA/output/MF/models/forestType_margin.rda")

# coast
dat_coast <- predict_response(mod_beta_intercep, "coast")
dat_coast
plot(dat_coast)
save(dat_coast, file = "I:/DATA/output/MF/models/coast_margin.rda")

# forest cover
dat_cover <- predict_response(mod_beta_intercep, "cover")
dat_cover
plot(dat_cover, colors = "blue")

# elevation
dat_elevation <- predict_response(mod_beta_intercep, "elevation")
dat_elevation
plot(dat_elevation)

# eastness
dat_eastness <- predict_response(mod_beta_intercep, "eastness")
dat_eastness
plot(dat_eastness)

# northness
dat_northness <- predict_response(mod_beta_intercep, "northness")
dat_northness
plot(dat_northness)

# relative_elevation
dat_relalevation <- predict_response(mod_beta_intercep, "relative_elevation")
dat_relalevation
plot(dat_relalevation)

# slope
dat_slope <- predict_response(mod_beta_intercep, "slope")
dat_slope
plot(dat_slope)

# TWI
dat_twi <- predict_response(mod_beta_intercep, "TWI")
dat_twi
plot(dat_twi)
summary(mod_beta_intercep)
