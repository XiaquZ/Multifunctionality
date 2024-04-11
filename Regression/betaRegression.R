library(DHARMa)
library(glmmTMB)
library(dplyr)
library(mgcViz)
load("I:/DATA/output/MF/standardized10000samples_xy.RData")
#### Fit the model####
###########################
## Test the poisson model###
###########################
data_sampled <- sample_n(data.sampled, 1000)
mfav <- unique(data_sampled$MF_av)
hist(data_sampled$MF_singleT_0.8)
hist(data_sampled$MF_av)

min(data_sampled$MF_av)
max(data_sampled$MF_av)
data_sampled$MF_av[which(data_sampled$MF_av == 0)] <- 0.0001
data_sampled$MF_av[which(data_sampled$MF_av == 1)] <- 0.9999

data_sampled$pos <- numFactor(data_sampled$x, data_sampled$y)
data_sampled$group <- factor(rep(1, nrow(data_sampled)))
head(data_sampled)

pois <- glmmTMB(MF_singleT_0.8 ~ latitude + coast + cover + elevation +
    eastness + type + northness + relative_elevation + slope +
    exp(pos + 0 | group), family = "poisson", data = data_sampled)
# the 'exp(pos + 0 | group)' is for the random effects?



hist(data_sampled$MF_av)

install.packages("betareg")
library(betareg)
model_beta <- betareg()

####Use 'gam()' for faster beta regression accounting spatial autocorrelation.####
library(mgcv)
mod <- gam(MF_av ~ type + coast + cover + elevation + eastness +
northness + relative_elevation + slope + TWI
s(x, y, bs = "gp", m = 2) - 1,
family = betar(link = "logit"),
data = data_sampled
)
summary(mod)
simulationOutput <- simulateResiduals(fittedModel = mod)
plot(simulationOutput) 

#for cover
mod_cover <- gam(MF_av ~ type + coast + s(cover) + elevation + eastness +
northness + relative_elevation + slope + TWI +
s(x, y, bs = "gp", m = 2) - 1,
family = betar(link = "logit"),
data = data_sampled
)
summary(mod_cover)
simulationOutput <- simulateResiduals(fittedModel = mod_cover)
plot(simulationOutput) 

#For elevation
mod_elevation <- gam( MF_av ~ type + coast + cover + s(elevation) + eastness +
    northness + relative_elevation + slope + TWI +
    s(x, y, bs = "gp", m = 2) - 1,
family = betar(link = "logit"),
data = data_sampled
)
summary(mod_elevation)
simulationOutput <- simulateResiduals(fittedModel = mod_elevation)
plot(simulationOutput)

#eastness
mod_east <- gam(MF_av ~ type + coast + cover + elevation + s(eastness) +
    northness + relative_elevation + slope + TWI +
    s(x, y, bs = "gp", m = 2) - 1,
family = betar(link = "logit"),
data = data_sampled
)
summary(mod_east)
simulationOutput <- simulateResiduals(fittedModel = mod_east)
plot(simulationOutput) 

#for northness
mod_north <- gam( MF_av ~ type + coast + cover + elevation + eastness +
    s(northness) + relative_elevation + slope + TWI +
    s(x, y, bs = "gp", m = 2) - 1,
family = betar(link = "logit"),
data = data_sampled
)
summary(mod_north)
simulationOutput <- simulateResiduals(fittedModel = mod_north)
plot(simulationOutput)

#reletive elevation
mod_re_elev <- gam( MF_av ~ type + coast + cover + elevation + eastness +
    northness + s(relative_elevation) + slope + TWI +
    s(x, y, bs = "gp", m = 2) - 1,
family = betar(link = "logit"),
data = data_sampled
)
summary(mod_re_elev)
simulationOutput <- simulateResiduals(fittedModel = mod_re_elev)
plot(simulationOutput)

#slope
mod_slope <- gam(MF_av ~ type + coast + cover + elevation + eastness +
    northness + relative_elevation + s(slope) + TWI +
    s(x, y, bs = "gp", m = 2) - 1,
family = betar(link = "logit"),
data = data_sampled
)
summary(mod_slope)
simulationOutput <- simulateResiduals(fittedModel = mod_slope)
plot(simulationOutput)
