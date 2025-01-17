library(terra)
library(tidyverse)
library(mgcv)

# Correlation test between relative elevation and different microrefugia indices.
# Load the data including 9 predictors.
load("I:/GitHub/MF/Data/10000samples_orig_MF_9predicts.RData")
rela_eleva <- predict_s[, c(3, 12, 13)]
regional <- predict_s[, c(10:13)]

# Load MIs.
fvomc <- rast("E:/Output/MicrorefugiaIndex/VoCC/MI_fvocc_75km_25m_add0.tif")
bvomc <- rast("E:/Output/MicrorefugiaIndex/VoCC/MI_bvocc_75km_25m_add0.tif")
maxt <- rast("E:/Output/MicrorefugiaIndex/Offset/MI_MaxTOffset_V2_crop.tif")
mint <- rast("E:/Output/MicrorefugiaIndex/Offset/MI_MinTOffset_V2_crop.tif")
warming <- rast("E:/Output/MicrorefugiaIndex/MI_WarmingMagnitude.tif")

# Extract the MI based on xy coordinates.
xy <- cbind(rela_eleva$x, rela_eleva$y)

# Extract the MI.
rela_eleva[, 4] <- extract(fvomc, xy)
colnames(rela_eleva)[4] <- "fvomc"
rela_eleva[, 5] <- extract(bvomc, xy)
colnames(rela_eleva)[5] <- "bvomc"
rela_eleva[, 6] <- terra::extract(maxt, xy)
colnames(rela_eleva)[6] <- "maxTOffset"
head(rela_eleva)
rela_eleva[, 7] <- terra::extract(mint, xy)
colnames(rela_eleva)[7] <- "minTOffset"
rela_eleva[, 8] <- terra::extract(warming, xy)
colnames(rela_eleva)[8] <- "warming"

# Correlation test
cortest <- rela_eleva |>
    correlate() |>
    focus(relative_elevation)

cor(rela_eleva)[1, -1]

p <- sapply(rela_eleva[, -1],
    FUN = function(x, y) cor.test(x, y)$p.value,
    y = rela_eleva$relative_elevation
)
max(rela_eleva$relative_elevation)
# [1] 697.6343

####
# Testing the interaction between average MF and other variables.
# Use Beta regression model for average MF and Poisson for single T.

mf_model <- gam(
    MF_av ~ northness * cover * slope +
    cover * type +
    coast +
    elevation +
    relative_elevation +
    TWI +
    s(x, y, bs = "gp", m = 2) -1,
    family = betar(link = "logit"),
     data = predict_s)
summary(mf_model)

mf_model_y <- gam(
    MF_av ~ northness * cover * slope +
    cover * type +
    coast +
    elevation +
    relative_elevation +
    TWI + x + y
     -1,
    family = betar(link = "logit"),
     data = predict_s)
summary(mf_model_y)

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