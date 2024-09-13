library(terra)
library(dplyr)

# Load data
load("./Data/10000sample_covariates_MF_V2.RData")
coast <- rast("H:/Input/Predictors/coast.tif")
elevation <- rast("H:/Input/Predictors/elevation.tif")
rela_eleva <- rast("H:/Input/Predictors/relative_elevation.tif")
slope <- rast("H:/Input/Predictors/slope.tif")
twi <- rast("H:/Input/Predictors/TWI.tif")
eastness <- rast("H:/Input/Predictors/eastness.tif")
north <- rast("H:/Input/Predictors/northness.tif")
type <- rast("H:/Input/Predictors/type.tif")
cover <- rast("H:/Input/Predictors/cover.tif")

# Get coordination
xy <- cbind(data_sample.mf02$x, data_sample.mf02$y)

# Extract values of predictors based on xy.
predict_s <- extract(coast, xy)
predict_s[,2] <- extract(elevation, xy)
predict_s[, 3] <- extract(rela_eleva, xy)
predict_s[, 4] <- extract(slope, xy)
predict_s[, 5] <- extract(twi, xy)
predict_s[, 6] <- extract(eastness, xy)
predict_s[, 7] <- extract(north, xy)
predict_s[, 8] <- extract(type, xy)
predict_s[, 9] <- extract(cover, xy)

# Load MF indices and extract the MF
MF_av <- rast("H:/Output/Multifunctionality/MF_average_5MIs_25m_V2.tif")
MF_singleT <- rast("H:/Output/Multifunctionality/MF_singleT0.8_EU_25m_V2.tif")

# Extract MF
predict_s[, 10] <- extract(MF_av, xy)
predict_s[, 11] <- extract(MF_singleT, xy)
predict_s <- predict_s %>% rename(x = V12, y = V13) 
predict_s <- predict_s %>% rename(MF_av = mean, MF_0.8T = lyr.1)

save(predict_s, file = "./Data/10000samples_orig_MF_9predicts.RData")
