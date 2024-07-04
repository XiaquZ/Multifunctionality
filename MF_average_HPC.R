####Load packages####
library(terra)
library(parallel)
terraOptions(verbose=T)

##Load all the rasters after cropping to the same extent.
filenames <- list.files("/lustre1/scratch/348/vsc34871/input/MI", pattern="tif$", full.names=TRUE)
crop_files <- lapply(filenames, rast)
c_stack <- rast(c(crop_files))
####MF_average Methods
terraOptions(verbose=T)
MF_av <- mean(c_stack, na.rm = TRUE)
MF_av
# MF_av02<- app(c_stack,mean, na.rm = TRUE, core =10)
# MF_av02
terraOptions(verbose=T)
MF_av<- round(MF_av, digits = 1)

names(MF_av) <- 'MF_average_5MIs'
terraOptions(verbose=T)
MF_av <- setMinMax(MF_av)
MF_av
terraOptions(verbose=T)
writeRaster(MF_av, filename = "/scratch/348/vsc34871/output/MF_Average_5MIs_EU_25m_EPSG3035.tif", overwrite = TRUE)
