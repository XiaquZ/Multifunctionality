####Load packages####
library(terra)
library(parallel)
library(dplyr)

MIs <- c('/lustre1/scratch/348/vsc34871/input/MIs/MIs_ClimateOffset_MAT_new.tif',
         '/lustre1/scratch/348/vsc34871/input/MIs/MIs_ClimateOffset_Max_new.tif',
         '/lustre1/scratch/348/vsc34871/input/MIs/MIs_ClimateOffset_Min_new.tif',
         '/lustre1/scratch/348/vsc34871/input/MIs/MIs_ClimateWarming_SSP370.tif')

output_vector <- mclapply(MIs, rast)
output_stack <- rast(c(output_vector))
mean_test <- mclapply(output_stack,mean)

writeRaster(mean_test, filename = "/lustre1/scratch/348/vsc34871/output/MF_MIs_Average_EU_25m_EPSG3035.tif", overwrite = TRUE)

##transfer the raster into dataset with cell ID and MF values.
####Create initial climate data frame.####
MF_av <- mean_test %>%
  values() %>%
  data.frame(cid = 1:ncell(mean_test)) %>%
  na.omit()

#Get the coordination for each pixel.
MF_av[,c("x","y")] <- xyFromCell(mean_test, MF_av$cid) 

# Save dataframe as .csv
write.csv(MF_av, file = "/lustre1/scratch/348/vsc34871/output/MI_MF_av.csv", row.names = FALSE)