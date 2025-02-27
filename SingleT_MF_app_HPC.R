####Load packages####
library(terra)
##Load all the rasters after cropping to the same extent.
filenames <- list.files("/lustre1/scratch/348/vsc34871/input/MI/", pattern="tif$", full.names=TRUE)

terraOptions(verbose=T)
crop_files <- lapply(filenames, rast) #load all files into raster.

terraOptions(verbose=T)
c_stack <- rast(c(crop_files)) #stack rasters to a stack.

print(c_stack)

####Use single threshold mutifunctionality to count the MIs that beyond the threshold values in each cell.####
# use parallel with app() from terra package to sum the number of MIs greater than threshold.
# Define the function to count values greater than the threshold
fun1 <- function(i) {
  sum(i > 0.6, na.rm = TRUE)  # Ensure NA values are handled
}

terraOptions(verbose=T)
MF_singleT <- app(c_stack, fun1, cores = 10)
MF_singleT <- round(MF_singleT, digits = 1)

#save data.
names(MF_singleT) <- 'MF_singleT_5MIs'
print(MF_singleT)
writeRaster(MF_singleT, filename = "/lustre1/scratch/348/vsc34871/output/MF_threshold0.6_5MIs_EU_25m_EPSG3035.tif", overwrite = TRUE)

# Check the output
mf06 <- rast("D:/PhD/MF_threshold0.6_5MIs_EU_25m_EPSG3035.tif")
mf06_v2 <- rast("D:/PhD/MF_thresholdDotSix_5MIs_EU_25m_EPSG3035_V2.tif")
plot(mf06_v2)
