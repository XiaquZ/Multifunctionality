####Load packages####
library(terra)

MI1 <- rast('/lustre1/scratch/348/vsc34871/input/MI_MaxTOffset.tif')
MI2 <- rast('/lustre1/scratch/348/vsc34871/input/MI_MinTOffset.tif')
MI3 <- rast('/lustre1/scratch/348/vsc34871/input/MI_ForestBVoCC_10kmSR_25m.tif')
MI4 <- rast('/lustre1/scratch/348/vsc34871/input/MI_ForestFVoCC_10kmSR_25m.tif')
MI5 <- rast("/lustre1/scratch/348/vsc34871/input/MI_WarmingMagnitude.tif")
##Make raster stacks.##
output_stack <- c(MI1, MI2, MI3, MI4,MI5)
print(output_stack)

####Use single threshold mutifunctionality to count the MIs that beyond the threshold values in each cell.####
# use parallel with app() from terra package to sum the number of MIs greater than threshold.
fun1 <- function(i) { sum(i > 0.5) }
MF_singleT <- app(output_stack, \(i) fun1(i))
names(MF_singleT) <- 'MF_singleT_5MIs'
writeRaster(MF_singleT, filename = "/lustre1/scratch/348/vsc34871/output/MF/MF_threshold0.5_5MIs_EU_25m_EPSG3035.tif", overwrite = TRUE)
