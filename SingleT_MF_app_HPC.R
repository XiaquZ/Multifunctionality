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
fun1 <- function(i) { sum(i > 0.8) }

terraOptions(verbose=T)
MF_singleT <- app(c_stack, \(i) fun1(i))
#OR with more cores, but does the code below really accelarate?
MF_singleT <- app(c_stack, fun=function(i, ff) ff(i), cores =10, ff=parallel_fun)#with core>1, things can be speed up.

#save data.
names(MF_singleT) <- 'MF_singleT_5MIs'
print(MF_singleT)
writeRaster(MF_singleT, filename = "/lustre1/scratch/348/vsc34871/output/MF/MF_threshold0.8_5MIs_EU_25m_EPSG3035.tif", overwrite = TRUE)
