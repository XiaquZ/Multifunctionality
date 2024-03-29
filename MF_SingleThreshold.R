library(terra)
##Load all the rasters after cropping to the same extent.
filenames <- list.files("I:/DATA/output/MF", pattern="tif$", full.names=TRUE)
crop_files <- lapply(filenames, rast)
c_stack <- rast(c(crop_files))

####Use single threshold mutifunctionality to count the MIs that beyond the threshold values in each cell.####
# use parallel with app() from terra package to sum the number of MIs greater than threshold.
parallel_fun <- function(i) { sum(i > 0.8) }
MF_singleT <- app(c_stack, fun=function(i, ff) ff(i), cores =10, ff=parallel_fun)#with core>1, things can be speed up.
plot(MF_singleT)
writeRaster(MF_singleT, filename = "D:/PhD/Data/Output/MF_singleThreshold_4MIs_EU_25m_EPSG3035.tif", overwrite = TRUE)
names(MF_singleT) <- 'MF_singleT_4MIs'