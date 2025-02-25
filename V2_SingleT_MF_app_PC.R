library(terra)
## Load all the rasters after cropping to the same extent.
mi_warming <- rast("E:/Output/MicrorefugiaIndex/MI_WarmingMagnitude.tif")
mi_max <- rast("E:/Output/MicrorefugiaIndex/Offset/MI_MaxTOffset_V2_crop.tif")
mi_min <- rast("E:/Output/MicrorefugiaIndex/Offset/MI_MinTOffset_V2_crop.tif")
mi_fvomc <- rast("E:/Output/MicrorefugiaIndex/VoCC/MI_fvocc_75km_25m_add0.tif")
mi_bvomc <- rast("E:/Output/MicrorefugiaIndex/VoCC/MI_bvocc_75km_25m_add0.tif")

terraOptions(verbose = T) ## monitor the memory
crop_files <- lapply(filenames, rast)
c_stack <- c(mi_bvomc, mi_fvomc, mi_max, mi_min, mi_warming)

#### Use single threshold mutifunctionality to count the MIs that beyond
#### the threshold values in each cell.####
# use parallel with app() from terra package to sum the number of MIs
# greater than threshold.
parallel_fun <- function(i) {
    sum(i > 0.6)
}

terraOptions(verbose = T)
MF_singleT <- app(c_stack,
    fun = function(i, ff) ff(i),
    cores = 10,
    ff = parallel_fun
)
# with core>1, things can be speed up.
plot(MF_singleT)

terraOptions(verbose = T)
writeRaster(MF_singleT,
    filename = "E:/Output/Multifunctionality/MF_singleThreshold0.6_5MIs_EU_25m_EPSG3035.tif",
    overwrite = TRUE
)
names(MF_singleT) <- "MF_singleT_5MIs"
