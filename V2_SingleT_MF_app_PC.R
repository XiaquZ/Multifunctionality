library(terra)
## Load all the rasters after cropping to the same extent.
mi_warming <- rast("H:/Output/MicrorefugiaIndex/MI_WarmingMagnitude.tif")
mi_max <- rast("H:/Output/MicrorefugiaIndex/Offset/MI_MaxTOffset_V2_crop.tif")
mi_min <- rast("H:/Output/MicrorefugiaIndex/Offset/MI_MinTOffset_V2_crop.tif")
mi_fvomc <- rast("H:/Output/MicrorefugiaIndex/VoCC/MI_fvocc_75km_25m_add0.tif")
mi_bvomc <- rast("H:/Output/MicrorefugiaIndex/VoCC/MI_bvocc_75km_25m_add0.tif")

terraOptions(verbose = T) ## monitor the memory

c_stack <- c(mi_bvomc, mi_fvomc, mi_max, mi_min, mi_warming)

#### Use single threshold mutifunctionality to count the MIs that beyond
#### the threshold values in each cell.####
# use parallel with app() from terra package to sum the number of MIs
# greater than threshold.
# Define a function to count the number of MIs greater than a threshold
parallel_fun <- function(i, threshold = 0.6) {
  sum(i > threshold)
}

# Apply the function in parallel using the app() function from terra
MF_singleT <- app(
  c_stack,
  fun = function(i, ff, ...) ff(i, ...),
  cores = 10,
  ff = parallel_fun,
  threshold = 0.6
)
# with core>1, things can be speed up.
plot(MF_singleT)

terraOptions(verbose = T)
writeRaster(MF_singleT,
    filename = "H:/Output/Multifunctionality/MF_singleThreshold0.6_5MIs_EU_25m_EPSG3035.tif",
    overwrite = TRUE
)
names(MF_singleT) <- "MF_singleT0.6_5MIs"

