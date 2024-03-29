library(terra)

bvocc <- rast("I:/DATA/output/MF/MI_BVoMC_75km_25m.tif")
fvocc <- rast('I:/DATA/output/MF/MI_FVoCC_75km_25m.tif')

m_forest <- rast('I:/DATA/output/MF/MI_WarmingMagnitude.tif')
s <- ifel(!is.na(m_forest),0,NA)
plot(s)
bv_cover <- merge(bvocc, s, first = TRUE)
fv_cover <- merge(fvocc, s, first = TRUE)

writeRaster(s, filename = "E:/Output/MicrorefugiaIndex/MI_0_75km_25m.tif", overwrite = T)
