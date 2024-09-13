library(terra)

template <- rast("E:/Output/MicrorefugiaIndex/VoCC/MI_BVoMC_75km_25m.tif")
r1 <- rast("E:/Input/Predictors/type.tif")
r2 <- rast("E:/Input/Predictors/latitude.tif")
r3 <- rast('E:/Input/Predictors/coast.tif')
r4 <- rast('E:/Input/Predictors/cover.tif')
r5 <- rast('E:/Input/Predictors/elevation.tif')
r6 <- rast('E:/Input/Predictors/eastness.tif')
r7 <- rast('E:/Input/Predictors/northness.tif')
r8 <- rast('E:/Input/Predictors/relative_elevation.tif')
r9 <- rast('E:/Input/Predictors/slope.tif')
r10 <- rast('E:/Input/Predictors/TWI.tif')
MF_av <- rast('E:/Output/Multifunctionality/MF_Average_5MIs_EU_25m_EPSG3035.tif')
MF_singT <- rast('E:/Output/Multifunctionality/MF_singleT0.8_EU_25m_EPSG3035.tif')

r1_c <- terra::crop(r1, ext(template))
ext(r1_c)

r2_c <- terra::crop(r2, ext(template))
ext(r2_c)

r3_c <- terra::crop(r3, ext(template))
ext(r3_c)

r4_c <- terra::crop(r4, ext(template))
ext(r4_c)

r5_c <- terra::crop(r5, ext(template))
ext(r5_c)

r6_c <- terra::crop(r6, ext(template))
ext(r6_c)

r7_c <- terra::crop(r7, ext(template))
ext(r7_c)

r8_c <- terra::crop(r8, ext(template))
ext(r8_c)

r9_c <- terra::crop(r9, ext(template))
ext(r9_c)

r10_c <- terra::crop(r10, ext(template))
ext(r10_c)

s <- c(r1_c, r2_c, r3_c, r4_c, r5_c, r6_c, r7_c, r8_c, r9_c, MF_av, MF_singT)

writeRaster(s, filename = "E:/Input/FutureMicroData/MAT/extentMATCH/MicroMAT_2071-2100_EU_SSP370_EPSG3035_25m_extMI.tif", overwrite = TRUE)

##Calculate the quantile values.
sample_s <- spatSample(s, 10^6, "regular", na.rm = T, xy = TRUE)
save(sample_s, file = 'I:/DATA/output/MF/data_sample_xy.RData')
clean_s <- sample_s[complete.cases(sample_s),]
save(clean_s, file = 'I:/DATA/output/MF/clean_data_samplexy.RData')
plot(s$macroMAT2085, s$spat_B1SEdt1KJERukUx_2364466)
hist(s$ForestClim_01, s$MF_singleT_0.8)
xy <- cbind(sample_s$x,sample_s$y)
twi <- extract(r10, xy)

#Save data
