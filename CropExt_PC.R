library(terra)
MI1 <- rast('I:/DATA/output/MI_MaxTOffset_V2.tif')
MI2 <- rast('I:/DATA/output/MI_MinTOffset_V2.tif')
MI3 <- rast('E:/Output/MicrorefugiaIndex/MI_WarmingMagnitude.tif')

MI1ext <- ext(MI1)
cropext <- ext(MI3)
MI1crop <- crop(MI1, cropext)
writeRaster(MI1crop, filename = 'E:/Output/MicrorefugiaIndex/MI_MaxTOffset_V2_cropext.tif', overwrite=T)
print(MI1crop)

MI2ext <- ext(MI2)
MI2crop <- crop(MI2, cropext)
writeRaster(MI2crop, filename = 'E:/Output/MicrorefugiaIndex/MI_MinTOffset_V2_cropext.tif', overwrite=T)
print(MI2crop)

mem_info(MI3)
terraOptions(verbose=T)
