library(terra)
MI1 <- rast('E:/Output/MicrorefugiaIndex/Original_extNOTmatch/MI_BVoMC_75km_25m.tif')
MI2 <- rast('E:/Output/MicrorefugiaIndex/Original_extNOTmatch/MI_MaxTOffset.tif')

cropext <- ext(MI1)

terraOptions(verbose=T)
MI2crop <- crop(MI2, cropext) #crop extent.
terraOptions(verbose=T)
writeRaster(MI2crop, filename = 'E:/Output/MicrorefugiaIndex/Original_extNOTmatch/MI_MaxTOffset_V2_cropext.tif', overwrite=T)
print(MI1crop)

MI2_round <- round(MI2, digits = 1)
writeRaster(MI2_round, filename = 'E:/Output/MicrorefugiaIndex/MI_MinTOffset_V2_round.tif', overwrite=T)

MI2ext <- ext(MI2)
MI2crop <- crop(MI2, cropext)
writeRaster(MI2crop, filename = 'E:/Output/MicrorefugiaIndex/MI_MinTOffset_V2_cropext.tif', overwrite=T)
print(MI2crop)

mem_info(MI2)
terraOptions(verbose=T)

# Check the extent of MIs.
mi_warming <- rast("E:/Output/MicrorefugiaIndex/MI_WarmingMagnitude.tif")
ext(mi_warming) #SpatExtent : 2636075, 5955525, 1386025, 5415875 (xmin, xmax, ymin, ymax)
mi_fvomc <- rast("E:/Output/MicrorefugiaIndex/VoCC/MI_fvocc_75km_25m_add0.tif")
ext(mi_fvomc) #SpatExtent : 2636075, 5955525, 1386025, 5415875 (xmin, xmax, ymin, ymax)
mi_bvomc <- rast("E:/Output/MicrorefugiaIndex/VoCC/MI_bvocc_75km_25m_add0.tif")
ext(mi_bvomc)
mi_maxoffset <- rast("E:/Output/MicrorefugiaIndex/Offset/MI_MaxTOffset_V2_crop.tif")
ext(mi_maxoffset)
mi_minoffset <- rast("E:/Output/MicrorefugiaIndex/Offset/MI_MinTOffset_V2_crop.tif")
ext(mi_minoffset)
