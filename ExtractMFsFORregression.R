library(terra)
mf.av <- rast('E:/Output/Multifunctionality/MF_average_5MIs_25m_V2.tif')
mf.singleT <- rast('E:/Output/Multifunctionality/MF_singleT0.8_EU_25m_V2.tif')
load("./Data/10000sample_covariates_MF_V2.RData")
xy <- cbind(data_sample.mf02$x, data_sample.mf02$y)
data_sampled$MF_av <- extract(mf.av,xy)
data_sampled$MF_singleT_0.8 <- extract(mf.singleT,xy)


colnames(data_sampled)[12] <- 'MF_av'
head(data_sampled)
colnames(data_sampled)[13] <- 'MF_singleT'
head(data_sampled)
data_sample.mf02 <- data_sampled[c(1:11,14,12,13)] #Reorder the column.
save(data_sample.mf02, file = 'I:/GitHub/MF/Data/10000sample_covariates_MF_V2.RData')
