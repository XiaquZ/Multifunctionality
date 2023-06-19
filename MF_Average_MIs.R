####Load packages####
library(terra)
library(parallel)

MIs <- c('D:/PhD/Data/MIs_ClimateOffset_MAT_new.tif',
         'D:/PhD/Data/MIs_ClimateOffset_Max_new.tif',
         'D:/PhD/Data/MIs_ClimateOffset_Min_new.tif',
         'D:/PhD/Data/MIs_ClimateWarming_SSP370.tif')

output_vector <- mclapply(MIs, rast)
output_stack <- rast(c(output_vector))
####MF_average Methods2: Without using foreach 
mean_test <- app(output_stack,mean, na.rm = TRUE, core =20)
mean_test <- round(mean_test, digits = 1)
class(mean_test) <- "SpatRaster"
names(mean_test) <- 'MF_average_4MIs'
mean_test <- setMinMax(mean_test)

writeRaster(mean_test, filename = "D:/PhD/Data/Output/MF_Average_4MIs_EU_25m_EPSG3035_V2.tif", overwrite = TRUE)

##round raster digits
MF_av <- rast('D:/PhD/Data/Output/MF_Average_4MIs_EU_25m_EPSG3035_V2.tif') 
MF_av <- round(MF_av, digits = 1)
writeRaster(MF_av, filename = "D:/PhD/Data/Output/MF_Average_4MIs_EU_25m_EPSG3035_V2.tif", overwrite = TRUE)
# Compare all cell values for near equality
# as floating point number imprecision can be a problem
m <- minmax(r1 - r2)
all(abs(m) < 1e-7)

####Use single threshold mutifunctionality to count the MIs that beyond the threshold values in each cell.####
# use parallel with app() from terra package to sum the number of MIs greater than threshold.
library(terra)
parallel_fun <- function(i) { sum(i > 0.8) }
MF_singleT <- app(output_stack, fun=function(i, ff) ff(i), cores =22, ff=parallel_fun)#with core>1, things can be speed up.
plot(MF_singleT)
writeRaster(MF_singleT, filename = "D:/PhD/Data/Output/MF_singleThreshold_4MIs_EU_25m_EPSG3035.tif", overwrite = TRUE)
names(MF_singleT) <- 'MF_singleT_4MIs'


##Single threshold 0.6
parallel_fun2 <- function(i) { sum(i > 0.6) }
MF_singleT0.6 <- app(output_stack, fun=function(i, ff) ff(i), cores =22, ff=parallel_fun2)#with core>1, things can be speed up.

names(MF_singleT0.6) <- 'MF_singleT0.6_4MIs'
writeRaster(MF_singleT0.6, filename = "D:/PhD/Data/Output/MF_singleThreshold0.6_4MIs_EU_25m_EPSG3035.tif", overwrite = TRUE)

##Single threshold 0.4
parallel_fun3 <- function(i) { sum(i > 0.4) }
MF_singleT0.4 <- app(output_stack, fun=function(i, ff) ff(i), cores =22, ff=parallel_fun3)#with core>1, things can be speed up.
names(MF_singleT0.4) <- 'MF_singleT0.4_4MIs'
writeRaster(MF_singleT0.4, filename = "D:/PhD/Data/Output/MF_singleThreshold0.4_4MIs_EU_25m_EPSG3035.tif", overwrite = TRUE)

##Single threshold 0.9
parallel_fun0.9 <- function(i) { sum(i > 0.9) }
MF_singleT0.9 <- app(output_stack, fun=function(i, ff) ff(i), cores =22, ff=parallel_fun3)#with core>1, things can be speed up.
names(MF_singleT0.9) <- 'MF_singleT0.9_4MIs'
writeRaster(MF_singleT0.9, filename = "D:/PhD/Data/Output/MF_singleThreshold0.9_4MIs_EU_25m_EPSG3035.tif", overwrite = TRUE)

####plotting MF.####
s1 <- spatSample(MF_av, 1000, method="random", replace=FALSE, na.rm=TRUE, 
                 as.raster=FALSE, as.df=TRUE, as.points=FALSE, values=TRUE)
s2 <- spatSample(MF_singleT, 1000, method="random", replace=FALSE, na.rm=TRUE, 
                 as.raster=FALSE, as.df=TRUE, as.points=FALSE, values=TRUE)
stackMF <- c(MF_av, MF_singleT)
s3 <- spatSample(stackMF, 10000, method="random", replace=FALSE, na.rm=TRUE, 
                 as.raster=FALSE, as.df=TRUE, as.points=FALSE, values=TRUE)
plot(s3)
library(graphics)
y_label <- "MF_singleT_4MIs"
x_label <- "MF_average_4MIs"
smoothScatter(s3$MF_average_4MIs, s3$MF_singleT_4MIs, pch = 19,
              transformation = function(x) x ^ 0.3,
              nrpoints=nrow(s3),
              xlim = c(0 , 1))


# Add x-axis label
text(x = 0.5, y = par()$usr[3] - 0.1 * (par()$usr[4] - par()$usr[3]),
     labels = x_label, pos = 1)
# Add y-axis label
text(x = par()$usr[1] - 0.05 * (par()$usr[2] - par()$usr[1]), y = par()$usr[4],
     labels = y_label, pos = 2, srt = 90)

# Add legend
legend("topright", legend = "Density", fill = "white", border = "black")

corr <- cor(s3$MF_average_4MIs,s3$MF_singleT_4MIs, method = "spearman")

####Test to add the labels and legend...
par(mfrow=c(1,2))
set.seed(3)
x1 = rnorm(1000)
y1 = rnorm(1000)
smoothScatter(x1,y1,nrpoints=length(x1),cex=3)

x2 = rnorm(200)
y2 = rnorm(200)
smoothScatter(x2,y2,nrpoints=length(x2),cex=3,colramp=colorRampPalette(c("white","red")))

