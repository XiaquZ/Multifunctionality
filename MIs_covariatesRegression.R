library(terra)
mi_fvocc <- rast('E:/Output/MicrorefugiaIndex/MI_fvocc_75km_25m_add0.tif')
mi_bvocc <- rast('E:/Output/MicrorefugiaIndex/MI_bvocc_75km_25m_add0.tif')
xy <- cbind(data.sampled$x, data.sampled$y)
mi_sample <- extract(mi_fvocc, xy)
mi_sample <- extract(mi_bvocc, xy)

colnames(mi_sample)[1] <- 'MI_bvocc'
head(mi_sample)
data.sampled$mi_bvocc <- mi_sample$MI_bvocc
head(data.sampled)
str(data.sampled)

pois <-glm(mi_bvocc ~ latitude + coast + cover +elevation + eastness +
                        northness +relative_elevation + slope + type, 
                         family = "poisson", data = data.sampled)

summary(pois)
poisres <- pois$residuals
hist(poisres)
qqnorm(poisres)# Plot the residuals
# Plot the Q-Q line
qqline(poisres)
##Use ggplot2 to check standardized residual 
library(ggplot2)

ggplot() +
  geom_qq(aes(sample = rstandard(pois))) +
  geom_abline(color = "red") +
  coord_fixed()

ggplot(data = data.sampled, aes(x = rstandard(pois))) +
  geom_histogram(aes(y= ..density..), binwidth = 0.5, color = "black", fill = "#00AFBB") +
  geom_density(alpha =0.2, fill = '#9aad2c') +
  theme(panel.background = element_rect(fill = 'white'),
  axis.line.x = element_line(),
  axis.line.y = element_line()) + 
  ggtitle('Hist for standardized res')

##Use DHARMa to test assumptions####
simulationOutput <- simulateResiduals(fittedModel = pois)
plot(simulationOutput) #Looks like underdispersion. check zero-inflation

#Dispersion
testDispersion(simulationOutput)

#Exact p-value for the quantile lines
testQuantiles(simulationOutput)

#test for zero-inflation.
testZeroInflation(simulationOutput) #There is no zero-inflation

#test 1-inflation?
countOnes <- function(x) sum(x == 1)  # testing for number of 1
testGeneric(simulationOutput, summary = countOnes, alternative = "greater") #not significant in 1-inflation.

#test for categorical predictors (forest type)
testCategorical(simulationOutput, catPred = data.sampled$type)
##one-sample Kolmogorov-Smirnov test determine whether the data is ditributed 
#significantly from a uniformity distribution per box. 
#here the 'type' does not follow uniformity.
#And a Levene's test for homogeneity of variances between boxes. A positive result will be in red.

#plot residual against other predictors.
plotResiduals(simulationOutput, data.sampled$cover)
plotResiduals(simulationOutput, data.sampled$latitude)
plotResiduals(simulationOutput, data.sampled$coast)

####quasipoisson####
# requires glmmTMB to include 'ziformula' for testing zero-inflation
quasipois <- glm(MF_av ~ latitude + coast + cover +elevation + eastness +
                        northness +relative_elevation + slope, 
                        family = "quasipoisson", data = data.sampled)
summary(quasipois) 
#DHARMa can't work with quasipoisson.So use qq-plot.
quasipoisres <- quasipois$residuals
hist(quasipois$residuals)
qqnorm(quasipoisres)# Plot the residuals
# Plot the Q-Q line
qqline(quasipoisres)
