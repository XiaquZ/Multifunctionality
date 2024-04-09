library(DHARMa)
library(lme4)

load('I:/DATA/output/MF/standardized10000samples_xy.RData')
####Fit the model####
###########################
##Test the poisson model###
###########################
plot(coast ~ MF_av, 
     xlab = "MF_av", ylab = "Standardized latitude", data = data.sampled)
pois <-glm(MF_av ~ latitude + coast + cover +elevation + eastness +
                        northness +relative_elevation + slope, 
                         family = "poisson", data = data.sampled)

summary(pois)
poisres <- pois$residuals
hist(poisres)
qqnorm(poisres)# Plot the residuals
# Plot the Q-Q line
qqline(poisres)

##Use DHARMa to test assumptions####
simulationOutput <- simulateResiduals(fittedModel = pois)
plot(simulationOutput) #Looks like underdispersion. check zero-inflation

#test for zero-inflation.
testZeroInflation(simulationOutput) #There is no zero-inflation

#test 1-inflation?
countOnes <- function(x) sum(x == 1)  # testing for number of 1
testGeneric(simulationOutput, summary = countOnes, alternative = "greater") #not significant in 1-inflation.

#Dispersion
testDispersion(simulationOutput)

#Exact p-value for the quantile lines
testQuantiles(simulationOutput)

#test for categorical predictors
testCategorical(simulationOutput, catPred = data.sampled$type)
##one-sample Kolmogorov-Smirnov test determine whether the data is ditributed 
#significantly from a uniformity distribution per box. 
#here the 'type' does not follow uniformity.
#And a Levene's test for homogeneity of variances between boxes. A positive result will be in red.

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

####Compare different MF among conifer and broadleaf.####
#########################################################
##Base on the KS test and the Levene's test, the Mann-Whitney U test is applied.
conif <- data.sampled[data.sampled$type == '1',]
broadl <- data.sampled[data.sampled$type == '2',]
shapiro.test(conif$MF_av) #test the nomality by using Shapiro-Wilk test.Sample size must below 5000.

library(dplyr)
library(ggpubr)
group_by(data.sampled, type) %>%
  summarise(
    count = n(),
    median = median(MF_av, na.rm = TRUE),
    IQR = IQR(MF_av, na.rm = TRUE))

res <- wilcox.test(MF_av~ type, 
                   data = data.sampled,
                   exact = FALSE)#p < 0.05, there is a significant different between two types.

ggboxplot(data.sampled, x = "type", y = "MF_av", 
          color = "type", palette = c("#00AFBB", "#E7B800"),
          ylab = "MF_av", xlab = "Forest type")
#Density chart
library(hrbrthemes)
library(tidyr)
library(ggplot2)
cols <- c("#00AFBB", "#E7B800")
p2 <- ggplot(data=data.sampled, aes(x=MF_av, fill = type)) +
    geom_density(alpha=.4, color = NA) +
    scale_fill_manual(values = cols)
