##Load the R data and inspect the data.
load('I:/DATA/output/MF/clean_data_samplexy.RData')
unique(clean_s$type)
mf_singT <- unique(clean_s$MF_singleT_0.8)
class(mf_av)
class(mf_singT)
hist(clean_s$latitude)
dim(clean_s)
colnames(clean_s)[12] <- 'MF_av'
head(clean_s)
#Save R data
save(clean_s, file = 'I:/DATA/output/MF/clean_data_sample.RData')

#Extract 10,000 rows for linear regression.
data.sampled<-clean_s[sample(1:nrow(clean_s),10000, replace=FALSE),]
save(data.sampled, file = 'I:/DATA/output/MF/10000samples_xy.RData')

#sample_residual <- df_sampled_lm$residuals


##Check the data
load('I:/DATA/output/MF/10000samples.RData')
str(data.sampled)
hist(data.sampled$MF_singleT_0.8)
hist(data.sampled$MF_av)
mf_av <- unique(data.sampled$MF_av)
data.sampled$MF_av <- round(data.sampled$MF_av, digits = 2)
mf_av <- unique(data.sampled$MF_av)
#boxplot 
boxplot(cover~MF_av, data = data.sampled,
        varwidth = TRUE)

####Use DHARMa package for residual diagnostics.####
library(DHARMa)
library(lme4)
library(glmmTMB)
load('I:/DATA/output/MF/10000samples.RData')

#Standardized X variables.
data.sampled$coast <- scale(data.sampled$coast)
data.sampled$cover <- scale(data.sampled$cover)
data.sampled$latitude <-scale(data.sampled$latitude)
data.sampled$elevation <- scale(data.sampled$elevation)
data.sampled$eastness <- scale(data.sampled$eastness)
data.sampled$northness <- scale(data.sampled$northness)
data.sampled$relative_elevation <- scale(data.sampled$relative_elevation)
data.sampled$slope <- scale(data.sampled$slope)
data.sampled$type <- as.factor(data.sampled$type)
save(data.sampled, file = 'I:/DATA/output/MF/standardized10000samples_xy.RData')
head(data.sampled)
hist(data.sampled$coast)

##glmer function(DON'T RUN, very slow)
#fittedModel <- glmer(MF_av ~ coast +  (1|type) , 
#                    family = "poisson", data = data.sampled)
#simulationOutput <- simulateResiduals(fittedModel = fittedModel)
#plot(simulationOutput)

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

#####By default, plotResiduals plots against predicted values. 
####However, you can also use it to plot residuals against a specific other predictors (highly recommend).
#If the predictor is a factor, or if there is just a small number of observations on the x axis,
#plotResiduals will plot a box plot with additional tests instead of a scatter plot.
plotResiduals(simulationOutput, data.sampled$type) 
plotResiduals(simulationOutput, data.sampled$latitude)

###################################
####Try the quansibinomial model.##
###################################
quasibino <-glm(MF_av ~ latitude + coast + cover +elevation + eastness +
                        northness +relative_elevation + slope, 
                         family = "quasibinomial", data = data.sampled)
summary(quasibino)
#simulationOutput <- simulateResiduals(fittedModel = binomial01) #!DHARMa can't run quansi model.


#########################
####Negative binomial####
#########################
library(MASS)
head(data.sampled)
negbino <- glm.nb(MF_av ~ latitude + coast + cover +elevation + eastness +
                        northness +relative_elevation + slope, 
                          data = data.sampled)
summary(negbino)
simulationOutput <- simulateResiduals(fittedModel = negbino)
plot(simulationOutput)
#test zero-inflation.
library(pscl)
zinb <- zeroinfl(MF_singleT_0.8 ~ latitude + coast + cover +elevation + eastness +
                        northness +relative_elevation + slope,
               dist = "negbin", data = data.sampled) #zero inflated nb, in the pscl package
#Still look overfitting and underdispersion.

####Binomial####
binomial02 <-glm(MF_av ~ latitude + coast + cover +elevation + eastness +
                        northness +relative_elevation + slope, 
                         family = "binomial", data = data.sampled)
summary(binomial02)
simulationOutput <- simulateResiduals(fittedModel = binomial02)
plot(simulationOutput)
#A bit better.but still look underdispersion and overfitting.
testDispersion(simulationOutput)
testZeroInflation(simulationOutput) #there is no zero-inflation

#Test the non-independence of the data (e.g. temporal autocorrelation, 
#check via DHARMa:: testTemporalAutocorrelation) that your predictors can use to overfit, 
#or that your data-generating process is simply not a Poisson process.
testSpatialAutocorrelation(simulationOutput)
##zero inflated nb
library(pscl)
install.packages("pscl")
zinb <- zeroinfl(MF_singleT_0.8 ~ latitude + coast + cover +elevation + eastness +
                        northness +relative_elevation + slope, ,
                        dist = "negbin", data = data.sampled) #zero inflated nb, in the pscl package. MF is not ineger.

####Compare different models.####
tmp <- summary(zinb)
tmp$coefficients$count[-7, 1] |> exp()
tmp$coefficients$zero[-7, 1] |> exp()
tmp <- data.frame(bino = AIC(binomial02), pois = AIC(poisson01), quasip = AIC(quasipois),
                  quasibin = AIC(quasibino),negb = AIC(negbino))
knitr::kable(tmp, align = "l")
