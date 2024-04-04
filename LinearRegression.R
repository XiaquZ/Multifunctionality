##Load the R data and inspect the data.
load('I:/DATA/output/MF/10000samples.RData')
unique(clean_s$type)
mf_av <- unique(clean_s$mean)
mf_singT <- unique(clean_s$MF_singleT_0.8)
class(mf_av)
class(mf_singT)
hist(clean_s$latitude)
dim(clean_s)
colnames(clean_s)[10] <- 'MF_av'
head(clean_s)
#Save R data
save(clean_s, file = 'I:/DATA/output/MF/clean_data_sample.RData')

#Extract 10,000 rows for linear regression.
data.sampled<-clean_s[sample(1:nrow(clean_s),10000, replace=FALSE),]
save(data.sampled, file = 'I:/DATA/output/MF/10000samples.RData')
##Fit a linear regression model
MFav_predictors <- lm(MF_av ~type + latitude + coast + cover + elevation +
                    eastness + northness + relative_elevation + slope, 
                    data = clean_s)

MFav_lm_residuals <- MFav_predictors$residuals
hist(MFav_lm_residuals)
qqnorm(MFav_lm_residuals)# Plot the residuals
# Plot the Q-Q line
qqline(MFav_lm_residuals)

##Fit linear regression on smaller dataset.
df_sampled_lm <- lm(MF_av ~type + latitude + coast + cover + elevation +
                      eastness + northness + relative_elevation + slope, 
                    data = data.sampled)


sample_residual <- df_sampled_lm$residuals
hist(sample_residual)
qqnorm(sample_residual)# Plot the residuals
# Plot the Q-Q line
qqline(sample_residual)

##Poisson model##
load('I:/DATA/output/MF/10000samples.RData')
library(ggplot2)
str(data.sampled)
hist(data.sampled$MF_singleT_0.8)
hist(data.sampled$MF_av)
data.sampled$MF_av <- round(data.sampled$MF_av, digits = 2)

mf_av <- unique(data.sampled$MF_av)
#Predict the MF_T values by the difference of forest type.
with(data.sampled, tapply(MF_av, type, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

#ggplot to compare among the two different forest types
ggplot(data.sampled, aes(MF_av, fill = type)) +
  geom_histogram(binwidth=.2, position="dodge")

hist(data.sampled$slope)

boxplot(cover~MF_av, data = data.sampled,
        varwidth = TRUE)

poisson01 <-glm(formula = MF_av ~ cover + coast + latitude +
                 elevation + relative_elevation + slope, family = "poisson",
                  data = data.sampled)
summary(poisson01)
poisson01_resi <- poisson01$residuals

####Use DHARMa package for residual diagnostics.
library(DHARMa)
library(lme4)
load('I:/DATA/output/MF/10000samples.RData')
fittedModel <- glmer(MF_av ~ cover + coast + latitude +
                 elevation + relative_elevation + slope + (1|type), 
                 family = "poisson", data = data.sampled)

#Standardized X variables.
data.sampled$coast <- scale(data.sampled$coast)
data.sampled$cover <- scale(data.sampled$cover)
data.sampled$latitude <-scale(data.sampled$latitude)
data.sampled$elevation <- scale(data.sampled$elevation)
data.sampled$eastness <- scale(data.sampled$eastness)
data.sampled$northness <- scale(data.sampled$northness)
data.sampled$relative_elevation <- scale(data.sampled$relative_elevation)
data.sampled$slope <- scale(data.sampled$slope)
head(data.sampled)
hist(data.sampled$coast)

##Test the dispersion
plot(coast ~ MF_av, 
     xlab = "MF_av", ylab = "Standardized latitude", data = data.sampled)
poisson01 <-glm(formula = MF_av ~ cover + coast + latitude +
                 elevation + relative_elevation + slope, family = "poisson",
                  data = data.sampled)
summary(poisson01)
simulationOutput <- simulateResiduals(fittedModel = poisson01)
plot(simulationOutput)
testDispersion(simulationOutput)
