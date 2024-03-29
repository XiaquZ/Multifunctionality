##Load the R data and inspect the data.
load('I:/DATA/output/MF/clean_data_sample.RData')
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

##Fit a linear regression model
MFav_predictors <- lm(MF_av ~type + latitude + coast + cover + elevation +
                    eastness + northness + relative_elevation + slope, 
                    data = clean_s)

MFav_lm_residuals <- MFav_predictors$residuals
hist(MFav_lm_residuals)
qqnorm(MFav_lm_residuals)# Plot the residuals
# Plot the Q-Q line
qqline(MFav_lm_residuals)
