library(DHARMa)
library(dplyr)
library(mgcv)
library(mgcViz)

load("./Data/10000samples_orig_MF_9predicts.RData")

#### Fit the model####
############################
## Test the poisson model###
############################

# Change the format to numeric.
predict_s$MF_av <- as.numeric(unlist(predict_s$MF_av))
predict_s$MF_0.8T <- as.numeric(unlist(predict_s$MF_0.8T))
predict_s <- as_tibble(predict_s)


# data_sampled$pos <- numFactor(data_sampled$x, data_sampled$y)
# data_sampled$group <- factor(rep(1, nrow(data_sampled)))
class(predict_s$type)
predict_s$type[which(predict_s$type == 1)] <- "Broadleaved forest"
predict_s$type[which(predict_s$type == 2)] <- "Coniferous forest"
predict_s$type <- as.factor(predict_s$type)
head(predict_s)

# Standardized predictors.
predict_s$coast <- scale(predict_s$coast)
predict_s$elevation <- scale(predict_s$elevation)
predict_s$relative_elevation <- scale(predict_s$relative_elevation)
predict_s$slope <- scale(predict_s$slope)
predict_s$TWI <- scale(predict_s$TWI)
predict_s$eastness <- scale(predict_s$eastness)
predict_s$northness <- scale(predict_s$northness)
predict_s$cover <- scale(predict_s$cover)


# Extract samples
data_3000 <- sample_n(predict_s, 3000)
head(data_3000)

# Standardized predictors samples.
data_3000$coast <- scale(data_3000$coast)
data_3000$elevation <- scale(data_3000$elevation)
data_3000$relative_elevation <- scale(data_3000$relative_elevation)
data_3000$slope <- scale(data_3000$slope)
data_3000$TWI <- scale(data_3000$TWI)
data_3000$eastness <- scale(data_3000$eastness)
data_3000$northness <- scale(data_3000$northness)
data_3000$cover <- scale(data_3000$cover)
apply(data_3000, 2, sd)
#### Poisson model####
# # Poisson WITHOUT intercept.
# mod_pois_no_intercept<- gam(
#   MF_0.8T ~ northness * cover * slope +
#     cover * type +
#     coast +
#     elevation +
#     relative_elevation +
#     TWI +
#     s(x, y, bs = "gp", m = 2) - 1,
#   family = poisson(link = "log"),
#   data = predict_s
# )
# summary(mod_pois_no_intercept)
# simulationOutput_pois <- simulateResiduals(fittedModel = mod_pois_no_intercept)
# plot(simulationOutput_pois)
# testDispersion(simulationOutput_pois) # overdispersion
# testZeroInflation(simulationOutput_pois) # Poisson model has zero-inflation.
# 
# par(mfrow = c(1,2))
# plotResiduals(simulationOutput_pois, predict_s$relative_elevation)
# plotResiduals(simulationOutput_pois, predict_s$elevation)
# plotResiduals(simulationOutput_pois, predict_s$type)
# # Overdispersion caused by elevation and forest types?


# Fit a ZINB model with spatial autocorrelation using brms
library(brms)

# ZINB model with 3 interaction terms.
mod_zinb <- brm(
  MF_0.8T ~ 
    northness * slope * cover +
    eastness +
    cover * type +
    coast +
    elevation +
    relative_elevation +
    TWI +
    s(x, y, bs = "tp") -1,  
  family = zero_inflated_negbinomial(),  # Zero-inflated negative binomial
  data = predict_s,
  cores = 2,  # Use multiple cores for faster computation
  chains = 2,
  iter = 5000,  # Number of iterations
  control = list(adapt_delta = 0.9), 
  warmup = 1000
)

# Check model summary
summary(mod_nb)
#Save model output
save(mod_nb,
     file = "I:/DATA/output/MF_origi/MFsingleT_zero_inflated_negbinomial.rda"
)

# Check residuals
simulationOutput_zinb <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod_zinb)),
  observedResponse = predict_s$MF_0.8T,
  fittedPredictedResponse = apply(t(posterior_epred(mod_zinb)), 1, mean),
  integerResponse = TRUE)

plot(simulationOutput_zingb)
testDispersion(simulationOutput_zingb)


#### Beta regression accounting spatial autocorrelation. ####
# Let the response variables all bgreater than 0.
epsilon <- 1e-6
predict_s$MF_av <- (predict_s$MF_av * (1 - 2 * epsilon)) + epsilon
max(predict_s$MF_av)
min(predict_s$MF_av)

data_3000 <- sample_n(predict_s, 3000)
max(data_3000$MF_av)
min(data_3000$MF_av)



#Beta regression with 2 interaction terms.
mod_bayes_beta <- brm(
  MF_av ~ 
    northness * slope +
    eastness +
    cover * type +
    coast +
    elevation +
    relative_elevation +
    TWI +
    s(x, y, bs = "tp") -1,  
  family = Beta(),  
  data = data_3000,
  cores = 2,  
  chains = 2,
  iter = 5000,  # Number of iterations
  warmup = 1000
)
summary(mod_bayes_beta)

# Final selected model:Beta regression with 3 interaction terms.
mod_bayes_beta02 <- brm(
  MF_av ~ 
    northness * slope * cover +
    eastness +
    cover * type +
    coast +
    elevation +
    relative_elevation +
    TWI +
    s(x, y, bs = "tp") -1,  
  family = Beta(),  
  data = predict_s,
  cores = 2,  
  chains = 2,
  iter = 5000,  # Number of iterations
  warmup = 1000
)
summary(mod_bayes_beta02)

#Save model output
save(mod_bayes_beta02,
     file = "I:/DATA/output/MF_origi/MFav_beta_bayes.rda"
)


# DHARMa residual plots
dharmaRes <- createDHARMa(simulatedResponse = t(posterior_predict(mod_bayes_beta)), 
                          observedResponse = data_1000$MF_av, 
                          fittedPredictedResponse = apply(
                            t(posterior_epred(mod_bayes_beta)),
                            1, mean), 
                          integer = FALSE)
plot(dharmaRes)



# # beta WITHOUT intercept.
# mod_beta <- gam(
#   MF_av ~ 
#     northness * slope +
#     eastness +
#     cover * type +
#     coast +
#     elevation +
#     relative_elevation +
#     TWI +
#     s(x, y, bs = "tp") -1 ,  
#   family = betar(link = "logit"),
#   data = data_1000
# )
# summary(mod_beta)
# simulationOutput_beta <- simulateResiduals(fittedModel = mod_beta)
# plot(simulationOutput_beta)

# # Check the model
# plotResiduals(simulationOutput_beta, predict_s$relative_elevation)
# plotResiduals(simulationOutput_beta, predict_s$elevation)
# plotResiduals(simulationOutput_beta, predict_s$type)
# plotResiduals(simulationOutput_beta, predict_s$slope)

# plot the interaction.
library(ggeffects)
# Interactions of cover and forest types.
cover_type <- predict_response(
    mod_bayes_beta02, c("cover", "type"),
    margin = "mean_mode"
)
cover_type
plot(cover_type)

# Interactions of aspect, cover and slope
hist(data_1000$slope)
slo_north_cov <- predict_response(
  mod_bayes_beta02, c("cover", "northness [-1, 0, 1]", "slope[-0.382, 4.126]"),
    margin = "mean_mode" # !Check if "mean_mode" is corret here.
)
plot(slo_north_cov)

# Interactions of aspect and slope.
north_slope <- predict_response(
  mod_bayes_beta02, c("slope","northness"),
  margin = "mean_mode"
)
plot(north_slope)
