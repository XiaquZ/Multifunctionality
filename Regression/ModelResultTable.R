library(stargazer)
load(file = "I:/DATA/output/MF_origi/MFav_beta_bayes.rda")
load(file = "I:/DATA/output/MF_origi/MFsingleT_zero_inflated_negbinomial.rda")
load(file = "I:/DATA/output/regression_macroCHELSA_VS_offset.RData")

summary(mod_bayes_beta02)
summary(mod_nb)
summary(reg1)

# load package
library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(mod_bayes_beta02,
    title = "Table S3: Bayesian beta regression for average multifunctionality
     with environmental predictors and spatial smoothing ",
    transform = NULL,
    file = "I:/DATA/output/MF_origi/Beta_bayes.html"
)

tab_model(reg1,
    title = "Table S1: Linear regression
          results between macroclimate and offset ",
    file = "I:/DATA/output/LM_OffsetMacro.html"
)

tab_model(mod_beta_intercep_v3, transform = NULL)

load(file = "I:/DATA/output/MF/PoissonWithIntercep_MF_V2.RData")
summary(mod_nb)
tab_model(mod_nb,
    title = "Table S2: Bayesian zero-inflated negative binomial results
    between single-threshold multifunctionality and variables ",
    # transform = NULL,
    file = "I:/DATA/output/MF_origi/zerong_bayes_singleT.html"
)
