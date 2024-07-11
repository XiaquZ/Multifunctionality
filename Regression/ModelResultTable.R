library(stargazer)
load(file = "I:/GitHub/MF/Data/models/MF_avBeta_interc_v2.rda")
load(file = "I:/GitHub/MF/Data/models/MF_avBeta_V3.RData")
load(file = "I:/DATA/output/regression_macroCHELSA_VS_offset.RData")
summary(mod_beta_intercep)
summary(mod_beta_intercep_v3)

# load package
library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(mod_beta_intercep_v3,
    title = "Table S2: Beta regression results
    between average multifunctionality and variables ",
    transform = NULL,
    file = "I:/DATA/output/BetaReg_MF_ave3.html"
)

tab_model(reg1,
    title = "Table S1: Linear regression
          results between macroclimate and offset ",
    ile = "I:/DATA/output/LM_OffsetMacro.html"
)

tab_model(mod_beta_intercep_v3, transform = NULL)

load(file = "I:/DATA/output/MF/PoissonWithIntercep_MF_V2.RData")
summary(mod_pois_intercep)
tab_model(mod_pois_intercep,
    title = "Table S3: Poisson regression results
    between single-threshold multifunctionality and variables ",
    #transform = NULL,
    file = "I:/DATA/output/PoissonReg_MFversion2_singleT.html"
)
