library(stargazer)
load(file = 'I:/GitHub/MF/Data/models/MF_avBeta_interc_v2.rda')
load(file = 'I:/GitHub/MF/Data/models/MF_avBeta_V3.RData')
summary(mod_beta_intercep)
summary(mod_beta_intercep_v3)

library(xtable)
reg1.table <- xtable(reg1)
reg1.table
print(reg1.table, type= "html")
library(mtable)

# load package
library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(mod_beta_intercep, mod_beta_intercep_v3,
          title = "Table S2: Beta regression results
          between average multifunctionality and variables ",
          file = "I:/DATA/output/BetaReg_MF_ave3.html")
tab_model(reg1, 
          title = "Table S1: Linear regression results between macroclimate and offset ",
          file = "I:/DATA/output/LM_OffsetMacro.html")
