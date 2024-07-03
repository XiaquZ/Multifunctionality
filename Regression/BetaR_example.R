library(mgcv)
####Use 'gam()' for faster beta regression accounting spatial autocorrelation.####
#beta without intercept.
mod_beta <- gam(
  MF_av ~ type + coast + cover + elevation + eastness +
    northness + relative_elevation + slope + TWI +
    s(x, y, bs = "gp", m = 2) - 1,
    #"x", "y" refer to longtitude and latitude. "gp" means Gussian process
    #if want to include the intercept in the results, remove '-1'.
  family = betar(link = "logit"),
  data = mydata
)
summary(mod_beta)