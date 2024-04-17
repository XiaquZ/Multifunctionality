library(ggplot2)
library(dplyr)
library(marginaleffects)
library(mgcv)

# load the data
load("I:/DATA/output/MF/10000samples_forBetaR.rda")

## extract 1000 samples for beta regression but with smaller sample size.
# beta with intercept
mod_beta_intercep <- gam(
  MF_av ~ type + coast + cover + elevation + eastness +
    northness + relative_elevation + slope + TWI +
    s(x, y, bs = "gp", m = 2),
  family = betar(link = "logit"),
  data = data_sampled
)
summary(mod_beta_intercep)

#### Plot marginal effects####
#############################
## Forest type based on "marginaleffects" R package.
type_margin <- predictions(mod_beta_intercep, by = "type")
save(type_margin, file = "I:/DATA/output/MF/models/forestType_marg.rda")

# plot maginal effects.
svg("I:/SVG/Regression/forestTypeMargin.svg")
ggplot(type_margin, aes(
  x = type_margin$type, y = type_margin$estimate,
  ymin = conf.low, ymax = conf.high
)) +
  geom_pointrange(fill = "#0F0E0F") +
  labs(x = "Forest type", y = "Multifunctionality index (average)") +
  theme_minimal()
dev.off()

## Distance to the coast
# Try maginaleffects package.
# library(future.apply) #parallel
# library(tictoc)
# plan("multicore", workers = 8) #set cores.
# options(marginaleffects_parallel = TRUE)
# Parallel seems doesn't help with increasing the speed...

coast_margin <- predictions(mod_beta_intercep, by = "coast")
coast_margin
save(coast_margin, file = "I:/DATA/output/MF/models/coast_margin02.rda")
# randomly select 1000 samples for ggplot.
coast_sam <- coast_margin[sample(nrow(coast_margin), 1000), ]
# ggplot
coast_margin |> select(coast, estimate, std.error, conf.low, conf.high)
ggplot(coast_sam, aes(x = coast, y = estimate)) +
  geom_ribbon(
    data = coast_sam, aes(ymin = conf.low, ymax = conf.high),
    fill = "#BF3984", alpha = 0.3
  ) +
  geom_point(size = 1, color = "#BF3984") +
  labs(
    x = "Distance to coast (scaled)",
    y = "Multifunctionality index (average)"
  ) +
  theme_minimal()
