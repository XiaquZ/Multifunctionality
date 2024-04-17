library(ggeffects)
library(magrittr)
library(mgcv)
library(ggplot2)

# load the model
# load("I:/DATA/output/MF/models/MF_avBeta_interc.rda")


load("I:/DATA/output/MF/standardized10000samples_xy.RData")
data_sampled <- data.sampled
# change min and max values for beta regression model.
min(data_sampled$MF_av)
max(data_sampled$MF_av)
data_sampled$MF_av[which(data_sampled$MF_av == 0)] <- 0.0001
data_sampled$MF_av[which(data_sampled$MF_av == 1)] <- 0.9999
levels(data_sampled$type)[1] <- "Broadleaved forest"
levels(data_sampled$type)[2] <- "Coniferous forest"
mfav <- unique(data_sampled$MF_av)
# To fixed error that predictors become matrix instead of 'numeric' after scaled
data_sampled$coast <- c(scale(data_sampled$coast))
data_sampled$cover <- c(scale(data_sampled$cover))
data_sampled$elevation <- c(scale(data_sampled$elevation))
data_sampled$eastness <- c(scale(data_sampled$eastness))
data_sampled$northness <- c(scale(data_sampled$northness))
data_sampled$relative_elevation <- c(scale(data_sampled$relative_elevation))
data_sampled$slope <- c(scale(data_sampled$slope))
data_sampled$TWI <- c(scale(data_sampled$TWI))
head(data_sampled)
save(data_sampled, file = "I:/DATA/output/MF/10000samples_forBetaR.rda")
# beta with intercept
mod_beta_intercep <- gam(
  MF_av ~ type + coast + cover + elevation + eastness +
    northness + relative_elevation + slope + TWI +
    s(x, y, bs = "gp", m = 2),
  family = betar(link = "logit"),
  data = data_sampled
)
summary(mod_beta_intercep)
save(mod_beta_intercep,
  file = "I:/DATA/output/MF/models/MF_avBeta_interc_v2.rda"
)
## Use forest type = 2 as reference in the model for marginal effects.
# mod_beta_ref2 <- gam(
#    MF_av ~ relevel(type, ref = 2) + coast + cover + elevation + eastness +
#        northness + relative_elevation + slope + TWI +
#        s(x, y, bs = "gp", m = 2),
#    family = betar(link = "logit"),
#    data = data_sampled
# )
# summary(mod_beta_ref2)

#### Create panel plots for marginal effects for each predictor.####

# forest type
load("I:/DATA/output/MF/models/MF_avBeta_interc_v2.rda")
sum <- summary(mod_beta_intercep)

dat_type <- predict_response(mod_beta_intercep, "type", mragin = "mean_mode")
dat_type
p_v <- sum$p.pv[2]
svg("I:/SVG/Regression/forestTypeMargin.svg")
gplot_type <- plot(dat_type) +
  geom_point(aes(color = dat_type$x),
    color = c("#5159CA", "#A148B8"), size = 5
  ) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = dat_type$x),
    color = c("#5159CA", "#A148B8"), width = 0, size = 1.5
  ) +
  labs(
    x = "Forest types",
    y = "Multifunctionality index (average)",
    title = NULL
  ) +
  geom_text(aes(label = paste("p < 0.001")),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    color = "black", size = 6
  ) +
  theme_light()
dev.off()
save(dat_type, file = "I:/DATA/output/MF/models/forestType_margin.rda")

# coast
dat_coast <- predict_response(mod_beta_intercep, "coast", mragin = "mean_mode")
dat_coast
p_v <- sum$p.pv[3]
svg("I:/SVG/Regression/coastMargin.svg")
gplot_coast <- plot(dat_coast) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    fill = "#DF88B9", alpha = 0.4
  ) +
  geom_line(color = "#A35180", size = 1.4) +
  labs(
    x = "Distance to coast (scaled)",
    y = "Multifunctionality index (average)",
    title = NULL
  ) +
  geom_text(aes(label = paste("p < 0.001")),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    color = "black", size = 6
  ) +
  theme_light()
dev.off()
save(dat_coast, file = "I:/DATA/output/MF/models/coast_margin.rda")

# forest cover
dat_cover <- predict_response(mod_beta_intercep, "cover", margin = "mean_mode")
dat_cover
p_v <- sum$p.pv[4]
svg("I:/SVG/Regression/coverMargin.svg")
gplot_cover <- plot(dat_cover) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    fill = "#9082E2", alpha = 0.4
  ) +
  geom_line(color = "#68587A", size = 1.4) +
  labs(
    x = "Tree cover density (scaled)",
    y = "Multifunctionality index (average)",
    title = NULL
  ) +
  geom_text(aes(label = paste("p < 0.001")),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    color = "black", size = 6
  ) +
  theme_light()
dev.off()
save(dat_cover, file = "I:/DATA/output/MF/models/cover_margin.rda")

# elevation
dat_elevation <- predict_response(mod_beta_intercep, "elevation", margin = "mean_mode")
dat_elevation
p_v <- sum$p.pv[5]
svg("I:/SVG/Regression/elevationMargin.svg")
gplot_ele <- plot(dat_elevation) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    fill = "#8ba8e7", alpha = 0.4
  ) +
  geom_line(color = "#586C7A", size = 1.4) +
  labs(
    x = "Elevation (scaled)",
    y = "Multifunctionality index (average)",
    title = NULL
  ) +
  geom_text(aes(label = paste("p < 0.001")),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    color = "black", size = 6
  ) +
  theme_light()
dev.off()
save(dat_cover, file = "I:/DATA/output/MF/models/cover_margin.rda")

# eastness
dat_eastness <- predict_response(mod_beta_intercep, "eastness", margin = "mean_mode")
dat_eastness
p_v <- sum$p.pv[6]
svg("I:/SVG/Regression/eastneddMargin.svg")
gplot_east <- plot(dat_eastness) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    color = NA,
    fill = "#8BCAE7", alpha = 0.4
  ) +
  geom_line(aes(y = dat_eastness$predicted),
    linetype = "dashed", color = "#586C7A", size = 2
  ) +
  labs(
    x = "Eastness (scaled)",
    y = "Multifunctionality index (average)",
    title = NULL
  ) +
  geom_text(aes(label = paste(("p = "), round(p_v, digits = 3))),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    color = "black", size = 6
  ) +
  theme_light()
dev.off()
save(dat_eastness, file = "I:/DATA/output/MF/models/eastness_margin.rda")

# northness
dat_northness <- predict_response(mod_beta_intercep, "northness", margin = "mean_mode")
dat_northness
p_v <- sum$p.pv[7]
svg("I:/SVG/Regression/NorthMargin.svg")
gplot_north <- plot(dat_northness) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    linetype = 0,
    fill = "#8FE4BC", alpha = 0.4
  ) +
  geom_line(aes(group = 1),
    linetype = "dashed", color = "#587A6C", size = 2
  ) +
  labs(
    x = "Northness (scaled)",
    y = "Multifunctionality index (average)",
    title = NULL
  ) +
  geom_text(aes(label = paste(("p = "), round(p_v, digits = 3))),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    color = "black", size = 6
  ) +
  theme_light()
dev.off()
save(dat_northness, file = "I:/DATA/output/MF/models/north_margin.rda")

# relative_elevation
dat_relalevation <- predict_response(mod_beta_intercep, "relative_elevation", margin = "mean_mode")
dat_relalevation
p_v <- sum$p.pv[8]
svg("I:/SVG/Regression/RelevationMargin.svg")
gplot_relaelevation <- plot(dat_relalevation) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    fill = "#bce78b", alpha = 0.4
  ) +
  geom_line(color = "#6B7A58", size = 1.4) +
  labs(
    x = "Relative elevation (scaled)",
    y = "Multifunctionality index (average)",
    title = NULL
  ) +
  geom_text(aes(label = paste("p < 0.001")),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    color = "black", size = 6
  ) +
  theme_light()
dev.off()
save(dat_relalevation, file = "I:/DATA/output/MF/models/relaElevation_margin.rda")


# slope
dat_slope <- predict_response(mod_beta_intercep, "slope", margin = "mean_mode")
dat_slope
p_v <- sum$p.pv[9]
svg("I:/SVG/Regression/slope.svg")
gplot_slope <- plot(dat_slope) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    fill = "#e7ca8b", alpha = 0.4
  ) +
  geom_line(color = "#AA8F46", size = 1.4) +
  labs(
    x = "Slope (scaled)",
    y = "Multifunctionality index (average)",
    title = NULL
  ) +
  geom_text(aes(label = paste("p < 0.001")),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    color = "black", size = 6
  ) +
  theme_light()
dev.off()
save(dat_slope, file = "I:/DATA/output/MF/models/slope_margin.rda")

# TWI
dat_twi <- predict_response(mod_beta_intercep, "TWI", margin = "mean_mode")
dat_twi
p_v <- sum$p.pv[10]
svg("I:/SVG/Regression/TWI.svg")
gplot_twi <- plot(dat_twi) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    fill = "#E7AD8B", alpha = 0.4
  ) +
  geom_line(color = "#AA6646", size = 1.4) +
  labs(
    x = "TWI (scaled)",
    y = "Multifunctionality index (average)",
    title = NULL
  ) +
  geom_text(aes(label = paste(("p = "), round(p_v, digits = 3))),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    color = "black", size = 6
  ) +
  theme_light()
dev.off()
save(dat_twi, file = "I:/DATA/output/MF/models/twi_margin.rda")
library(ggpubr)
svg("I:/SVG/Regression/Predictors.svg")
ggarrange(gplot_type, gplot_coast, gplot_cover, gplot_ele,
gplot_relaelevation, gplot_slope, gplot_twi,
gplot_east, gplot_north,
labels = c("a", "b", "c", "d", "e", "f", "g", 
"h", "I"),
ncol = 3, nrow = 3)
dev.off()
