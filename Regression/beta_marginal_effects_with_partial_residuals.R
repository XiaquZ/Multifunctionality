# Required packages
library(brms)
library(ggplot2)
library(dplyr)
library(tidybayes)
library(ggtext)

# Load the model
load("I:/DATA/output/MF_origi/MFav_beta_bayes.rda")
sum <- summary(mod_bayes_beta02)
sum
# Extract the 95% credible intervals
ci <- sum$fixed[, 3:4]
ci <- round(ci[, 1:2], digits = 2)

# 1. Extract model data and add fitted values + residuals
model_data <- mod_bayes_beta02$data %>%
  add_epred_draws(mod_bayes_beta02, re_formula = NA) %>%
  group_by(.row) %>%
  summarise(fitted_response = mean(.epred)) %>%
  bind_cols(mod_bayes_beta02$data) %>%
  mutate(
    raw_resids = MF_av - fitted_response,
    partial_resid = fitted_response + raw_resids
  )

hist(model_data$raw_resids)
hist(model_data$partial_resid)

# 2. Reverse standardization
##### Backtranform the standardize predictors.####
load("./Data/10000samples_orig_MF_9predicts.RData")
predict_s$MF_av <- as.numeric(unlist(predict_s$MF_av))
predict_s$MF_0.8T <- as.numeric(unlist(predict_s$MF_0.8T))
predict_s <- as_tibble(predict_s)

class(predict_s$type)
predict_s$type[which(predict_s$type == 1)] <- "Broadleaved forest"
predict_s$type[which(predict_s$type == 2)] <- "Coniferous forest"
predict_s$type <- as.factor(predict_s$type)
head(predict_s)

# Prepare mean and sd for backtransformation.
mean_slope <- mean(predict_s$slope, na.rm = TRUE)
sd_slope <- sd(predict_s$slope, na.rm = TRUE)

mean_northness <- mean(predict_s$northness, na.rm = TRUE)
sd_northness <- sd(predict_s$northness, na.rm = TRUE)

mean_cover <- mean(predict_s$cover, na.rm = TRUE)
sd_cover <- sd(predict_s$cover, na.rm = TRUE)

mean_elevation <- mean(predict_s$elevation, na.rm = TRUE)
sd_elevation <- sd(predict_s$elevation, na.rm = TRUE)

mean_rela_elevation <- mean(predict_s$relative_elevation, na.rm = TRUE)
sd_rela_elevation <- sd(predict_s$relative_elevation, na.rm = TRUE)

mean_TWI <- mean(predict_s$TWI, na.rm = TRUE)
sd_TWI <- sd(predict_s$TWI, na.rm = TRUE)

mean_eastness <- mean(predict_s$eastness, na.rm = TRUE)
sd_eastness <- sd(predict_s$eastness, na.rm = TRUE)

mean_coast <- mean(predict_s$coast, na.rm = TRUE)
sd_coast <- sd(predict_s$coast, na.rm = TRUE)

# Add to the model data.
model_data <- model_data %>%
  mutate(
    coast_original = coast * sd_coast + mean_coast,
    elevation_original = elevation * sd_elevation + mean_elevation,
    relative_elevation_original = relative_elevation * sd_rela_elevation +
      mean_rela_elevation,
    cover_original = cover * sd_cover + mean_cover,
    slope_original = slope * sd_slope + mean_slope,
    TWI_original = TWI * sd_TWI + mean_TWI,
    eastness_original = eastness * sd_eastness + mean_eastness,
    aspect_group = case_when(
    northness <= -0.5 ~ "South",
    northness >=  0.5 ~ "North",
    TRUE           ~ "Flat"
  ))
# For plotting northness

# 3. Plotting marginal effects + partial residuals for individual predictors
plot_main_effect <- function(varname, varlabel, mean_val, sd_val) {
  eff <- conditional_effects(mod_bayes_beta02, effects = varname, re_formula = NA)[[varname]] %>%
    mutate(var_original = .data[[varname]] * sd_val + mean_val)

  ggplot() +
    geom_point(
      data = model_data,
      aes_string(x = paste0(varname, "_original"), y = "partial_resid"),
      alpha = 0.15, size = 2.5
    ) +
    geom_ribbon(
      data = eff,
      aes(x = var_original, ymin = lower__, ymax = upper__),
      alpha = 0.5, fill = "#99914B"
    ) +
    geom_line(
      data = eff,
      aes(x = var_original, y = estimate__),
      color = "#99914B", size = 1.5
    ) +
    labs(x = varlabel, y = "Multifunctionality index", title = paste("Effect of", varlabel)) +
    theme_light() +
    theme(
      axis.line = element_line(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      text = element_text(size = 28),
      axis.text = element_text(size = 28),
      axis.title.x = element_text(size = 32),
      axis.title.y = element_text(size = 32, face = "bold"),
      legend.text = element_text(size = 28),
      legend.position = c(0.75, 0.2)
    )
}

# 4. Plot interaction: cover × forest_type
eff_cf <- conditional_effects(
  mod_bayes_beta02, effects = "cover:type", re_formula = NA
  )[["cover:type"]] %>%
  mutate(cover_original = cover * sd_cover + mean_cover)

ci95_covertype <- paste0("CI: ", paste("[", ci[14, 1], ",", ci[14, 2], "]"))

svg("I:/SVG/MFs_v3_bayes/withPartialResid/coverANDType.svg", width = 10, height = 9)
 ggplot() +
  geom_point(
    data = model_data,
    aes(
      x = cover_original,
      y = partial_resid,
      color = type
    ),
    alpha = 0.15, size = 2.5
  ) +
  geom_ribbon(
    data = eff_cf,
    aes(x = cover_original, ymin = lower__, ymax = upper__, fill = type),
    alpha = 0.5
  ) +
  geom_line(
    data = eff_cf,
    aes(x = cover_original, y = estimate__, color = type),
    size = 1.5
  ) +
  labs(
    x = "Cover (%)",
    y = "Multifunctionality index (average)",
    title = paste("(a) Cover × Forest type  ", ci95_covertype)
  ) +
  ylim(0.0, 0.8) +
  scale_color_manual(
    values = c("#5159CA", "#C8CA46"),
    name = "Forest type"
  ) +
  scale_fill_manual(
    values = c("#5159CA", "#C8CA46"),
    name = "Forest type"
  ) +
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 28),
    axis.text = element_text(size = 28),
    axis.title.x = element_text(size = 32),
    axis.title.y = element_text(size = 32, face = "bold"),
    legend.text = element_text(size = 28),
    legend.position = c(0.75, 0.2)
  )
dev.off()

# 5. Plot interaction: cover × northness × slope
ci95_slope <- paste0("CI: ", paste("[",ci[15, 1], ",", ci[15, 2], "]"))

slope_std5 <- (5 - mean_slope) / sd_slope

  eff_cns <- conditional_effects(mod_bayes_beta02,
    effects = "cover:northness",
    conditions = data.frame(slope = slope_std5),
    re_formula = NA
  )[["cover:northness"]] %>%
    mutate(cover_original = cover * sd_cover + mean_cover,
    aspect_group = case_when(
      northness <= -0.5 ~ "South",
      northness >=  0.5 ~ "North",
      TRUE              ~ "Flat"
    )
    )

head(eff_cns)
svg(
  "I:/SVG/MFs_v3_bayes/withPartialResid/slope5_north_cover.svg",
 width = 10,
  height = 9
  )
  ggplot() +
    geom_point(
      data = model_data,
      aes(x = cover_original, y = partial_resid, color = aspect_group),
      alpha = 0.4, size = 2.5
    ) +
    geom_ribbon(
      data = eff_cns,
      aes(x = cover_original, ymin = lower__, ymax = upper__, fill = aspect_group),
      alpha = 0.5
    ) +
    geom_line(
      data = eff_cns,
      aes(x = cover_original, y = estimate__, color = aspect_group),
      size = 1.5
    ) +
    labs(
      x = "Cover (%)",
      y = NULL,
      title = paste(
        "(b) Cover × Northness at slope = 5", "°\n", ci95_slope
        ),
      color = "Northness", fill = "Northness"
    ) +
    ylim(0.0, 0.8) +
    scale_color_manual(
      values = c("South"="#E69F00", "Flat"="#009E73", "North" = "#0072B2"),
      name = "Aspect") +
    scale_fill_manual(
      values = c("South"="#E69F00", "Flat"="#009E73", "North" = "#0072B2"),
      name = "Aspect") +
    theme_light() +
    theme(
      axis.line = element_line(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      text = element_text(size = 28),
      axis.text = element_text(size = 28),
      axis.title.x = element_text(size = 32),
      axis.title.y = element_text(size = 32, face = "bold"),
      legend.text = element_text(size = 28),
      legend.position = c(0.75, 0.2)
    )
dev.off()

# Generate plot coast

plot_coast <- plot_main_effect("coast", "Distance to coast (km)", mean_coast, sd_coast)


plot_elevation <- plot_main_effect("elevation", "Elevation (m)", mean_elevation, sd_elevation)
plot_rel_elev <- plot_main_effect("relative_elevation", "Relative elevation (m)", mean_relative_elevation, sd_relative_elevation)

plot_cns_5 <- plot_cover_north_slope(5, mean_slope, sd_slope, ci95_slope)

plot_cns_5
plot_cns_45 <- plot_cover_north_slope(45, mean_slope, sd_slope)

# To display or save the plots:
# print(plot_coast)
# print(plot_elevation)
# print(plot_rel_elev)
# print(plot_cover_forest)
# print(plot_cns_5)
# print(plot_cns_45)
