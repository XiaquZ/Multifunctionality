library(ggeffects)
####Methods from Xiaqu for beta model####

# Get model data and add residuals
model_data <- mod_bayes_beta02$data %>%
  mutate(
    fitted_logit = fitted(mod_bayes_beta02, summary = TRUE)[, "Estimate"], 
    pearson_resids = residuals(mod_bayes_beta02, type = "pearson", summary = TRUE)[, "Estimate"],
    
    # Convert fitted values from logit scale to response scale (0-1)
    fitted_response = plogis(fitted_logit),

    # Compute partial residuals in response scale
    partial_resid = fitted_response + pearson_resids,
  )

# For model_data (partial residuals)
model_data <- model_data %>%
  mutate(
    cover_original = (cover * sd_cover) + mean_cover # Reverse standardization
  )

# ggplot for paper.
ggplot() +
  # Add partial residuals first (as background points)
  geom_point(
    data = model_data,
    aes(x = coast_original, y = partial_resid),
    alpha = 0.15, # More transparent than original
    size = 2.5 # Slightly larger points
  ) +
  # Your original marginal effects plot layers
  geom_ribbon(
    data = dis_coast,
    aes(x = coast_origi, ymin = conf.low, ymax = conf.high, fill = group),
    alpha = 0.5
  ) +
  geom_line(
    data = dis_coast,
    aes(x = coast_origi, y = predicted, color = group),
    size = 1.5
  ) +
  # Your existing formatting
  labs(
    x = "Distance to coast (km)",
    y = "Multifunctionality index (average)",
    title = NULL
  ) +
  geom_richtext(
    data = annotations,
    aes(x = xpos, y = ypos, label = annotateText, hjust = hjustvar, vjust = vjustvar),
    size = 12, color = "black", fill = NA, label.colour = NA, show.legend = FALSE
  ) +
  ylim(0.0, 0.8) +
  scale_color_manual(values = "#99914B") +
    scale_fill_manual(values = "#99914B") +
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 28),
    legend.position = c(0.75, 0.2),
    axis.text = element_text(size = 28),
    axis.title.x = element_text(size = 32),
    axis.title.y = element_text(size = 32, face = "bold"),
    legend.text = element_text(size = 28)
  )

# Plot partial residuals vs. marginal effects
ggplot() +
  # Partial residuals (actual data)
  geom_point(
    data = model_data,
    aes(x = cover, y = partial_resid, color = type),
    alpha = 0.2
  ) +
  # Marginal effects (model average)
  geom_line(
    data = cover_type,
    aes(x = x, y = predicted, color = group),
    linewidth = 1.5
  ) +
  labs(title = "Partial Residuals vs. Marginal Effects")

# For three-way interaction (I didn't figure out this yet)
slope45_north_cov <- predict_response(
  mod_bayes_beta02, c("cover[all]", "northness [-1, 0, 1]", "slope[4.127]"),
    margin = "mean_mode" 
)

ggplot() +
  # Add partial residuals first (as background points)
  geom_point(
    data = model_data,
    aes(x = cover_original, y = partial_resid, color = as.factor(northness)),
    alpha = 0.15, # More transparent than original
    size = 2.5 # Slightly larger points
  ) +
  
  # Confidence ribbon from predictions
  geom_ribbon(
    data = slope45_north_cov,
    aes(x = cover, ymin = conf.low, ymax = conf.high, fill = as.factor(northness)),
    alpha = 0.3
  ) +
  
  # Line for predicted values
  geom_line(
    data = slope45_north_cov,
    aes(x = cover, y = predicted, color = as.factor(northness)),
    size = 1.5
  ) +
  
  # Labels and theme
  labs(
    x = "Tree cover density (%)",
    y = "Multifunctionality index (average)",
    color = "Northness",
    fill = "Northness",
    title = "Effect of Northness, Slope, and Cover on Multifunctionality Index"
  ) +
  
  ylim(0.0, 0.8) +
  scale_color_manual(values = c("#5159CA", "#C8CA46", "#E76F51")) +
  scale_fill_manual(values = c("#5159CA", "#C8CA46", "#E76F51")) +
  
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 28),
    legend.position = "right",
    axis.text = element_text(size = 28),
    axis.title.x = element_text(size = 32),
    axis.title.y = element_text(size = 32, face = "bold"),
    legend.text = element_text(size = 28)
  )


#### Methods from Koenraad. ####
library(brms)
library(tidybayes)
library(tidyverse)

# Fit negative binomial model
# fit <- brm(y ~ x + z, data = dat, family = negbinomial(), seed = 123, cores = 4)

# 2. Get fitted values and partial residuals
dat_with_preds <- predict_s %>%
  add_epred_draws(mod_bayes_beta02, re_formula = NA) %>%
  group_by(.row) %>%
  summarise(mod_bayes_beta02 = mean(.epred)) %>%
  mutate(
    y = predict_s$MF_av,
    x = predict_s$cover,  # still on standardized scale
    x_unscaled = predict_s$cover_raw,  # use original x for plotting
    resid = y - mod_bayes_beta02, # Compute residuals
    partial_resid = mod_bayes_beta02 + resid # Compute partial residuals
  )
  %>%
  bind_cols(predict_s %>% select(-MF_av)) # Reattach all predictors
hist(dat_with_preds$partial_resid)


# Assuming `fit` is your brms model
me <- conditional_effects(mod_bayes_beta02,
  effects = c(cover, type),  # Interaction term
  plot = FALSE
) # Change "slope" to the predictor of interest

# Reverse standardization of cover for plotting
me_df <- me_df %>%
  mutate(cover_unscaled = cover * sd_cover + mean_cover)

# Plot marginal effects with unscaled cover
ggplot() +
  # Marginal effects
  geom_line(data = me_df,
   aes(x = cover_unscaled, y = estimate__, color = type),
    size = 1) +
  geom_ribbon(data = me_df,
   aes(x = cover_unscaled, ymin = lower__, ymax = upper__, fill = type),
    alpha = 0.2) +

  # Partial residuals
  geom_point(data = dat_with_preds,
   aes(x = x_unscaled, y = partial_resid),
    color = "black", alpha = 0.5) +
  labs(x = "Tree Cover Density (%)", y = "Predicted Multifunctionality Index") +
  theme_minimal()


# 4. Plot marginal effects with partial residuals
marg_plot <- plot(me, plot = FALSE)[[1]] + 
  geom_point(data = dat_with_preds,
             mapping = aes(x = x_unscaled,
              y = partial_resid,
              color = type),
              inherit.aes = FALSE,  # Change "slope" to match the marginal effect variable
             alpha = 0.4, color = "red") +
  labs(title = "Marginal effects with partial residuals (red dots)")

# Show plot
marg_plot

