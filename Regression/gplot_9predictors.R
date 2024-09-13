library(ggeffects)
library(magrittr)
library(mgcv)
library(ggplot2)
#install.packages("ggtext")
library(ggtext)
#install.packages("showtext")
library(showtext)
#install.packages("gghighlight")
library(gghighlight)
#install.packages("tidyverse")
library(tidyverse)

####################################################################
#### Create panel plots for marginal effects for each predictor.####
####################################################################
# Second argument = path to .otf-file
font_add('fa-reg', 'I:/SVG/otfs/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'I:/SVG/otfs/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'I:/SVG/otfs/Font Awesome 6 Free-Solid-900.otf')
showtext_auto()

#Load the beta regression models
load("I:/DATA/output/MF_origi/MF_avBeta_orig.RData")
sum <- summary(mod_beta_intercep)

############################################
#### Make margin plots for 9 variables. ####
############################################

#Forest types
dat_type <- predict_response(mod_beta_intercep, "type", mragin = "mean_mode")
load("I:/GitHub/MF/Data/models/forestType_margin.rda")
dat_type
p_v <- sum$p.pv[2]
annotations <- data.frame(
  xpos = c(-Inf, Inf),
  ypos = c(Inf, Inf),
  annotateText = c(
    " A <span style='font-family:fa-solid;'>&#xf1bb;</span>",
    paste("p = 0.251")
  ),
  hjustvar = c(0, 1),
  vjustvar = c(1, 1)
) # adjust

svg("I:/SVG/MFs/forestTypeMargin.svg")
plot(dat_type) +
  geom_point(aes(color = dat_type$x),
    color = c("#5159CA", "#C8CA46"), size = 14
  ) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = dat_type$x),
    linetype = "dashed",
    color = c("#5159CA", "#C8CA46"), width = 0, size = 4.0
  ) +
  labs(
    x = "Forest types",
    y = "Multifunctionality index (average)",
    title = NULL
  ) +
  geom_richtext(
    data = annotations, label.colour = NA, fill = NA, aes(
      x = xpos, y = ypos,
      hjust = hjustvar, vjust = vjustvar, 
      label = annotateText, show.legend = FALSE
    ),
    size = 8, col = "black",
  ) +
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 25)
  )
dev.off()
save(dat_type, file = "I:/DATA/output/MF_origi/9predictors/forestType_margin.rda")

# coast
dat_coast <- predict_response(mod_beta_intercep, "coast", mragin = "mean_mode")
load("I:/DATA/output/MF/models/coast_margin.rda")
dat_coast
p_v <- sum$p.pv[3]
# create annotations df.
annotations <- data.frame(
  xpos = c(-Inf, Inf),
  ypos = c(Inf, Inf),
  annotateText = c(
    " B <span style='font-family:fa-solid;'>&#xf4d7;</span>",
    paste("p < 0.001")
  ),
  hjustvar = c(0, 1),
  vjustvar = c(1, 1)
) #<- adjust

svg("I:/SVG/Regression/coastMargin.svg")
plot(dat_coast) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    fill = "#F1EF4A", alpha = 0.4
  ) +
  geom_line(color = "#99914B", size = 1.4) +
  labs(
    x = "Distance to coast",
    y = NULL,
    title = NULL
  ) +
  geom_richtext(
    data = annotations, label.colour = NA, fill = NA, aes(
      x = xpos, y = ypos,
      hjust = hjustvar, vjust = vjustvar, 
      label = annotateText, show.legend = FALSE
    ),
    size = 8, col = "black",
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  theme_light() +
  theme( # plot.title = element_text(lineheight=0.5,family = "TNR"),
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 25)
  )
dev.off()
save(dat_coast, file = "I:/GitHub/MF/Data/models/coast_margin.rda")

# forest cover
dat_cover <- predict_response(mod_beta_intercep_v3, "cover", margin = "mean_mode")
load("I:/DATA/output/MF/models/cover_margin.rda")
dat_cover
p_v <- sum$p.pv[4]
# create annotations df.
annotations <- data.frame(
  xpos = c(-Inf, Inf),
  ypos = c(Inf, Inf),
  annotateText = c(
    " C <span style='font-family:fa-solid;'>&#xf042;</span>",
    paste("p < 0.001")
  ),
  hjustvar = c(0, 1),
  vjustvar = c(1, 1)
) #<- adjust
# plot
svg("I:/SVG/Regression/coverMargin.svg")
plot(dat_cover) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    fill = "#9082E2", alpha = 0.4
  ) +
  geom_line(color = "#68587A", size = 1.4) +
  labs(
    x = "Tree cover density (scaled)",
    y = NULL,
    title = NULL
  ) +
  geom_richtext(
    data = annotations, label.colour = NA, fill = NA, aes(
      x = xpos, y = ypos,
      hjust = hjustvar, vjust = vjustvar, 
      label = annotateText, show.legend = FALSE
    ),
    size = 8, col = "black",
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 25)
  )
dev.off()
save(dat_cover, file = "I:/GitHub/MF/Data/models/cover_margin.rda")

# elevation
dat_elevation <- predict_response(mod_beta_intercep_v3, "elevation",
  margin = "mean_mode"
)
load("I:/DATA/output/MF/models/elevation_margin.rda")
dat_elevation
p_v <- sum$p.pv[5]
# create annotations df.
annotations <- data.frame(
  xpos = c(-Inf, Inf),
  ypos = c(Inf, Inf),
  annotateText = c(
    " D <span style='font-family:fa-solid;'>&#xf277;</span>",
    paste("p < 0.001")
  ),
  hjustvar = c(0, 1),
  vjustvar = c(1, 1)
) #<- adjust
svg("I:/SVG/Regression/elevationMargin.svg")
plot(dat_elevation) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    fill = "#6972f0", alpha = 0.4
  ) +
  geom_line(color = "#4D4E7C", size = 1.4) +
  labs(
    x = "Elevation (scaled)",
    y = "Multifunctionality index (average)",
    title = NULL
  ) +
 geom_richtext(
    data = annotations, label.colour = NA, fill = NA, aes(
      x = xpos, y = ypos,
      hjust = hjustvar, vjust = vjustvar, 
      label = annotateText, show.legend = FALSE
    ),
    size = 8, col = "black",
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  theme_light() +
  theme( # plot.title = element_text(lineheight=0.5,family = "TNR"),
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 25)
  )
dev.off()
save(dat_elevation, file = "I:/GitHub/MF/Data/models/elevation_margin.rda")

# eastness
dat_eastness <- predict_response(mod_beta_intercep_v3, "eastness", margin = "mean_mode")
load("I:/DATA/output/MF/models/eastness_margin.rda")
dat_eastness
p_v <- sum$p.pv[6]
# create annotations df.
annotations <- data.frame(
  xpos = c(-Inf, Inf),
  ypos = c(Inf, Inf),
  annotateText = c(
    " H <span style='font-family:fa-solid;'>&#xf061;</span>",
    paste(("p = "), round(p_v, digits = 3))
  ),
  hjustvar = c(0, 1),
  vjustvar = c(1, 1)
) #<- adjust
svg("I:/SVG/Regression/EastnessMargin.svg")
plot(dat_eastness) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    color = NA,
    fill = "#8BCAE7", alpha = 0.4
  ) +
  geom_line(aes(y = dat_eastness$predicted),
    linetype = "dashed", color = "#586C7A", size = 1.4
  ) +
  labs(
    x = "Eastness (scaled)",
    y = NULL,
    title = NULL
  ) +
  geom_richtext(
    data = annotations, label.colour = NA, fill = NA, aes(
      x = xpos, y = ypos,
      hjust = hjustvar, vjust = vjustvar, 
      label = annotateText
    ),
    size = 8, col = "black",
  ) +
  theme_light() +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  theme( # plot.title = element_text(lineheight=0.5,family = "TNR"),
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 25)
  )
dev.off()
save(dat_eastness, file = "I:/GitHub/MF/Data/models/eastness_margin.rda")

# northness
dat_northness <- predict_response(mod_beta_intercep_v3, "northness", margin = "mean_mode")
load("I:/DATA/output/MF/models/north_margin.rda")
dat_northness
p_v <- sum$p.pv[7]
# create annotations df.
annotations <- data.frame(
  xpos = c(-Inf, Inf),
  ypos = c(Inf, Inf),
  annotateText = c(
    " I <span style='font-family:fa-solid;'>&#xf062;</span>",
    paste(("p = "), round(p_v, digits = 3))
  ),
  hjustvar = c(0, 1),
  vjustvar = c(1, 1)
) #<- adjust
svg("I:/SVG/Regression/NorthMargin.svg")
plot(dat_northness) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    linetype = 0,
    fill = "#8FE4BC", alpha = 0.4
  ) +
  geom_line(aes(group = 1),
    linetype = "dashed", color = "#587A6C", size = 1.4
  ) +
  labs(
    x = "Northness (scaled)",
    y = NULL,
    title = NULL
  ) +
  geom_richtext(
    data = annotations, label.colour = NA, fill = NA, aes(
      x = xpos, y = ypos,
      hjust = hjustvar, vjust = vjustvar,
      label = annotateText
    ),
    size = 8, col = "black",
  ) +
  theme_light() +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  theme( # plot.title = element_text(lineheight=0.5,family = "TNR"),
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 25)
  )
dev.off()
save(dat_northness, file = "I:/GitHub/MF/Data/models/north_margin.rda")

# relative_elevation
dat_relalevation <- predict_response(mod_beta_intercep_v3, "relative_elevation", margin = "mean_mode")
load("I:/DATA/output/MF/models/relaElevation_margin.rda")
dat_relalevation
p_v <- sum$p.pv[8]
# create annotations df.
annotations <- data.frame(
  xpos = c(-Inf, Inf),
  ypos = c(Inf, Inf),
  annotateText = c(
    " E <span style='font-family:fa-solid;'>&#xf424;&#xf277; </span>",
    paste("p < 0.001")),
  hjustvar = c(0, 1),
  vjustvar = c(1, 1)
) #<- adjust
svg("I:/SVG/Regression/RelevationMargin.svg")
plot(dat_relalevation) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    fill = "#bce78b", alpha = 0.4
  ) +
  geom_line(color = "#6B7A58", size = 1.4) +
  labs(
    x = "Relative elevation (scaled)",
    y = NULL,
    title = NULL
  ) +
  geom_richtext(
    data = annotations, label.colour = NA, fill = NA, aes(
      x = xpos, y = ypos,
      hjust = hjustvar, vjust = vjustvar,
      label = annotateText
    ),
    size = 8, col = "black",
  ) +
  theme_light() +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  theme( # plot.title = element_text(lineheight=0.5,family = "TNR"),
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 25)
  )
dev.off()
save(dat_relalevation, file = "I:/GitHub/MF/Data/models/relaElevation_margin.rda")


# slope
dat_slope <- predict_response(mod_beta_intercep_v3, "slope", margin = "mean_mode")
load("I:/DATA/output/MF/models/slope_margin.rda")
dat_slope
p_v <- sum$p.pv[9]
# create annotations df.
annotations <- data.frame(
  xpos = c(-Inf, Inf),
  ypos = c(Inf, Inf),
  annotateText = c(
    " F <span style='font-family:fa-solid;'>&#xe4b7;</span>",
    paste("p < 0.001")),
  hjustvar = c(0, 1),
  vjustvar = c(1, 1)
) #<- adjust
svg("I:/SVG/Regression/slope.svg")
plot(dat_slope) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    fill = "#e7ca8b", alpha = 0.4
  ) +
  geom_line(color = "#AA8F46", size = 1.4) +
  labs(
    x = "Slope (scaled)",
    y = NULL,
    title = NULL
  ) +
  geom_richtext(
    data = annotations, label.colour = NA, fill = NA, aes(
      x = xpos, y = ypos,
      hjust = hjustvar, vjust = vjustvar,
      label = annotateText
    ),
    size = 8, col = "black",
  ) +
  theme_light() +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  theme( # plot.title = element_text(lineheight=0.5,family = "TNR"),
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 25)
  )
dev.off()
save(dat_slope, file = "I:/GitHub/MF/Data/models/slope_margin.rda")

# TWI
dat_twi <- predict_response(mod_beta_intercep_v3, "TWI", margin = "mean_mode")
load("I:/GitHub/MF/Data/models/twi_margin.rda")
dat_twi
p_v <- sum$p.pv[10]

annotations <- data.frame(
  xpos = c(-Inf, Inf),
  ypos = c(Inf, Inf),
  annotateText = c(
    " G <span style='font-family:fa-solid;'>&#xf043;</span>",
    paste(("p = "),
          round(p_v, digits = 3))
  ),
  hjustvar = c(0, 1),
  vjustvar = c(1, 1)
)

svg("I:/SVG/Regression/TWI.svg")
plot(dat_twi) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    fill = "#E7AD8B", alpha = 0.4
  ) +
  geom_line(aes(grop = 1),
  linetype = "dashed", color = "#AA6646", size = 1.4
  ) +
  labs(
    x = "TWI (scaled)",
    y = "Multifunctionality index (average)",
    title = NULL
  ) +
  geom_richtext(
    data = annotations, label.colour = NA, fill = NA, aes(
      x = xpos, y = ypos,
      hjust = hjustvar, vjust = vjustvar,
      label = annotateText
    ),
    size = 8, col = "black",
  ) +
  theme_light() +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  theme( # plot.title = element_text(lineheight=0.5,family = "TNR"),
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 25)
  )
dev.off()
save(dat_twi, file = "I:/GitHub/MF/Data/models/twi_margin.rda")

library(ggpubr)
svg("I:/SVG/Regression/Predictors.svg")
ggarrange(gplot_type, gplot_coast, gplot_cover, gplot_ele,
  gplot_relaelevation, gplot_slope, gplot_twi,
  gplot_east, gplot_north,
  labels = c(
    "a", "b", "c", "d", "e", "f", "g",
    "h", "I"
  ),
  ncol = 3, nrow = 3
)
dev.off()
