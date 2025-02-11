library(ggeffects)
library(magrittr)
library(mgcv)
library(ggplot2)
# install.packages("ggtext")
library(ggtext)
# install.packages("showtext")
library(showtext)
# install.packages("gghighlight")
library(gghighlight)
# install.packages("tidyverse")
library(tidyverse)

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

####################################################################
#### Create panel plots for marginal effects for each predictor.####
####################################################################
# Second argument = path to .otf-file
font_add("fa-reg", "I:/SVG/otfs/Font Awesome 6 Free-Regular-400.otf")
font_add("fa-brands", "I:/SVG/otfs/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-solid", "I:/SVG/otfs/Font Awesome 6 Free-Solid-900.otf")
showtext_auto()

# Load the beta regression models
load("I:/DATA/output/MF_origi/MFav_beta_bayes.rda")
sum <- summary(mod_bayes_beta02)
sum
ci <- sum$fixed[, 3:4]
ci <- round(ci[, 1:2], digits = 2)
############################################
#### Make margin plots for 9 variables. ####
############################################

# Forest types
cover_type <- predict_response(
    mod_bayes_beta02, c("cover", "type"),
    margin = "mean_mode"
)
save(cover_type,
    file =
        "I:/DATA/output/MF_origi/9predictors/Version2_MarginalPlots/cover_type_margin.rda"
)

load("I:/DATA/output/MF_origi/9predictors/Version2_MarginalPlots/cover_type_margin.rda")
cover_type
ci95 <- paste0("CI: ", paste0(ci[14, 1], " - ", ci[14, 2]))
annotations <- data.frame(
    xpos = c(-Inf, Inf),
    ypos = c(Inf, Inf),
    annotateText = c(
        " A <span style='font-family:fa-solid;'>",
        paste(ci95)
    ),
    hjustvar = c(0, 1),
    vjustvar = c(1, 1)
) # adjust

svg("I:/SVG/MFs_v3_bayes/coverANDType.svg", width = 8, height = 8)
ggplot(
    cover_type,
    aes(x = x, y = predicted, color = group, fill = group)
) + # `x` and `predicted` are standard in `ggeffects`
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4) +
    geom_point(size = 1) +
    labs(
        x = "Forest types * Tree cover density",
        y = "Multifunctionality index (average)",
        title = NULL
    ) +
    geom_richtext(
        data = annotations, aes(
            x = xpos, y = ypos, label = annotateText,
            hjust = hjustvar, vjust = vjustvar
        ),
        size = 8, color = "black", fill = NA, label.colour = NA, show.legend = FALSE
    ) +
    ylim(0.0, 0.8) +
    # Custom color scale
    scale_color_manual(values = c("#5159CA", "#C8CA46")) +
    scale_fill_manual(values = c("#5159CA", "#C8CA46")) +
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
# save(dat_type, file = "I:/DATA/output/MF_origi/9predictors/forestType_margin.rda")

# coast
dat_coast <- predict_response(mod_beta_intercep, "coast", mragin = "mean_mode")
load("I:/DATA/output/MF_origi/9predictors/coast_margin.rda")
dat_coast
p_v <- sum$p.pv[4]
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

svg("I:/SVG/MFs/y_lmit/coastMargin02.svg", width = 8, height = 8)
plot(dat_coast) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
        fill = "#F1EF4A", alpha = 0.4
    ) +
    geom_line(color = "#99914B", size = 1.4) +
    labs(
        # x = "Distance to coast (km)",
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
    scale_x_continuous("Distance to coast (km)", limits = c(0.0, 400.0)) +
    scale_y_continuous(limits = c(0.2, 0.8)) +
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
save(dat_coast, file = "I:/DATA/output/MF_origi/9predictors/coast_margin.rda")

# forest cover
dat_cover <- predict_response(mod_beta_intercep, "cover", margin = "mean_mode")
load("I:/DATA/output/MF_origi/9predictors/cover_margin.rda")
dat_cover
p_v <- sum$p.pv[3]
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
svg("I:/SVG/MFs/y_lmit/coverMargin02.svg", width = 8, height = 8)
plot(dat_cover) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
        fill = "#9082E2", alpha = 0.4
    ) +
    geom_line(color = "#68587A", size = 1.4) +
    labs(
        x = "Tree cover density (%)",
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
    ylim(0.2, 0.8) +
    theme(
        axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(size = 25)
    )
dev.off()
save(dat_cover, file = "I:/DATA/output/MF_origi/9predictors/cover_margin.rda")

# elevation
dat_elevation <- predict_response(mod_beta_intercep, "elevation",
    margin = "mean_mode"
)
load("I:/DATA/output/MF_origi/9predictors/elevation_margin.rda")
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
svg("I:/SVG/MFs/y_lmit/elevationMargin02.svg", width = 8, height = 8)
plot(dat_elevation) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
        fill = "#6972f0", alpha = 0.4
    ) +
    geom_line(color = "#4D4E7C", size = 1.4) +
    labs(
        x = "Elevation (m)",
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
    ylim(0.2, 0.8) +
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
save(dat_elevation, file = "I:/DATA/output/MF_origi/9predictors/elevation_margin.rda")

# eastness
dat_eastness <- predict_response(mod_beta_intercep, "eastness", margin = "mean_mode")
load("I:/DATA/output/MF_origi/9predictors/eastness_margin.rda")
dat_eastness
p_v <- sum$p.pv[8]
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
svg("I:/SVG/MFs/y_lmit/EastnessMargin02.svg", width = 8, height = 8)
plot(dat_eastness) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
        color = NA,
        fill = "#8BCAE7", alpha = 0.4
    ) +
    geom_line(aes(y = dat_eastness$predicted),
        linetype = "dashed", color = "#586C7A", size = 1.4
    ) +
    labs(
        x = "Eastness",
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
    ylim(0.2, 0.8) +
    theme_light() +
    # scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
    # scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
    theme( # plot.title = element_text(lineheight=0.5,family = "TNR"),
        axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(size = 25)
    )
dev.off()
save(dat_eastness, file = "I:/DATA/output/MF_origi/9predictors/eastness_margin.rda")

# northness
dat_northness <- predict_response(mod_beta_intercep, "northness", margin = "mean_mode")
load("I:/DATA/output/MF_origi/9predictors/north_margin.rda")
dat_northness
p_v <- sum$p.pv[9]
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

svg("I:/SVG/MFs/y_lmit/NorthMargin02.svg", width = 8, height = 8)
plot(dat_northness) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
        linetype = 0,
        fill = "#8FE4BC", alpha = 0.4
    ) +
    geom_line(aes(group = 1),
        linetype = "dashed", color = "#587A6C", size = 1.4
    ) +
    labs(
        x = "Northness",
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
    ylim(0.2, 0.8) +
    theme( # plot.title = element_text(lineheight=0.5,family = "TNR"),
        axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(size = 25)
    )
dev.off()
save(dat_northness, file = "I:/DATA/output/MF_origi/9predictors/north_margin.rda")

# relative_elevation
dat_relalevation <- predict_response(mod_beta_intercep, "relative_elevation", margin = "mean_mode")
load("I:/DATA/output/MF_origi/9predictors/relaElevation_margin.rda")
dat_relalevation
p_v <- sum$p.pv[6]
# create annotations df.
annotations <- data.frame(
    xpos = c(-Inf, Inf),
    ypos = c(Inf, Inf),
    annotateText = c(
        " E <span style='font-family:fa-solid;'>&#xf424;&#xf277; </span>",
        paste("p < 0.001")
    ),
    hjustvar = c(0, 1),
    vjustvar = c(1, 1)
) #<- adjust
svg("I:/SVG/MFs/y_lmit/RelevationMargin02.svg", width = 8, height = 8)
plot(dat_relalevation) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
        fill = "#bce78b", alpha = 0.4
    ) +
    geom_line(color = "#6B7A58", size = 1.4) +
    labs(
        x = "Relative elevation (m)",
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
    ylim(0.2, 0.8) +
    theme(
        axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(size = 25)
    )
dev.off()
save(dat_relalevation, file = "I:/DATA/output/MF_origi/9predictors/relaElevation_margin.rda")


# slope
dat_slope <- predict_response(mod_beta_intercep, "slope", margin = "mean_mode")
load("I:/DATA/output/MF_origi/9predictors/slope_margin.rda")
dat_slope
p_v <- sum$p.pv[7]
# create annotations df.
annotations <- data.frame(
    xpos = c(-Inf, Inf),
    ypos = c(Inf, Inf),
    annotateText = c(
        " F <span style='font-family:fa-solid;'>&#xe4b7;</span>",
        paste("p < 0.001")
    ),
    hjustvar = c(0, 1),
    vjustvar = c(1, 1)
) #<- adjust
svg("I:/SVG/MFs/y_lmit/slope02.svg", width = 8, height = 8)
plot(dat_slope) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
        fill = "#e7ca8b", alpha = 0.4
    ) +
    geom_line(color = "#AA8F46", size = 1.4) +
    labs(
        x = "Slope (°)",
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
    ylim(0.2, 0.8) +
    theme(
        axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(size = 25)
    )
dev.off()
save(dat_slope, file = "I:/DATA/output/MF_origi/9predictors/slope_margin.rda")

# TWI
dat_twi <- predict_response(mod_beta_intercep, "TWI", margin = "mean_mode")
load("I:/DATA/output/MF_origi/9predictors/twi_margin.rda")
dat_twi
p_v <- sum$p.pv[10]

annotations <- data.frame(
    xpos = c(-Inf, Inf),
    ypos = c(Inf, Inf),
    annotateText = c(
        " G <span style='font-family:fa-solid;'>&#xf043;</span>",
        paste(
            ("p = "),
            round(p_v, digits = 3)
        )
    ),
    hjustvar = c(0, 1),
    vjustvar = c(1, 1)
)

svg("I:/SVG/MFs/y_lmit/TWI02.svg", width = 8, height = 8)
plot(dat_twi) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
        fill = "#E7AD8B", alpha = 0.4
    ) +
    geom_line(
        linetype = "dashed", color = "#AA6646", size = 1.4
    ) +
    labs(
        x = "Topographic wetness index",
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
    ylim(0.2, 0.8) +
    theme(
        axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(size = 25)
    )
dev.off()
save(dat_twi, file = "I:/DATA/output/MF_origi/9predictors/twi_margin.rda")

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
