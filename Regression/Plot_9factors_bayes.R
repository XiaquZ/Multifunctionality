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

####################################################################
#### Create panel plots for marginal effects for each predictor.####
####################################################################
# Second argument = path to .otf-file
# font_add("fa-reg", "I:/SVG/otfs/Font Awesome 6 Free-Regular-400.otf")
# font_add("fa-brands", "I:/SVG/otfs/Font Awesome 6 Free-Regular-400.otf")
font_add("fa-solid", "I:/SVG/otfs/Font Awesome 6 Free-Regular-400.otf")
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
cover_type$cover_original <- cover_type$x * sd_cover + mean_cover

ci95 <- paste0("CI: ", paste0(ci[14, 1], " - ", ci[14, 2]))
annotations <- data.frame(
    xpos = c(-Inf, Inf),
    ypos = c(Inf, Inf),
    annotateText = c(
        " (a) <span style='font-family:fa-solid;'>",
        paste(ci95)
    ),
    hjustvar = c(0, 1),
    vjustvar = c(1, 1)
) # adjust

svg("I:/SVG/MFs_v3_bayes/coverANDType.svg", width = 9, height = 9)
ggplot(
    cover_type,
    aes(x = cover_original, y = predicted, color = group, fill = group)
) + # `x` and `predicted` are standard in `ggeffects`
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
    geom_line(size = 1.5) +
    labs(
        x = "Tree cover density (%)",
        y = "Multifunctionality index (average)",
        title = NULL
    ) +
    geom_richtext(
        data = annotations, aes(
            x = xpos, y = ypos, label = annotateText,
            hjust = hjustvar, vjust = vjustvar
        ),
        size = 12, color = "black", fill = NA, label.colour = NA, show.legend = FALSE
    ) +
    ylim(0.0, 0.8) +
    # Custom color scale
    scale_color_manual(
        values = c("#5159CA", "#C8CA46"),
        name = "Forest Type") +
    scale_fill_manual(
        values = c("#5159CA", "#C8CA46"),
        name = "Forest Type"
        ) +
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
        axis.title.x = element_text(size = 32), # X-axis title size
        axis.title.y = element_text(size = 32, face = "bold"), # Y-axis title bold
        legend.text = element_text(size = 28), # Legend text larger

    )
dev.off()
# save(dat_type, file = "I:/DATA/output/MF_origi/9predictors/forestType_margin.rda")

# 3-way interaction of cover, northness and slope.
slope5_north_cov <- predict_response(
  mod_bayes_beta02, c("cover", "northness [-1, 0, 1]", "slope[-0.382]"),
    margin = "mean_mode" 
)
slope45_north_cov <- predict_response(
  mod_bayes_beta02, c("cover", "northness [-1, 0, 1]", "slope[4.127]"),
    margin = "mean_mode" 
)

save(slope5_north_cov,
    file =
        "I:/DATA/output/MF_origi/9predictors/Version2_MarginalPlots/slope5_north_cover.rda"
)
save(slope45_north_cov,
    file =
        "I:/DATA/output/MF_origi/9predictors/Version2_MarginalPlots/slope45_north_cover.rda"
)

load("I:/DATA/output/MF_origi/9predictors/Version2_MarginalPlots/slope5_north_cover.rda")	
load("I:/DATA/output/MF_origi/9predictors/Version2_MarginalPlots/slope45_north_cover.rda")
#Backtransfer the standardize predictors.
slope5_north_cov$cover_original <- slope5_north_cov$x* sd_cover + mean_cover
slope45_north_cov$cover_original <- slope45_north_cov$x* sd_cover + mean_cover
class(slope5_north_cov$group)
slope5_north_cov$group <- as.character(slope5_north_cov$group)  # Convert factor to character
slope5_north_cov$group[slope5_north_cov$group == "1"] <- "North"  # Replace "1" with "North"
slope5_north_cov$group[slope5_north_cov$group == "-1"] <- "South"  # Replace "1" with "South"
slope5_north_cov$group[slope5_north_cov$group == "0"] <- "Flat"  # Replace "0" with "Flat"

#For slope 45 degree.
slope45_north_cov$group <- as.character(slope45_north_cov$group)  # Convert factor to character
slope45_north_cov$group[slope45_north_cov$group == "1"] <- "North"  # Replace "1" with "North"
slope45_north_cov$group[slope45_north_cov$group == "-1"] <- "South"  # Replace "1" with "South"
slope45_north_cov$group[slope45_north_cov$group == "0"] <- "Flat"  # Replace "0" with "Flat"

# Create annotations df.
ci95 <- paste0("CI: ", paste0(ci[15, 1], " - ", ci[15, 2]))
annotations <- data.frame(
    xpos = c(-Inf, Inf),
    ypos = c(Inf, Inf),
    annotateText = c(
        " (b2) <span style='font-family:fa-solid;'>",
        paste(ci95, "<br>Slope = 45°")
    ),
    hjustvar = c(0, 1),
    vjustvar = c(1, 1)
) # adjust

svg("I:/SVG/MFs_v3_bayes/slope45_north_cover02.svg", width = 9, height = 9)
ggplot(
    slope45_north_cov,
    aes(x = cover_original, y = predicted, fill = group, color = group)
) + # `x` and `predicted` are standard in `ggeffects`
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
    geom_line(size = 1.5) +
    #facet_wrap(~ facet, labeller = label_both) + 
    labs(
        x = "Tree cover density (%)",
        y = NULL,
        
        title = NULL
    ) +
    geom_richtext(
        data = annotations, aes(
            x = xpos, y = ypos, label = annotateText,
            hjust = hjustvar, vjust = vjustvar
        ),
        size = 12, color = "black", fill = NA, label.colour = NA, show.legend = FALSE
    ) +
    ylim(0.0, 0.8) +
    # Custom color scale
    scale_color_manual(
        values = c("#D55E00", "#0072B2", "#009E73"),
        name = "Aspect") +
    scale_fill_manual(
        values = c("#D55E00", "#0072B2", "#009E73"),
        name = "Aspect") +
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
        axis.title.x = element_text(size = 32), # X-axis title size
        axis.title.y = element_text(size = 32, face = "bold"), # Y-axis title bold
        legend.text = element_text(size = 28), # Legend text larger

    )
dev.off()

# coast
dis_coast <- predict_response(
    mod_bayes_beta02, "coast",
    margin = "mean_mode"
)
save(dis_coast,
    file =
        "I:/DATA/output/MF_origi/9predictors/Version2_MarginalPlots/distance_coast.rda"
)
load("I:/DATA/output/MF_origi/9predictors/Version2_MarginalPlots/distance_coast.rda")
dis_coast$coast_origi <- dis_coast$x * sd_coast + mean_coast

ci95 <- paste0("CI: ", paste0(ci[7, 1], " - ", ci[7, 2]))
# create annotations df.
annotations <- data.frame(
    xpos = c(-Inf, Inf),
    ypos = c(Inf, Inf),
    annotateText = c(
        " \\(c\\) <span style='font-family:fa-solid;'>",
        paste(ci95)
    ),
    hjustvar = c(0, 1),
    vjustvar = c(1, 1)
) #<- adjust

svg("I:/SVG/MFs_v3_bayes/dist_coast.svg", width = 9, height = 9)
ggplot(
    dis_coast,
    aes(x = coast_origi, y = predicted, color = group, fill = group)
) + # `x` and `predicted` are standard in `ggeffects`
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
    geom_line(size = 1.5) +
    labs(
        x = "Distance to coast (km)",
        y = "Multifunctionality index (average)",
        title = NULL
    ) +
    geom_richtext(
        data = annotations, aes(
            x = xpos, y = ypos, label = annotateText,
            hjust = hjustvar, vjust = vjustvar
        ),
        size = 12, color = "black", fill = NA, label.colour = NA, show.legend = FALSE
    ) +
    ylim(0.0, 0.8) +
    # Custom color scale
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
        legend.position = c(0.75, 0.4),
        axis.text = element_text(size = 28),
        axis.title.x = element_text(size = 32), # X-axis title size
        axis.title.y = element_text(size = 32, face = "bold"), # Y-axis title bold
        legend.text = element_text(size = 28), # Legend text larger

    )
dev.off()

# Elevation
elevation <- predict_response(
    mod_bayes_beta02, "elevation",
    margin = "mean_mode"
)
save(elevation,
    file =
        "I:/DATA/output/MF_origi/9predictors/Version2_MarginalPlots/elevation.rda"
)
load("I:/DATA/output/MF_origi/9predictors/Version2_MarginalPlots/elevation.rda")
elevation$elevation_origi <- elevation$x * sd_elevation + mean_elevation

ci95 <- paste0("CI: ", paste0(ci[8, 1], " - ", ci[8, 2]))
# create annotations df.
annotations <- data.frame(
    xpos = c(-Inf, Inf),
    ypos = c(Inf, Inf),
    annotateText = c(
        " (d) <span style='font-family:fa-solid;'>",
        paste(ci95)
    ),
    hjustvar = c(0, 1),
    vjustvar = c(1, 1)
) #<- adjust

svg("I:/SVG/MFs_v3_bayes/elevation.svg", width = 9, height = 9)
ggplot(
    elevation,
    aes(x = elevation_origi, y = predicted, color = group, fill = group)
) + # `x` and `predicted` are standard in `ggeffects`
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
    geom_line(size = 1.5) +
    labs(
        x = "Elevation (m)",
        y = NULL,
        title = NULL
    ) +
    geom_richtext(
        data = annotations, aes(
            x = xpos, y = ypos, label = annotateText,
            hjust = hjustvar, vjust = vjustvar
        ),
        size = 12, color = "black", fill = NA, label.colour = NA, show.legend = FALSE
    ) +
    ylim(0.0, 0.8) +
    # Custom color scale
    scale_color_manual(values = "#5c64cf") +
    scale_fill_manual(values = "#5c64cf") +
    theme_light() +
    theme(
        axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(size = 28),
        legend.position = c(0.75, 0.4),
        axis.text = element_text(size = 28),
        axis.title.x = element_text(size = 32), # X-axis title size
        axis.title.y = element_text(size = 32, face = "bold"), # Y-axis title bold
        legend.text = element_text(size = 28), # Legend text larger
    )
dev.off()

# Eastness
eastness <- predict_response(
    mod_bayes_beta02, "eastness",
    margin = "mean_mode"
)
save(eastness,
    file =
        "I:/DATA/output/MF_origi/9predictors/Version2_MarginalPlots/eastness.rda"
)
load("I:/DATA/output/MF_origi/9predictors/Version2_MarginalPlots/eastness.rda")

eastness$eastness_origi <- eastness$x * sd_eastness + mean_eastness

ci95 <- paste0("CI: ", paste0(ci[4, 1], " - ", ci[4, 2]))
# create annotations df.
annotations <- data.frame(
    xpos = c(-Inf, Inf),
    ypos = c(Inf, Inf),
    annotateText = c(
        " (f) <span style='font-family:fa-solid;'>",
        paste(ci95)
    ),
    hjustvar = c(0, 1),
    vjustvar = c(1, 1)
) #<- adjust

svg("I:/SVG/MFs_v3_bayes/eastness.svg", width = 9, height = 9)
ggplot(
    eastness,
    aes(x = eastness_origi, y = predicted, color = group, fill = group)
) + # `x` and `predicted` are standard in `ggeffects`
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
     geom_line(
        linetype = "dashed", size = 1.5
    ) +
    labs(
        x = "Eastness",
        y = "Multifunctionality index (average)",
        title = NULL
    ) +
    geom_richtext(
        data = annotations, aes(
            x = xpos, y = ypos, label = annotateText,
            hjust = hjustvar, vjust = vjustvar
        ),
        size = 12, color = "black", fill = NA, label.colour = NA, show.legend = FALSE
    ) +
    ylim(0.0, 0.8) +
    # Custom color scale
    scale_color_manual(values = "#5b978d") +
    scale_fill_manual(values = "#5b978d") +
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
        axis.title.x = element_text(size = 32), # X-axis title size
        axis.title.y = element_text(size = 32, face = "bold"), # Y-axis title bold
        legend.text = element_text(size = 28), # Legend text larger
    )
dev.off()

# Relative elevation
relative_elevation <- predict_response(
    mod_bayes_beta02, "relative_elevation",
    margin = "mean_mode"
)
save(relative_elevation,
    file =
        "I:/DATA/output/MF_origi/9predictors/Version2_MarginalPlots/relative_elevation.rda"
)
load("I:/DATA/output/MF_origi/9predictors/Version2_MarginalPlots/relative_elevation.rda")
relative_elevation$relat_elevation_origi <- relative_elevation$x * sd_rela_elevation +
 mean_rela_elevation

ci95 <- paste0("CI: ", paste0(ci[9, 1], " - ", ci[9, 2]))
# create annotations df.
annotations <- data.frame(
    xpos = c(-Inf, Inf),
    ypos = c(Inf, Inf),
    annotateText = c(
        " (e) <span style='font-family:fa-solid;'>",
        paste(ci95)
    ),
    hjustvar = c(0, 1),
    vjustvar = c(1, 1)
) #<- adjust

svg("I:/SVG/MFs_v3_bayes/rela_elevation.svg", width = 9, height = 9)
ggplot(
    relative_elevation,
    aes(x = relat_elevation_origi, y = predicted, color = group, fill = group)
) + # `x` and `predicted` are standard in `ggeffects`
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
    geom_line(size = 1.5) +
    labs(
        x = "Relative elevation (m)",
        y = NULL,
        title = NULL
    ) +
    geom_richtext(
        data = annotations, aes(
            x = xpos, y = ypos, label = annotateText,
            hjust = hjustvar, vjust = vjustvar
        ),
        size = 12, color = "black", fill = NA, label.colour = NA, show.legend = FALSE
    ) +
    ylim(0.0, 0.8) +
    xlim(0, 500) +
    # Custom color scale
    scale_color_manual(values = "#dabb56") +
    scale_fill_manual(values = "#dabb56") +
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
        axis.title.x = element_text(size = 32), # X-axis title size
        axis.title.y = element_text(size = 32, face = "bold"), # Y-axis title bold
        legend.text = element_text(size = 28), # Legend text larger
    )
dev.off()

# TWI
twi <- predict_response(
    mod_bayes_beta02, "TWI",
    margin = "mean_mode"
)
save(twi,
    file =
        "I:/DATA/output/MF_origi/9predictors/Version2_MarginalPlots/TWI.rda"
)
load("I:/DATA/output/MF_origi/9predictors/Version2_MarginalPlots/TWI.rda")

twi$twi_origi <- twi$x * sd_TWI + mean_TWI

ci95 <- paste0("CI: ", paste0(ci[10, 1], " - ", ci[10, 2]))
# create annotations df.
annotations <- data.frame(
    xpos = c(-Inf, Inf),
    ypos = c(Inf, Inf),
    annotateText = c(
        " (g) <span style='font-family:fa-solid;'>",
        paste(ci95)
    ),
    hjustvar = c(0, 1),
    vjustvar = c(1, 1)
) #<- adjust

svg("I:/SVG/MFs_v3_bayes/TWI.svg", width = 9, height = 9)
ggplot(
    twi,
    aes(x = twi_origi, y = predicted, color = group, fill = group)
) + # `x` and `predicted` are standard in `ggeffects`
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
     geom_line(
        linetype = "dashed", size = 1.5
    ) +
    labs(
        x = "Topographic wetness index",
        y = NULL,
        title = NULL
    ) +
    geom_richtext(
        data = annotations, aes(
            x = xpos, y = ypos, label = annotateText,
            hjust = hjustvar, vjust = vjustvar
        ),
        size = 12, color = "black", fill = NA, label.colour = NA, show.legend = FALSE
    ) +
    ylim(0.0, 0.8) +
    # Custom color scale
    scale_color_manual(values = "#AA6646") +
    scale_fill_manual(values = "#AA6646") +
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
        axis.title.x = element_text(size = 32), # X-axis title size
        axis.title.y = element_text(size = 32, face = "bold"), # Y-axis title bold
        legend.text = element_text(size = 28), # Legend text larger
    )
dev.off()

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
