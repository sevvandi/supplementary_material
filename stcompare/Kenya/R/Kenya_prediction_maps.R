# Load the prediction data over Kenya for each model and plot
# maps of the predicted prevalences and uncertainties

library(readr)
library(dplyr)
library(ggplot2)
library(raster)
library(rmapshaper)
library(RColorBrewer)
library(patchwork) # Arranging plots
library(viridis)

# Load data
#------------------------------------------------------------------------
# Set the working directory to the "Kenya" directory
setwd()

INLA_data <- readRDS("Prediction_Maps/Data_Outputs/INLA_prediction_data.rds")
SpRF_data <- readRDS("Prediction_Maps/Data_Outputs/SpRF_prediction_data.rds")
GPBoost_data <- readRDS("Prediction_Maps/Data_Outputs/GPBoost_prediction_data.rds")
FRK_data <- readRDS("Prediction_Maps/Data_Outputs/FRK_prediction_data.rds")



# Prep the data for plotting
#------------------------------------------------------------------------
INLA_pred <- INLA_data$data[c("x","y","pred","pred_SD")]
colnames(INLA_pred) <- c("x", "y", "pred", "sd")

GPBoost_pred <- GPBoost_data$data[c("x","y","prev_pred")]
GPBoost_pred$sd <- sqrt(GPBoost_data$data$var_pred)
colnames(GPBoost_pred) <- c("x","y","pred", "sd")

SpRF_pred <- SpRF_data$data[c("x","y","pred","sd")]
colnames(SpRF_pred) <- c("x","y","pred", "sd")

FRK_pred <- FRK_data$data[c("x","y","mu_sam","sd_sam")]
colnames(FRK_pred) <- c("x","y","pred", "sd")


# Get Kenya border
#------------------------------------------------------------------------
kenyaBorder <- readRDS("Data_Inputs/Kenya_border_simple.rds")

# Plotting predicted prevalences
#------------------------------------------------------------------------

plot_common_theme <- theme(
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  plot.margin = unit(c(-0.6,0,-0.6,0), 'cm')
)


INLA_pred_plot <- ggplot(data = INLA_pred, mapping = aes(x = x, y = y, fill = pred, color = pred)) +
  geom_tile() +
  geom_polygon(data = kenyaBorder, aes(x = long, y = lat, group = group), colour = "black", size = 0.6, fill = NA)+
  scale_fill_distiller(palette = "Spectral", limits = c(0,0.8), name = "Prevalence") +
  scale_color_distiller(palette = "Spectral", limits = c(0,0.8), name = "Prevalence") +
  theme_bw() +
  coord_quickmap() +
  plot_common_theme +
  theme(
    legend.position = "left"
  )

# We have to clip the GPBoost data as it has small negative values
GPBoost_pred$clipped <- clamp(GPBoost_pred$pred, 0, 1)

GPBoost_pred_plot <- ggplot(data = GPBoost_pred, mapping = aes(x = x, y = y, fill = clipped, color = clipped)) +
  geom_tile() +
  geom_polygon(data = kenyaBorder, aes(x = long, y = lat, group = group), colour = "black", size = 0.6, fill = NA)+
  scale_fill_distiller(palette = "Spectral", limits = c(0,0.8), name = "Prevalence") +
  scale_color_distiller(palette = "Spectral", limits = c(0,0.8), name = "Prevalence") +
  theme_bw() +
  coord_quickmap() +
  plot_common_theme +
  theme(
    legend.position = "left"
  )

SpRF_pred_plot <- ggplot(data = SpRF_pred, mapping = aes(x = x, y = y, fill = pred, color = pred)) +
  geom_tile() +
  geom_polygon(data = kenyaBorder, aes(x = long, y = lat, group = group), colour = "black", size = 0.6, fill = NA)+
  scale_fill_distiller(palette = "Spectral", limits = c(0,0.8), name = "Prevalence") +
  scale_color_distiller(palette = "Spectral", limits = c(0,0.8), name = "Prevalence") +
  theme_bw() +
  coord_quickmap() +
  plot_common_theme +
  theme(
    legend.position = "left"
  )

FRK_pred_plot <- ggplot(data = FRK_pred, mapping = aes(x = x, y = y, fill = pred, color = pred)) +
  geom_tile() +
  geom_polygon(data = kenyaBorder, aes(x = long, y = lat, group = group), colour = "black", size = 0.6, fill = NA)+
  scale_fill_distiller(palette = "Spectral", limits = c(0,0.8), name = "Prevalence") +
  scale_color_distiller(palette = "Spectral", limits = c(0,0.8), name = "Prevalence") +
  theme_bw() +
  coord_quickmap() +
  plot_common_theme +
  theme(
    legend.position = "left"
  )



# Plotting uncertainties
#------------------------------------------------------------------------
INLA_sd_plot <-ggplot(data = INLA_pred, mapping = aes(x = x, y = y, fill = sd, color = sd)) +
  geom_tile()+
  geom_polygon(data = kenyaBorder, aes(x = long, y = lat, group = group), colour = "black", size = 0.6, fill = NA)+
  scale_fill_viridis(option = "rocket", breaks = c(0,0.4),  limits = c(0,0.4), name = "SD", direction = -1) +
  scale_color_viridis(option = "rocket", breaks = c(0,0.4), limits = c(0,0.4), name = "SD", direction = -1) +
  theme_bw() +
  coord_quickmap() +
  plot_common_theme

SpRF_sd_plot <- ggplot(data = SpRF_pred, mapping = aes(x = x, y = y, fill = sd, color = sd)) +
  geom_tile()+
  geom_polygon(data = kenyaBorder, aes(x = long, y = lat, group = group), colour = "black", size = 0.6, fill = NA)+
  scale_fill_viridis(option = "rocket", breaks = c(0,0.4),  limits = c(0,0.4), name = "SD", direction = -1) +
  scale_color_viridis(option = "rocket", breaks = c(0,0.4), limits = c(0,0.4), name = "SD", direction = -1) +

  theme_bw() +
  coord_quickmap() +
  plot_common_theme +
  theme(
    legend.position = "right"
  )

GPBoost_sd_plot <- ggplot(data = GPBoost_pred, mapping = aes(x = x, y = y, fill = sd, color = sd)) +
  geom_tile()+
  geom_polygon(data = kenyaBorder, aes(x = long, y = lat, group = group), colour = "black", size = 0.6, fill = NA)+
  scale_fill_viridis(option = "rocket", breaks = c(0,0.4),  limits = c(0,0.4), name = "SD", direction = -1) +
  scale_color_viridis(option = "rocket", breaks = c(0,0.4), limits = c(0,0.4), name = "SD", direction = -1) +
  theme_bw() +
  coord_quickmap() +
  plot_common_theme

FRK_sd_plot <- ggplot(data = FRK_pred, mapping = aes(x = x, y = y, fill = sd, color = sd)) +
  geom_tile() +
  geom_polygon(data = kenyaBorder, aes(x = long, y = lat, group = group), colour = "black", size = 0.6, fill = NA)+
  scale_fill_viridis(option = "rocket", breaks = c(0,0.4),  limits = c(0,0.4), name = "SD", direction = -1) +
  scale_color_viridis(option = "rocket", breaks = c(0,0.4), limits = c(0,0.4), name = "SD", direction = -1) +
  theme_bw() +
  coord_quickmap() +
  plot_common_theme +
  theme(
    legend.position = "right"
  )


# Arrange the plots
#------------------------------------------------------------------------

# Make separate figure for each model
im_width <- 20
im_height <- 8
im_res <- 300

png("Prediction_Maps/Figures/INLA_Kenya.png", units = "cm", width = im_width, height = im_height, res = im_res)
INLA_pred_plot + plot_spacer() +  INLA_sd_plot + plot_layout(widths = c(4,1.5,4))&theme(plot.margin = unit(c(-0.5,0,-0.5,0), 'cm'))
dev.off()

png("Prediction_Maps/Figures/SpRF_Kenya.png", units = "cm", width = im_width, height = im_height, res = im_res)
SpRF_pred_plot + plot_spacer() + SpRF_sd_plot + plot_layout(widths = c(4,1.5,4))&theme(plot.margin = unit(c(-0.5,0,-0.5,0), 'cm'))
dev.off()

png("Prediction_Maps/Figures/GPBoost_Kenya.png", units = "cm", width = im_width, height = im_height, res = im_res)
GPBoost_pred_plot + plot_spacer() + GPBoost_sd_plot + plot_layout(widths = c(4,1.5,4))&theme(plot.margin = unit(c(-0.5,0,-0.5,0), 'cm'))
dev.off()

png("Prediction_Maps/Figures/FRK_Kenya.png", units = "cm", width = im_width, height = im_height, res = im_res)
FRK_pred_plot  + plot_spacer()+ FRK_sd_plot + plot_layout(widths = c(4,1.5,4))&theme(plot.margin = unit(c(-0.5,0,-0.5,0), 'cm'))
dev.off()

# # Compare time taken
# #------------------------------------------------------------------------
# INLA_times
# GPB_times
# SpRF_times
# FRK_times

