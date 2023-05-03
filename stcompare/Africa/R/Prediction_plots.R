# Make individual prevalence and uncertainty plots for each model on each
# of the three input data sets.
library(ggplot2)
library(dplyr)
library(patchwork)
library(viridis)
library(RColorBrewer)
library(raster)

# Set working directory to the Africa folder
setwd()

# Load the predictions from each model
# -------------------------------------------------------------------------

# Trained on observations
INLA_obs <- readRDS("Data_Outputs/INLA_868_observation_data.rds")
SpRF_obs <- readRDS("Data_Outputs/SpRF_868_observation_data.rds")
GPBoost_obs <- readRDS("Data_Outputs/GPBoost_868_observation_data.rds")
FRK_obs <- readRDS("Data_Outputs/FRK_868_observation_data.rds")

# Trained on binomial simulated data at observation locations
INLA_binomial_obs <- readRDS("Data_Outputs/INLA_840_binomial_observation_locations.rds")
SpRF_binomial_obs <- readRDS("Data_Outputs/SpRF_840_binomial_observation_locations.rds")
GPBoost_binomial_obs <- readRDS("Data_Outputs/GPBoost_840_binomial_observation_locations.rds")
FRK_binomial_obs <- readRDS("Data_Outputs/FRK_840_binomial_observation_locations.rds")

# Trained on binomial simulated data at uniformly selected locations
INLA_uniform <- readRDS("Data_Outputs/INLA_1000_binomial_uniform.rds")
SpRF_uniform <- readRDS("Data_Outputs/SpRF_1000_binomial_uniform.rds")
GPBoost_uniform <- readRDS("Data_Outputs/GPBoost_1000_binomial_uniform.rds")
FRK_uniform <- readRDS("Data_Outputs/FRK_1000_binomial_uniform.rds")

# GPBoost predictions can lie outside [0,1], clip them for the plot
GPBoost_obs$data$pred <- clamp(GPBoost_obs$data$pred,0,1)
GPBoost_binomial_obs$data$pred <- clamp(GPBoost_binomial_obs$data$pred,0,1)
GPBoost_uniform$data$pred <- clamp(GPBoost_uniform$data$pred,0,1)



All_list <- list(INLA_obs, INLA_uniform, INLA_binomial_obs,
                 FRK_obs, FRK_uniform, FRK_binomial_obs,
                 SpRF_obs, SpRF_uniform, SpRF_binomial_obs,
                 GPBoost_obs, GPBoost_uniform, GPBoost_binomial_obs)



# Make plots for each model/data combination
#------------------------------------------------------------------------
Africa_border <- readRDS("Data_Inputs/Africa_border.rds")

im_width <- 12
im_height <- 11
im_res <- 300

make_plots <- function(data_container){
  # Add prevalence legend only for the first plot
  if(data_container$metadata$model == "INLA" & data_container$metadata$type == "observation_data"){
    legend_pos <- c(0.22, 0.27)
  }
  else{
    legend_pos <- "none"
  }
  # Add uncertainty legend to the observation plot for each model
  if(data_container$metadata$type == "observation_data"){
    uc_legend_pos <- c(0.22, 0.27)
  }
  else{
    uc_legend_pos <- "none"
  }

  pred_plot <- ggplot(data = data_container$data, mapping = aes(x = x, y = y, fill = pred, color = pred)) +
    geom_tile() +
    geom_polygon(data = Africa_border, aes(x = long, y = lat, group = group), colour = "#00000015", size = 0.1, fill = NA) +
    scale_fill_distiller(palette = "Spectral", limits = c(0,1), name = "Prevalence") +
    scale_color_distiller(palette = "Spectral", limits = c(0,1), name = "Prevalence") +
    theme_bw() +
    coord_quickmap() +
    theme(
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = legend_pos,
      plot.margin = unit(c(-0.6,-1,-0.6,-1), 'cm')
    )


  uncertainty_plot <- ggplot(data = data_container$data, mapping = aes(x = x, y = y, fill = sd, color = sd)) +
    geom_tile()+
    geom_polygon(data = Africa_border, aes(x = long, y = lat, group = group), colour = "#00000015", size = 0.1, fill = NA) +
    scale_fill_viridis(option  = "rocket", limits = c(0,1), breaks = c(0,1), name = "SD", direction = -1) +
    scale_color_viridis(option = "rocket", limits = c(0,1), breaks = c(0,1), name = "SD", direction = -1) +
    theme_bw() +
    coord_quickmap() +
    theme(
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.margin = unit(c(-0.6,-1,-0.6,-1), 'cm'),
      legend.position = uc_legend_pos
    )

  prediction_plot_file <- paste0("Figures/Prevalence/",
                                 data_container$metadata$model,"_",
                                 data_container$metadata$type,"_",
                                 data_container$metadata$n,"_prevalence", ".png")
  png(prediction_plot_file, units = "cm", width = im_width, height = im_height, res = im_res)
  print(pred_plot)
  dev.off()

  uncertainty_plot_file <- paste0("Figures/Uncertainty/",
                                  data_container$metadata$model,"_",
                                  data_container$metadata$type,"_",
                                  data_container$metadata$n,"_uncertainty", ".png")
  png(uncertainty_plot_file, units = "cm", width = im_width, height = im_height, res = im_res)
  print(uncertainty_plot)
  dev.off()
}

lapply(All_list, make_plots)



