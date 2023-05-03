# Make plots for the GPBoost runs which had Vecchia approximations
# We had to turn of variance prediction due to high memory and time use
library(ggplot2)
library(dplyr)
library(patchwork)
library(viridis)
library(RColorBrewer)
library(raster)

# Set working directory to the "GPBoost_Vecchia" directory
setwd()

# Load the predictions
# -------------------------------------------------------------------------
# Trained on observations
GPBoost_obs <- readRDS("Data_Outputs/n868typeobservation_datamodelGPBoostveccVECCHIAmv300mvp150.rds")

All_list <- list(GPBoost_obs)

# Restrict values to [0,1]
clamp_values <- function(data_container){
  clamppred <- clamp(data_container$data$pred, 0,1)
  data_container$data$pred <- clamppred
  return(data_container)
}

All_list <- lapply(All_list, clamp_values)



# Mask the outputs
#------------------------------------------------------------------------
# Load the map raster
map_raster <- readRDS("Data_Inputs/Rect_pfpr_2_10_2009_surface.rds")
# Convert to spatial pixels
map_raster_sp <- as(map_raster, "SpatialPixelsDataFrame")

# Convert all the data to spatial points
to_sp <- function(data_container){
  coordinates(data_container$data) <- ~x+y
  proj4string(data_container$data) <- "+proj=longlat +datum=WGS84 +no_defs"
  return(data_container)
}
All_list <- lapply(All_list, to_sp)

mask <- function(data_container){
  masked_data <- data_container$data[map_raster_sp,]
  data_container$data<- masked_data
  return(data_container)
}

All_list_masked <- lapply(All_list, mask)


# Make plots for each model/data combination
#------------------------------------------------------------------------
Africa_border <- readRDS("Data_Inputs/Africa_border.rds")

im_width <- 12
im_height <- 11
im_res <- 300

background_fill <- "#f5f5f5"
border_colour <- "#a1a1a1"


make_plots <- function(data_container){
  data = as.data.frame(data_container$data)
  legend_pos <- c(0.22, 0.27)
  # Add prevalence legend only for the first plot
  pred_plot <- ggplot(data = data, mapping = aes(x = x, y = y, fill = pred, color = pred)) +
    geom_polygon(data = Africa_border, mapping = aes(x = long, y = lat, group = group), color = border_colour, fill = background_fill, size = 0.2 )+
    geom_tile() +
    scale_fill_distiller(palette = "Spectral", limits = c(0,1), name = "Prevalence") +
    scale_color_distiller(palette = "Spectral", limits = c(0,1), name = "Prevalence") +
    theme_bw() +
    ggtitle(paste0("Time: ", as.numeric(data_container$times$total), " ", units(data_container$times$total))) +
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
      #plot.margin = unit(c(-0.6,-1,-0.6,-1), 'cm')
    )

  pred_plot_detail <- ggplot(data = data, mapping = aes(x = x, y = y, fill = pred, color = pred)) +
    geom_polygon(data = Africa_border, mapping = aes(x = long, y = lat, group = group), color = border_colour, fill = background_fill, size = 0.2 )+
    geom_tile() +
    lims(x = c(9,43), y = c(-35,-5)) +
    scale_fill_distiller(palette = "Spectral", limits = c(0,1), name = "Prevalence") +
    scale_color_distiller(palette = "Spectral", limits = c(0,1), name = "Prevalence") +
    theme_bw() +
    ggtitle(paste0("Time: ", as.numeric(data_container$times$total), " ", units(data_container$times$total))) +
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
      legend.position = "none",
      #plot.margin = unit(c(-0.6,-1,-0.6,-1), 'cm')
    )


  metadata_tail <- ""
  for (i in 1:length(data_container$metadata)){
    metadata_tail <- paste0(metadata_tail, names(data_container$metadata[i]), data_container$metadata[[i]])
  }


  prediction_plot_file <- paste0("Figures/For_Paper/",
                                 metadata_tail, ".png")
  detail_plot_file <-  paste0("Figures/For_Paper/",
                              metadata_tail, "_detail.png")
  png(prediction_plot_file, units = "cm", width = im_width, height = im_height, res = im_res)
  print(pred_plot)
  dev.off()
  png(detail_plot_file, units = "cm", width = im_width, height = im_height, res = im_res)
  print(pred_plot_detail)
  dev.off()

}

lapply(All_list_masked, make_plots)


