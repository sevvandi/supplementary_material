# Produce maps from the INLA results using the noise added simulated data
# Plot the posterior means of range and intercept too
library(readr)
library(dplyr)
library(ggplot2)
library(raster)
library(rmapshaper)
library(RColorBrewer)
library(patchwork)
library(viridis)


# Prepare data
#------------------------------------------------------------------------
# Set the working directory to the INLA_Noise folder
setwd()

# Leave out sd = 1 due to convergence issues

sds <- c(dat_0 = 0, dat_0.2 = 0.2, dat_0.4 = 0.4, dat_0.6 = 0.6, dat_0.8 = 0.8,
         #dat_1 = 1,
         dat_1.2 = 1.2, dat_1.4 = 1.4, dat_1.7 = 1.7, dat_2 = 2)
load_noisy_inputs <- function(sd){
  filename <- sprintf("Data_Inputs/noise_added_data_sd_%s.rds",sd)
  data <- readRDS(filename)
  return(data)
}

noisy_inputs <- lapply(sds, load_noisy_inputs)

load_noisy_data <- function(sd){
  filename <- sprintf("Data_Outputs/INLA_simulated_914_noise_added_%s.rds", sd)
  data <- readRDS(filename)
  return(data)
}

INLA_data <- lapply(sds,  load_noisy_data)

# Compute the uncertainties and add model name to data
add_properties <- function(dat, model){
  dat$metadata$model <- model
  dat$data$iqr <- dat$data$iqr <- dat$data$'0.75quant' -  dat$data$'0.25quant'
  return(dat)
}

INLA_data <- lapply(INLA_data, {function(x) add_properties(x, "INLA")})



# Plot prediction maps
#------------------------------------------------------------------------
Africa_border <- readRDS("Data_Inputs/Africa_border.rds")

im_width <- 12
im_height <- 11
im_res <- 300


make_plots <- function(data_container){
  uc_type <- "IQR"
  uc_colour <- "mako"
  data_container$data$uc <- data_container$data$iqr
  if(data_container$metadata$sd == 0){
    legend_pos <- c(0.22,0.27)
  }
  else{
    legend_pos <- "none"
  }
  pred_plot <-  ggplot(data = data_container$data, mapping = aes(x = x, y = y, fill = pred, color = pred)) +
    geom_tile() +
    geom_polygon(data = Africa_border, aes(x = long, y = lat, group = group), colour = "#00000015", size = 0.1, fill = NA)+
    scale_fill_distiller(palette = "Spectral", limits = c(0,1), name = "Prevalence") +
    scale_color_distiller(palette = "Spectral", limits = c(0,1), name = "Prevalence") +
    theme_bw() +
    coord_quickmap()+
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


  prediction_plot_file <- paste0("Figures/INLA_noise/",data_container$metadata$model,"_", data_container$metadata$type,"_sd_", data_container$metadata$sd,"_prevalence", ".png")
  png(prediction_plot_file, units = "cm", width = im_width, height = im_height, res = im_res)
  print(pred_plot)
  dev.off()
}

# SD = 0, 0.4, 1.2 to  give a good picture of what is happening
plot_data <- INLA_data[c("dat_0", "dat_0.4", "dat_1.2")]

lapply(plot_data, make_plots)


# Plot the hyperparameter means and times
# -------------------------------------------------------------------------
library(INLA)
# Extract the intercept means, range mean, and time
get_values <- function(data_container){
  sd <- data_container$metadata$sd
  time <- data_container$time$total
  intercept_mean <- data_container$intercept_summary$mean
  int_q0.25 <- data_container$intercept_summary$'0.25quant'
  int_q0.75 <- data_container$intercept_summary$'0.75quant'
  range_summary <- inla.zmarginal(data_container$hyperparam_marginals$marginals.range.nominal[[1]])
  range_mean <- range_summary$mean
  print(range_mean)
  range_q0.25 <- range_summary$'quant0.25'
  range_q0.75 <- range_summary$'quant0.75'
  variance_summary <- inla.zmarginal(data_container$hyperparam_marginals$marginals.variance.nominal[[1]])
  variance_mean <- variance_summary$mean
  variance_q0.25 <- variance_summary$quant0.25
  variance_q0.75 <- variance_summary$quant0.75
  data_row <- data.frame(sd = sd, time = time, intercept_mean = intercept_mean,
                          int_q0.25 = int_q0.25,
                          int_q0.75 = int_q0.75,
                          range_q0.75 = range_q0.75,
                          range_q0.25 = range_q0.25,
                          range_mean = range_mean,
                          variance_mean = variance_mean,
                          variance_q0.25 = variance_q0.25,
                          variance_q0.75 = variance_q0.75)
  return(data_row)
}

data_row_list <- lapply(INLA_data, get_values)
data_df <- do.call(rbind.data.frame, data_row_list)

# Plotting
p_intercept <- ggplot(data = data_df, mapping = aes(x = sd, y = intercept_mean)) +
  geom_point(colour = "#E68613") +
  geom_errorbar(mapping = aes(ymin = int_q0.25, ymax = int_q0.75), colour = "#E68613") +
  ggtitle("Intercept") +
  ylab("Intercept") +
  xlab("SD")

p_range <- ggplot(data = data_df, mapping = aes(x = sd, y = range_mean)) +
  geom_point(colour = "#00A9FF") +
  geom_errorbar(mapping = aes( ymin = range_q0.25, ymax = range_q0.75), colour = "#00A9FF") +
  ggtitle("Nominal Range") +
  ylab("Range") +
  xlab("SD")

p_variance <- ggplot(data = data_df, mapping = aes(x = sd, y = variance_mean)) +
  geom_point(colour = "#F8766D") +
  geom_errorbar(mapping = aes( ymin = variance_q0.25, ymax = variance_q0.75), colour = "#F8766D") +
  ggtitle("Nominal Variance") +
  ylab("Variance") +
  xlab("SD")

p_time <- ggplot(data = data_df, mapping = aes(x = sd, y = time)) +
  geom_point(colour = "#7CAE00") +
  ggtitle("Time") +
  ylab("Time (mins)") +
  xlab("SD") +
  ylim(c(0,90))

# Use patchwork to arrange the plots
(p_intercept|p_range)/(p_variance|p_time)
