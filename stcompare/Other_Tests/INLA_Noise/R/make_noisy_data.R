# Start with the binomial simulated data at the observation locations
# Add increasing amounts of noise
# Goal is to test INLA as the noise increases

library(dplyr)
library(ggplot2)

# Set the working directory to the "INLA_Noise" directory
setwd()


# Begin
# -------------------------------------------------------------------------
set.seed(1)

# Start with the MAP simulated data that had no added noise
map_sim_dat <- readRDS("../../Africa/Data_Inputs/binomial_observation_locations_840.rds")

# map_pr contains the prevalence sampled from the map raster
# positive and simulated_pr come from the binomial sampling
# only use simulated_pr for the sd = 0 case

noised_data_proper <- map_sim_dat

logit_func <- function(p){
  return(log(p/(1-p)))
}
inv_logit <- function(x){
  return(exp(x)/(1+exp(x)))
}

sds <- c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.7, 2)


# Steps for adding noise:
# 1. Logit transform the prevalence sampled from map raster (clip it so it's not
#    0 or 1 first)
# 2. Add Gaussian noise of varying standard deviations
# 3. Inverse logit transform it
# 4. Generate binomial samples using this new prevalence
# 5. Use the npositive and nexamined from these samples for INLA FRK
# 6. Use npositive/nexamined from these samples for SpRF GPBoost
add_noise_proper <- function(sd, data_container) {
  dat <- data_container$data

  # Don't change anything if sd = 0, ie: don't resample the prevalences
  if(sd == 0){
    dat$noisy_positive <- dat$positive
    dat$noisy_simulated_pr <- dat$simulated_pr
  }
  else{
    # clip the prevalence pre logit
    # Start with the map sampled prevalence, not the binomial simulated one
    dat$clipped_pr <- pmax(pmin(dat$map_pr, 0.999999),0.000001)
    dat$logit_pr <- logit_func(dat$clipped_pr)
    dat$noised_logit_pr <- dat$logit_pr + rnorm(nrow(dat), mean = 0, sd = sd)
    dat$noised_pr <- inv_logit(dat$noised_logit_pr)

    # Generate binomial samples using this prevalence
    # For compatibility with the INLA script, name it noisy_positive
    dat$noisy_positive <- mapply({function(x,y) rbinom(1, size = x, prob = y)},
                                  dat$examined, dat$noised_pr)
    # Calculate the corresponding prevalence from samples
    dat$noisy_simulated_pr <- dat$noisy_positive / dat$examined
  }


  dat <- dat[,c("longitude", "latitude", "map_pr", "examined", "point_ID",
                "noisy_positive", "noisy_simulated_pr")]

  data_container$data <- dat
  data_container$metadata$type <- "noise_added"
  data_container$metadata$sd <- sd

  return(data_container)
}



noised_data_proper_list <- lapply(sds, {function(x) add_noise_proper(x, noised_data_proper)})




# Plot the noisy data
# -------------------------------------------------------------------------
Africa_border <- readRDS("../../Africa/Data_Inputs/Africa_border.rds")

obs_plot_theme <- theme(
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  #legend.position = "none"
)

im_width <- 15
im_height <- 10
im_res <- 300

plot_points_proper <- function(points_container){
  plot <- ggplot() +
    geom_polygon(data = Africa_border, mapping = aes(x = long, y = lat, group = group), color = "#878787", fill = "#f2f2f2", size = 0.2 )+
    geom_point(data = points_container$data, mapping = aes(x = longitude, y = latitude, color = noisy_simulated_pr), size = 0.5) +
    scale_color_distiller(palette = "Spectral", limits = c(0,1), name = "Prevalence") +
    coord_quickmap() +
    ggtitle(paste0("Noise added points, sd = ", points_container$metadata$sd)) +
    obs_plot_theme
  png(filename = paste0("figures/points/","noisy_data_sd",points_container$metadata$sd,"_proper.png"), units = "cm",  width = im_width, height = im_height, res = im_res)
  print(plot)
  dev.off()
}

lapply(noised_data_proper_list, plot_points_proper)



# Save the data
# -------------------------------------------------------------------------
for(data_container in noised_data_proper_list){
  filename <- paste0("Data_Inputs/noise_added_data_sd_", data_container$metadata$sd, ".rds")
  saveRDS(data_container, filename)
}









