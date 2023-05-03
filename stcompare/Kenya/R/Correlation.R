# Load the prediction data over Kenya and compute the correlation coefficients
# between data and predictions at corresponding locations

library(readr)
library(dplyr)
library(ggplot2)
library(raster)
library(rmapshaper)
library(RColorBrewer)
library(viridis)
library(patchwork) # Arranging plots
library(geosphere) # For distance matrix

# Load data
#------------------------------------------------------------------------
# Set working directory to "Kenya" directory
setwd()

# Load predictions
INLA_prediction_data <- readRDS("Prediction_Maps/Data_Outputs/INLA_prediction_data.rds")
SpRF_prediction_data <- readRDS("Prediction_Maps/Data_Outputs/SpRF_prediction_data.rds")
GPBoost_prediction_data <- readRDS("Prediction_Maps/Data_Outputs/GPBoost_prediction_data.rds")
FRK_prediction_data <- readRDS("Prediction_Maps/Data_Outputs/FRK_prediction_data.rds")


# Load observations
dat <- readRDS("Data_Inputs/Kenya_pf_pr_2009.rds")
dat <- dat %>% mutate(x = longitude, y = latitude)
dat <- dat[, c("x","y", "pr")]



# Get predictions at Data locations
#------------------------------------------------------------------------
# Compute distance matrices between prediction grid and observation points.
# Because FRK predicts at BAUs, its grid is different to INLA, GPBoost, and SpRF
# so we have to treat them separately
obs_points <- dat[,c("x","y")]
grid_points_IGS <- INLA_prediction_data$data[, c("x","y")] # GPBoost and SpRF have the same grid

dist_matrix_IGS <- distm(obs_points, grid_points_IGS)
# Row i is observation point i, collumn j is grid point j
# Distances in meters

# Get column indices which minimise distances
close_point_indices_IGS <- apply(dist_matrix_IGS, 1, which.min)
# jth element is index of grid point closest to observation j

close_points_IGS <- grid_points_IGS[close_point_indices_IGS, ]
close_points_IGS$id <- c(1:380)
obs_points$id <- c(1:380)


INLA_close <- INLA_prediction_data$data[close_point_indices_IGS,]
GPBoost_close <- GPBoost_prediction_data$data[close_point_indices_IGS,]
SpRF_close <- SpRF_prediction_data$data[close_point_indices_IGS,]

# Compare the observation locations with the closest prediction points
ggplot() +
  geom_point(data = obs_points, mapping = aes(x = x, y = y, colour = id)) +
  geom_point(data = close_points_IGS, mapping = aes(x = x, y = y, colour = id), shape = 15) +
  scale_color_distiller(palette = "Spectral")


# Repeat for FRK's grid
grid_points_FRK <- FRK_prediction_data$data[, c("x","y")]
dist_matrix_FRK <- distm(obs_points[,c("x","y")], grid_points_FRK)
close_point_indices_FRK <- apply(dist_matrix_FRK, 1, which.min)
close_points_FRK <- grid_points_FRK[close_point_indices_FRK, ]
FRK_close <- FRK_prediction_data$data[close_point_indices_FRK, ]

# Compute correlations
#------------------------------------------------------------------------
close_data <- data.frame(x = dat$x, y = dat$y, obs_pr = dat$pr,
                          INLA_pr = INLA_close$pred, GPBoost_pr = GPBoost_close$prev_pred,
                          SpRF_pr = SpRF_close$q0.5, FRK_pr = FRK_close$mu_sam)

INLA_cor <- cor(close_data$obs_pr, close_data$INLA_pr)
# 0.9091226
GPBoost_cor <- cor(close_data$obs_pr, close_data$GPBoost_pr)
# 0.8732895
SpRF_cor <- cor(close_data$obs_pr, close_data$SpRF_pr)
# 0.9117767
FRK_cor <- cor(close_data$obs_pr, close_data$FRK_pr)
# 0.9018516

