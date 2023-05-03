# Goal: Run SpRF on data specified by command line argument

# This script should be run from the command line with a single command
# line argument giving the path to a chosen data file
# eg: Data_Inputs/uniform_1000.rds
command_args <- commandArgs(trailingOnly = TRUE)
input_file <- command_args[1]
data_container <- readRDS(input_file)
#structure list, $data (dataframe), $metadata (list)


t_begin <- Sys.time()

library(ranger)
library(GSIF)
library(rgdal)
library(raster)
library(readr)
library(sp)
library(dplyr)
library(sf)

# Set up training data and prediction locations
# ----------------------------------------------------------------------
set.seed(1)
# Load the grid of points over Africa to do prediction at
load("Data_Inputs/Africa_grids_mh.RData")
grid <- grid_whole_mh

# Add a column to the grid made up of random normally distributed values
# Adds the column to the data slot, must be same length as number of points
grid$falsevals <- rnorm(dim(grid@coords)[1])
# buffer.dist requires a SpatialPixelsDataFrame
gridded(grid) = TRUE

dat <- data_container$data
dat$x <- dat$longitude
dat$y <- dat$latitude

# Use the correct prevalence based on type of data
if(data_container$metadata$type == "observation_data"){
  # Do nothing, prevalence column already named "pr"
} else if(data_container$metadata$type == "binomial_observation_locations" | data_container$metadata$type == "binomial_uniform"){
  # Use the simulated prevalence
  dat$pr <- dat$simulated_pr
}


coordinates(dat) = ~x+y

# Set up Spatial Random Forest model
# ----------------------------------------------------------------------

# Compute distances between each observation point and each prediction point
t_distance_begin <- Sys.time()
grid_dist0 <- GSIF::buffer.dist(dat["pr"], grid[1], as.factor(1:nrow(dat)))
t_distance_end <- Sys.time()

# Set up the formula for the model
dn0 <- paste(names(grid_dist0), collapse="+")
fm0 <- as.formula(paste("pr ~ ", dn0))

# Extract approximate distances between each pair of observation locations
proj4string(dat) <- proj4string(grid_dist0)
ov.pr <- over(dat["pr"], grid_dist0)

# Attach the observed prevalences
rm.pr <- cbind(dat@data["pr"], ov.pr)
train <- rm.pr

# Set up the test set, contains a row for each prediction point, containing
# the distances from this point to each observation point
test <- grid_dist0@data

#train the model
t_train_begin <- Sys.time()
mod <- ranger(fm0, train, quantreg=TRUE, num.trees=500, seed=1, num.threads = 1)
t_train_end <- Sys.time()
quantiles <- c(0.025, 0.25, 0.5, 0.75, 0.975)

# Predictions
# ----------------------------------------------------------------------
t_predict_begin <- Sys.time()
predmod <- predict(mod, test, type="quantiles", quantiles=quantiles)$predictions
t_predict_end <- Sys.time()
colnames(predmod) <- c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")

# Attach the predicted prevalence and quantiles to the prediction points
pred_grid <- grid
pred_grid$q0.025 <- predmod[ ,1]
pred_grid$q0.25 <- predmod[ ,2]
pred_grid$q0.5 <- predmod[ ,3]
pred_grid$q0.75 <- predmod[ ,4]
pred_grid$q0.975 <- predmod[ ,5]

# Remove points outside of Africa
t_mask_output_begin <- Sys.time()
pred_grid_masked <- pred_grid[grid_cut_mh, ]
t_mask_output_end <- Sys.time()

# Save prediction data
#------------------------------------------------------------------------
SpRF_prediction_data  <- as.data.frame(pred_grid_masked)
SpRF_prediction_data$iqr <- SpRF_prediction_data$q0.75 - SpRF_prediction_data$q0.25
SpRF_prediction_data$pred <- SpRF_prediction_data$q0.5

t_end <- Sys.time()

t_tot <- t_end - t_begin
t_train <- t_train_end - t_train_begin
t_predict <- t_predict_end - t_predict_begin
t_distance <- t_distance_end - t_distance_begin
t_mask_output <- t_mask_output_end - t_mask_output_begin

SpRF_times <- data.frame(total = t_tot,
                         distance = t_distance,
                         training = t_train,
                         prediction = t_predict,
                         output_masking = t_mask_output)

result <- list(data = SpRF_prediction_data, times = SpRF_times, metadata = data_container$metadata)
result$metadata$model <- "SpRF"
result$metadata <- result$metadata[c("model", "n", "type")]

output_file <- "Data_Outputs/"
file_tail <- do.call(paste, c(result$metadata, sep = "_"))
output_file <- paste0("Data_Outputs/", file_tail, ".rds")

saveRDS(result, output_file)


