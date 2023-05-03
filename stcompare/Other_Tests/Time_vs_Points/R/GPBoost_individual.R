# Goal: Run GPBoost on data specified by command line argument

# This script should be run from the command line with a single command
# line argument giving the path to a chosen data file
# eg: Data_Inputs/uniform_1000.rds
command_args <- commandArgs(trailingOnly = TRUE)
input_file <- command_args[1]
data_container <- readRDS(input_file)
#structure list, $data (dataframe), $metadata (list)

t_begin <- Sys.time()
library(gpboost)
library(readr)
library(rgdal)
library(sp)
library(raster)
library(dplyr)

# Set up the training data and coordinates
#------------------------------------------------------------------------
# Load prediction points
load("Data_Inputs/Africa_grids_mh.RData")
grid <- grid_cut_mh

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

coords <- as.matrix(dat[ ,c('x','y')])

# Set up and train the model
#------------------------------------------------------------------------
gp_model <- GPModel(gp_coords = coords,
                    cov_function = "exponential")
intercept <- matrix(rep(1, dim(dat)[1]), ncol=1)
labels <- dat$pr

# Train the model
t_train_begin <- Sys.time()
bst <- gpboost(data = intercept,
               label = labels,
               gp_model = gp_model,
               nrounds = 247,
               learning_rate = 0.01,
               max_depth = 6,
               min_data_in_leaf = 5,
               num_leaves = 2^10,
               objective = "regression_l2",
               verbose = 1,
               num_threads = 1)
t_train_end <- Sys.time()

# Prediction
#------------------------------------------------------------------------
X_test <- matrix(rep(1, dim(grid@coords)[1]), ncol=1)
# For storing upper and lower uncertainties
lower_and_upper <- matrix(0, ncol = 2, nrow = dim(grid@coords)[1])

t_predict_begin <- Sys.time()
pred <- predict(bst, data = X_test, gp_coords_pred = grid@coords,
                predict_var = TRUE, pred_latent = TRUE)
prev_pred <- pred$fixed_effect + pred$random_effect_mean
var_pred<- pred$random_effect_cov
lower_and_upper[, 1] <- prev_pred - sqrt(pred$random_effect_cov)
lower_and_upper[, 2] <- prev_pred + sqrt(pred$random_effect_cov)
t_predict_end <- Sys.time()

# Save prediction data
#------------------------------------------------------------------------
# Connect the vector y_pred with the prediction locations
GPBoost_prediction_data <- data.frame(grid@coords, prev_pred, var_pred, lower = lower_and_upper[,1], upper = lower_and_upper[,2])
GPBoost_prediction_data$sd <- sqrt(GPBoost_prediction_data$var_pred)
GPBoost_prediction_data$pred <- GPBoost_prediction_data$prev_pred


t_end <- Sys.time()

t_tot <- t_end - t_begin
t_train <- t_train_end - t_train_begin
t_predict <- t_predict_end - t_predict_begin

GPBoost_times <- data.frame(total = t_tot,
                            training = t_train,
                            prediction = t_predict)

result <- list(data = GPBoost_prediction_data, times = GPBoost_times, metadata = data_container$metadata)
result$metadata$model <- "GPBoost"
result$metadata <- result$metadata[c("model", "n", "type")]

output_file <- "Data_Outputs/"

# Add metadata to file name
for (i in result$metadata){
  output_file <- paste0(output_file,"_",i)
}
output_file <- paste0(output_file, ".rds")

saveRDS(result, output_file)


