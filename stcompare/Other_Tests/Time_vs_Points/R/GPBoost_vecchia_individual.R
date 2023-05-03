# Goal: Run GPBoost with data sampled from MAP predictions
# Take training data file as a command line arg


# command line arguments: 1: dataset path to individual data file
command_args <- commandArgs(trailingOnly = TRUE) # skip automatically included ones
input_file <- command_args[1]

data_container <- readRDS(input_file)
#structure list, $data (dataframe), $metadata (list)



# Time each step
t_begin <- Sys.time()

library(gpboost)
library(readr)
library(rgdal)
library(sp)
library(raster)
library(dplyr)


# Set up the training data and coordinates
#------------------------------------------------------------------------

# Load in the grid of points over Africa to do prediction at
load("Data_Inputs/Africa_grids_mh.RData")
# Contains medium-high resolution (cellsize = 0.15) grids

# Each grid is a spatialPoints object with WGS84 CRS
grid <- grid_cut_mh




print("start of run_GPB")


run_GPBoost <- function(data_container){
  dat <- data_container$data

  dat$x <- dat$longitude
  dat$y <- dat$latitude

  # Use the binomial simulated prevalence
  dat$pr <- dat$simulated_pr
  coords <- as.matrix(dat[ ,c('x','y')])

  # Set up and train the model
  #------------------------------------------------------------------------
  print("set up model")
  # Vecchia controls
  mv <- 30 #num neighbours for fitting
  mvp <- 150 #num neighbours for prediction

  gp_model <- GPModel(gp_coords = coords,
                      cov_function = "exponential", vecchia_approx = TRUE, num_neighbors = mv)
  intercept <- matrix(rep(1, dim(dat)[1]), ncol=1)
  labels <- dat$pr

  print("train model")
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
  print("prediction")
  X_test <- matrix(rep(1, dim(grid@coords)[1]), ncol=1)

  # For storing upper and lower uncertainties
  lower_and_upper <- matrix(0, ncol = 2, nrow = dim(grid@coords)[1])

  t_predict_begin <- Sys.time()
  pred <- predict(bst, data = X_test, gp_coords_pred = grid@coords,
                  predict_var = FALSE, pred_latent = TRUE,
                  num_neighbors_pred = mvp
                  ) # predict_var turned to false due to memory requirements using vecchia
  prev_pred <- pred$fixed_effect + pred$random_effect_mean
  var_pred<- pred$random_effect_cov
  lower_and_upper[, 1] <- prev_pred - sqrt(pred$random_effect_cov)
  lower_and_upper[, 2] <- prev_pred + sqrt(pred$random_effect_cov)
  t_predict_end <- Sys.time()

  # Save prediction data
  #------------------------------------------------------------------------
  # Connect the vector y_pred with the prediction locations
  GPBoost_prediction_data <- data.frame(grid@coords, prev_pred, var_pred, lower = lower_and_upper[,1], upper = lower_and_upper[,2])

  t_end <- Sys.time()

  t_tot <- t_end - t_begin
  t_train <- t_train_end - t_train_begin
  t_predict <- t_predict_end - t_predict_begin

  GPBoost_times <- data.frame(total = t_tot,
                              training = t_train,
                              prediction = t_predict)
  data_container$metadata$threads <- 1

  return(list(data = GPBoost_prediction_data, times = GPBoost_times, metadata = data_container$metadata))

}



result <- run_GPBoost(data_container)

output_file <- "Data_Outputs/GPBoost_simulated"

# Add metadata to file name
for (i in data_container$metadata){
  output_file <- paste0(output_file,"_",i)
}
output_file <- paste0(output_file, "vecchia.rds")

saveRDS(result, output_file)


