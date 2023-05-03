# Train a GPBoost model on the 2009 Kenya malaria prevalence data and produce
# a map of predictions
# For time comparisons, don't use parallelisation

t_begin <- Sys.time()

library(gpboost)
library(dplyr)
library(readr)
library(rgdal)
library(sp)
library(raster)
# Set the working directory to the "Kenya" directory
setwd()


# Set up the training data and coordinates
#------------------------------------------------------------------------
dat <- readRDS("Data_Inputs/Kenya_pf_pr_2009.rds")
dat <- dat %>% mutate(x = longitude, y = latitude)

# GPBoost requires the coordinates as a matrix
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
               verbose = 0,
               num_threads = 1) # No parallelisation
t_train_end <- Sys.time()

# Prediction
#------------------------------------------------------------------------

# Generate a grid of points over Kenya to do prediction at
kenya <- getData('GADM', country = 'KENYA', level = 1)
grid <- makegrid(kenya, cellsize = 0.1)
colnames(grid) <- c("x", "y")

# Turn grid into a SpatialPointsDataFrame, with locations given by x and y
coordinates(grid) = ~x+y
proj4string(grid) <- proj4string(kenya) # Give the grid the WGS84 CRS

# Keep only the points which lie within Kenya
kya <- kenya[kenya$NAME_0 == "Kenya", ]
proj4string(grid) <- proj4string(kenya)
grid <- grid[kya, ]


X_test <- matrix(rep(1, dim(grid@coords)[1]), ncol=1)

# For storing the lower and upper uncertainties
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

# Connect the prediction data with the locations
GPB_prediction_data <- data.frame(grid@coords, prev_pred, var_pred, lower = lower_and_upper[,1], upper = lower_and_upper[,2])

t_end <- Sys.time()

t_tot <- t_end - t_begin
t_train <- t_train_end - t_train_begin
t_predict <- t_predict_end - t_predict_begin

GPB_times <- data.frame(total = t_tot,
                        training = t_train,
                        prediction = t_predict)

GPB_metadata <- list(model = "GPB")

result <- list(data = GPB_prediction_data, time = GPB_times, metadata = GPB_metadata)

saveRDS(result, file = "Prediction_Maps/Data_Outputs/GPBoost_prediction_data.rds")
