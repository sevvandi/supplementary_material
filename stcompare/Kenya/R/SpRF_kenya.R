# Train a SpRF model on the 2009 Kenya malaria prevalence data and produce
# a map of predictions

t_begin <- Sys.time()

library(ranger)
library(GSIF)
library(rgdal)
library(raster)
library(readr)
library(sp)
library(dplyr)
library(sf)

# Set the working directory to the 'Kenya' directory
setwd()



# Set up training data and prediction locations
# ----------------------------------------------------------------------
dat <- readRDS("Data_Inputs/Kenya_pf_pr_2009.rds")
dat <- dat %>% mutate(x = longitude, y = latitude)
class(dat) <- "data.frame"
# Turn dat into a SpatialPointsDataFrame
coordinates(dat) = ~x+y

set.seed(1)

# Make a grid of points to do prediction at
kenya <- getData('GADM', country = 'KENYA', level = 1)
grid <- makegrid(kenya, cellsize = 0.1)
colnames(grid) <- c("x", "y")

# Add a column to the grid made up of random normally distributed values
grid$falsevals <- rnorm(dim(grid)[1])

# Turn grid into a SpatialPointsDataFrame
coordinates(grid) = ~x+y
class(grid)

# The buffer.dist function requires the grid to be a SpatialPixelsDataFrame
gridded(grid) = TRUE


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
ov.pr <- over(dat["pr"], grid_dist0)

# Attach the observed prevalences
rm.pr <- cbind(dat@data["pr"], ov.pr)
train <- rm.pr

# Set up the testing set, contains a row for each prediction point, containing
# the distances from this point to each observation point
test <- grid_dist0@data

# Train the model
t_train_begin <- Sys.time()
mod <- ranger(fm0, train, quantreg=TRUE, num.trees=500, seed=1, num.threads = 1)
t_train_end <- Sys.time()
quantiles <- c(0.025, 0.25, 0.5, 0.75, 0.975)


# Prediction
t_predict_begin <- Sys.time()
# Quantiles
predquant <- predict(mod, test, type="quantiles", quantiles=quantiles)$predictions
t_predict_end <- Sys.time()
colnames(predquant) <- c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
# Mean of response
predmean <- predict(mod, test, type = "response", num.threads = 1)$predictions

# Attach the predicted mean and quantiles to the prediction points
pred_grid <- grid
pred_grid$q0.025 <- predquant[ ,1]
pred_grid$q0.25 <- predquant[ ,2]
pred_grid$q0.5 <- predquant[ ,3]
pred_grid$q0.75 <- predquant[ ,4]
pred_grid$q0.975 <- predquant[ ,5]
pred_grid$mean <- predmean

# Remove points outside of Kenya
t_mask_output_begin <- Sys.time()
kya <- kenya[kenya$NAME_0 == "Kenya", ]
proj4string(pred_grid) <- proj4string(kenya)
pred_grid_kenya_only <- pred_grid[kya, ]
t_mask_output_end <- Sys.time()

# Compute the IQR and approximate SD
pred_grid_kenya_only$iqr <- pred_grid_kenya_only$q0.75 - pred_grid_kenya_only$q0.25
pred_grid_kenya_only$sd <- pred_grid_kenya_only$iqr/1.34898

# Save prediction data
#------------------------------------------------------------------------
SpRF_prediction_data  <- as.data.frame(pred_grid_kenya_only)
# Use median as prediction
SpRF_prediction_data$pred <- SpRF_prediction_data$q0.5

t_end <- Sys.time()

t_tot <- t_end - t_begin
t_train <- t_train_end - t_train_begin
t_predict <- t_predict_end - t_predict_begin
t_distance <- t_distance_end - t_distance_begin

SpRF_times <- data.frame(total = t_tot,
                         distance = t_distance,
                         training = t_train,
                         prediction = t_predict)

SpRF_metadata <- list(model = "SpRF")

result <- list(data = SpRF_prediction_data, time = SpRF_times, metadata = SpRF_metadata)

saveRDS(result, file = "Prediction_Maps/Data_Outputs/SpRF_prediction_data.rds")





