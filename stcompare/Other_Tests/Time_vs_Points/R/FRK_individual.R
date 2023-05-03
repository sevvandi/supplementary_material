
# Goal: Run FRK on data specified by command line argument

# This script should be run from the command line with a single command
# line argument giving the path to a chosen data file
# eg: Data_Inputs/uniform_1000.rds
command_args <- commandArgs(trailingOnly = TRUE) # skip automatically included ones
input_file <- command_args[1]
data_container <- readRDS(input_file)
#structure list, $data (dataframe), $metadata (list)

t_begin <- Sys.time()
library(sp)
library(dplyr)
library(FRK)
library(readr)
library(raster)
library(ggplot2)

opts_FRK$set("progress",FALSE)
# No parallelisation
opts_FRK$set("parallel",0L)

#Set up the data and coordinates
#------------------------------------------------------------------------
set.seed(1)
# Load the prediction points
load("Data_Inputs/Africa_grids_mh.RData")
grid <- grid_cut_mh

# Load the polygons of Africa map
load("Data_Inputs/Africa_map.RData")

dat <- container$data
dat$x <- dat$longitude
dat$y <- dat$latitude
zdf <- data.frame(x = dat$x, y = dat$y, positive = dat$positive, k_Z = dat$examined)

# change into an sp object
coordinates(zdf) = ~x+y

# Set up the FRK model: BAUs and Basis Functions
#------------------------------------------------------------------------
# Generate Basic Areal Units
t_BAUs_begin <- Sys.time()
GridBAUs1 <- auto_BAUs(manifold = plane(),
                       cellsize = c(0.15,0.15),
                       type = "grid",
                       data = grid,
                       convex=-0.05,
                       nonconvex_hull=FALSE)
GridBAUs1$fs <- 1 # fine-scale variation at BAU level
t_BAUs_end <- Sys.time()

t_basis_begin <- Sys.time()
# Basis functions
G <- auto_basis(manifold = plane(),
                data = zdf,
                nres = 2,
                type = "Gaussian",
                regular = 1)
t_basis_end <- Sys.time()

ztrain <- zdf
# The training data and BAUs require the same CRS, which should be WGS84
proj4string(ztrain) <- proj4string(grid)

# Construct the Spatial Random Effects model
f <- positive ~ 1
S <- SRE(f = f,
         data = list(ztrain),
         BAUs = GridBAUs1,
         basis = G,
         response = "binomial",
         link = "logit",
         K_type = "precision")

# Fit the SRE model
t_train_begin <- Sys.time()
S <- SRE.fit(S, # SRE model
             method = "TMB")
t_train_end <- Sys.time()

# Prediction
#------------------------------------------------------------------------
t_predict_begin <- Sys.time()
GridBAUs2 <- predict(S, obs_fs = FALSE)

# Compute the mean and standard deviation of the predictions
mu_sam <- apply(GridBAUs2$MC$prob_samples, 1, mean)
sd_sam <- apply(GridBAUs2$MC$prob_samples, 1, sd)

t_predict_end <- Sys.time()

# Predictions extend over Africa's borders as a rectangle.
# Cut out points outside the border
data_for_plot <- GridBAUs2$newdata
data_for_plot$mu_sam <- mu_sam
data_for_plot$sd_sam <- sd_sam
proj4string(data_for_plot) <- proj4string(grid)


# Remove points outside of Africa
t_mask_output_begin <- Sys.time()
FRK_prediction_data <- data_for_plot[Africa_map, ]
t_mask_output_end <- Sys.time()

# Save prediction data
#------------------------------------------------------------------------
FRK_prediction_data <- as.data.frame(FRK_prediction_data)
FRK_prediction_data$sd <- FRK_prediction_data$sd_sam
FRK_prediction_data$pred <- FRK_prediction_data$mu_sam

t_end <- Sys.time()
t_tot <- t_end - t_begin
t_train <- t_train_end - t_train_begin
t_predict <- t_predict_end - t_predict_begin
t_BAUs <- t_BAUs_end - t_BAUs_begin
t_basis <- t_basis_end - t_basis_begin
t_mask_output <- t_mask_output_end - t_mask_output_begin

FRK_times <- data.frame(total = t_tot,
                        BAUs = t_BAUs,
                        basis = t_basis,
                        training = t_train,
                        prediction = t_predict,
                        output_masking = t_mask_output)

result <- list(data = FRK_prediction_data, time = FRK_times, metadata = container$metadata, basis = G)
result$metadata$model <- "FRK"
result$metadata <- result$metadata[c("model", "n", "type")]

output_file <- "Data_Outputs/"

# Add metadata to file name
for (i in result$metadata){
  output_file <- paste0(output_file,"_",i)
}
output_file <- paste0(output_file, ".rds")

saveRDS(result, file = output_file)
