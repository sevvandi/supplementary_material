# Train an FRK model on the 2009 Kenya malaria prevalence data and produce
# a map of predictions

t_begin <- Sys.time()

library(sp)
library(dplyr)
library(FRK)
library(readr)
library(raster)
# Set the working directory to the "Kenya" directory
setwd()

opts_FRK$set("progress",FALSE) # No progress bars
opts_FRK$set("parallel",0L) # No parallelisation

#Set up the data and coordinates
#------------------------------------------------------------------------
set.seed(1)

# Read the Kenya prevalence data
dat <- readRDS("Data_Inputs/Kenya_pf_pr_2009.rds")
dat <- dat %>% mutate(x = longitude, y = latitude)
zdf <- data.frame(x = dat$x, y = dat$y, positive = dat$positive, k_Z = dat$examined)

# Change into an sp object. FRK requires data to be a spatial points or
# polygons dataframe
coordinates(zdf) = ~x+y

# Generate a grid of points over Kenya to do prediction at
kenya <- getData('GADM', country = 'KENYA', level = 1)
grid <- makegrid(kenya, cellsize = 0.1)
colnames(grid) <- c("x", "y")
# Convert into a SpatialPoints object
coordinates(grid) <- ~x + y

# Give the grid the same CRS as the Kenya polygon (WGS84)
proj4string(grid) <- proj4string(kenya)

# Removing the points lying outside of Kenya
kya <- kenya[kenya$NAME_0 == "Kenya", ]
grid_cut <- grid[kya, ]


# Set up the FRK model: BAUs and Basis Functions
#------------------------------------------------------------------------

# Basic Areal Units
t_BAUs_begin <- Sys.time()
GridBAUs1 <- auto_BAUs(manifold = plane(), # 2D plane
                       cellsize = c(0.1,0.1), # BAU cellsize
                       type = "grid", # grid (not hex)
                       data = grid_cut, # data around which to create BAUs
                       convex=-0.05, # border buffer factor
                       nonconvex_hull=FALSE) # convex hull
GridBAUs1$fs <- 1 # fine-scale variation at BAU level
t_BAUs_end <- Sys.time()

# Basis functions
t_basis_begin <- Sys.time()
G <- auto_basis(manifold = plane(),
                data = zdf,
                nres = 2,
                type = "Gaussian",
                regular = 1)

# Plot the basis functions
show_basis(G) +
  coord_fixed() +
  xlab("Longitude") +
  ylab("Latitude")

t_basis_end <- Sys.time()


ztrain <- zdf


# The training data and BAUs require the same CRS, which should be WGS84
proj4string(ztrain) <- proj4string(kenya)

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
S <- SRE.fit(S, method = "TMB")
t_train_end <- Sys.time()


# Prediction
#------------------------------------------------------------------------

t_predict_begin <- Sys.time()
GridBAUs2 <- predict(S, obs_fs = FALSE)

# Compute the mean and standard deviation of the predictions
mu_sam <- apply(GridBAUs2$MC$prob_samples, 1, mean)
sd_sam <- apply(GridBAUs2$MC$prob_samples, 1, sd)

t_predict_end <- Sys.time()


# Predictions extend over Kenya's border as a rectangle. Make a copy and
# cut out points outside the border.
prediction_data_copy <- GridBAUs2$newdata
prediction_data_copy$mu_sam <- mu_sam
prediction_data_copy$sd_sam <- sd_sam
proj4string(prediction_data_copy) <- proj4string(kenya)

# Remove points outside of Kenya
FRK_prediction_data <- prediction_data_copy[kya, ]



# Save prediction data
#------------------------------------------------------------------------
FRK_prediction_data <- as.data.frame(FRK_prediction_data)

t_end <- Sys.time()
t_tot <- t_end - t_begin
t_train <- t_train_end - t_train_begin
t_predict <- t_predict_end - t_predict_begin
t_BAUs <- t_BAUs_end - t_BAUs_begin
t_basis <- t_basis_end - t_basis_begin

FRK_times <- data.frame(total = t_tot,
                        BAUs = t_BAUs,
                        basis = t_basis,
                        training = t_train,
                        prediction = t_predict)


FRK_metadata <- list(model = "FRK")

result <- list(data = FRK_prediction_data, time = FRK_times, metadata = FRK_metadata)

saveRDS(result, file = "Prediction_Maps/Data_Outputs/FRK_prediction_data.rds")


