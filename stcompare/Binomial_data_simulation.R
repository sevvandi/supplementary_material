# Select points from the map surface and sample
# n_positive using binomial distribution

# Simulate prevalence at uniformly selected points, as well as the same locations
# as the observation data.
library(raster)
library(malariaAtlas)
library(ggplot2)
library(readr)
library(dplyr)
library(RColorBrewer)
library(sp)

# Set working directory to the Africa directory
setwd()

# Create a large rectangle to select MAP raster points from
coords <- rbind(c(-30,-40),c(55,-40),c(55,40),c(-30,40),c(-30,-40))
square <- Polygon(coords, hole = FALSE)
square_pols <- Polygons(list(square), 1)
sp_square <- SpatialPolygons(list(square_pols ))

Africa_PfPR_2_10_2009 <- getRaster(surface = "Plasmodium falciparum PR2-10", shp = sp_square, year = 2009)


# Convert from RasterLayer to data frame
Africa_PfPR2_10_df <- as.MAPraster(Africa_PfPR_2_10_2009)
names(Africa_PfPR2_10_df)[names(Africa_PfPR2_10_df) == 'z'] <- 'map_pr'

# Get 3000 rows at random
set.seed(1)
uniform_3000 <- Africa_PfPR2_10_df[sample(c(1:nrow(Africa_PfPR2_10_df)), size = 3000, replace = FALSE),]

Real_dat <- readRDS("Data_Inputs/Africa_pf_pr_2009.rds")


summary(Real_dat$examined)
# Set a global constant number of people examined
# Mean in the Africa data (without DHS) is about 86
n_tested <- 85
uniform_3000$examined <- n_tested

# Sample number positive from binomial distribution
uniform_3000$positive <-  sapply(uniform_3000$map_pr, {function(x) rbinom(1, size = n_tested, prob = x)})

# GPBoost and SpRF use a single prevalence value instead of positive and tested.
# Compute a 'simulated' pr value from the results of the binomial sampling for n_positive.
# This will add noise to the inputs for these models, so that they are not
# using a smoother surface than INLA and FRK which use examined and positive

uniform_3000$simulated_pr <- uniform_3000$positive / uniform_3000$examined

# Rename coordinates
names(uniform_3000)[names(uniform_3000) == "x"] <- "longitude"
names(uniform_3000)[names(uniform_3000) == "y"] <- "latitude"

# Make subsets (uniformly selected)
uniform_200 <- uniform_3000[sample(c(1:nrow(uniform_3000)), size = 200, replace = FALSE),]
uniform_1000 <- uniform_3000[sample(c(1:nrow(uniform_3000)), size = 1000, replace = FALSE),]

# Save the data
make_uniform_container <- function(data){
  container <- list()
  container$data <- data
  container$metadata <- list(n = nrow(data), type = "binomial_uniform")
  saveRDS(container, file = paste0("Data_Inputs/binomial_uniform_",nrow(data),".rds"))
}

uniform_list <- list(uniform_3000, uniform_200, uniform_1000)

lapply(uniform_list, make_uniform_container)

# Select points at the observation locations
#------------------------------------------------------------------------

set.seed(1)


# Convert to spatial pixels data frame
PfPR_for_loc_sampling <- Africa_PfPR2_10_df
coordinates(PfPR_for_loc_sampling) <- ~x+y
gridded(PfPR_for_loc_sampling) <- TRUE

# Convert observation locations to SpatialPoints
obs_locs <- Real_dat[c("longitude","latitude")]
class(obs_locs) <- "data.frame"
coordinates(obs_locs) <- ~longitude+latitude

# Extract MAP raster values at the observation locations
obs_locs_simulated_dat <- over(obs_locs, PfPR_for_loc_sampling)
obs_locs_simulated_dat$longitude <- obs_locs@coords[,"longitude"]
obs_locs_simulated_dat$latitude <- obs_locs@coords[,"latitude"]
class(obs_locs_simulated_dat) <- "data.frame"
obs_locs_simulated_dat <- obs_locs_simulated_dat[,c("longitude","latitude","map_pr")]

# In this case, our locations are real observation locations and have attached #examined
# use these values rather than constant n_tested = 90 to generate binomial samples
obs_locs_simulated_dat$examined <- Real_dat$examined

summary(obs_locs_simulated_dat$map_pr)
# There are 28 NA pr values, lying in holes in the map. Easiest to just remove
# them, as some are not close to any available map data.
obs_locs_simulated_dat <- filter(obs_locs_simulated_dat, !is.na(map_pr))

# Binomial sampling
binom_samp <- function(size,prob){
  return(rbinom(1, size, prob))
}

obs_locs_simulated_dat$positive <-  mapply(binom_samp, obs_locs_simulated_dat$examined, obs_locs_simulated_dat$map_pr)

# Add in the simulated prevalence values for GPBoost and SpRF
obs_locs_simulated_dat$simulated_pr <- obs_locs_simulated_dat$positive / obs_locs_simulated_dat$examined

# Save the simulated data
obs_points_container <- list()
obs_points_container$data <- obs_locs_simulated_dat
obs_points_container$metadata$n <- nrow(obs_locs_simulated_dat)
obs_points_container$metadata$type <- "binomial_observation_locations"

saveRDS(obs_points_container, file = paste0("Data_Inputs/binomial_observation_locations_",nrow(obs_locs_simulated_dat),".rds"))
