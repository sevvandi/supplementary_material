# Binomial simulation from MAP surface with uniformly selected points
# Use to test time vs the number of points
library(raster)
library(malariaAtlas)
library(ggplot2)
library(readr)
library(dplyr)
library(RColorBrewer)
library(sp)
library(malariaAtlas)

# Set working directory to the "Time_vs_Points" folder
setwd()

# Create a large rectangle to select MAP raster points from
coords <- rbind(c(-30,-40),c(55,-40),c(55,40),c(-30,40),c(-30,-40))
square <- Polygon(coords, hole = FALSE)
square_pols <- Polygons(list(square), 1)
sp_square <- SpatialPolygons(list(square_pols ))

Africa_PfPR_2_10_2009 <- getRaster(surface = "Plasmodium falciparum PR2-10", shp = sp_square, year = 2009)

# Convert from RasterLayer to data frame
Africa_PfPR2_10_df <- as.MAPraster(Africa_pfPR_2_10_2009)
names(Africa_PfPR2_10_df)[names(Africa_PfPR2_10_df) == 'z'] <- 'map_pr'



# Get 10000 rows at random
set.seed(1)
uniform_10000 <- Africa_PfPR2_10_df[sample(c(1:nrow(Africa_PfPR2_10_df)), size = 10000, replace = FALSE),]

# Approximately average n_tested from surveys
n_tested <- 90

uniform_10000$examined <- n_tested

# number of positive is a sample from binomial distribution with size = n_tested (constant)
# and probability given by the pr value
uniform_10000$positive <-  sapply(uniform_10000$map_pr, {function(x) rbinom(1,size = n_tested,prob = x)})

# GPBoost and SpRF just take a single prevalence value, ie: no binomial response.
# compute a 'simulated' pr value from the results of the binomial sampling for n_positive
# This will add noise to the inputs for these models, so that they are not
# using a smoother surface than INLA and FRK which use examined and positive

uniform_10000$simulated_pr <- uniform_10000$positive / uniform_10000$examined

names(uniform_10000)[names(uniform_10000) == "x"] <- "longitude"
names(uniform_10000)[names(uniform_10000) == "y"] <- "latitude"

# Make subsets (uniformly selected)
uniform_5000 <- uniform_10000[sample(c(1:nrow(uniform_10000)), size = 5000, replace = FALSE),]
uniform_4000 <- uniform_10000[sample(c(1:nrow(uniform_10000)), size = 4000, replace = FALSE),]
uniform_3000 <- uniform_10000[sample(c(1:nrow(uniform_10000)), size = 3000, replace = FALSE),]
uniform_2000 <- uniform_10000[sample(c(1:nrow(uniform_10000)), size = 2000, replace = FALSE),]
uniform_1000 <- uniform_10000[sample(c(1:nrow(uniform_10000)), size = 1000, replace = FALSE),]

# Save data
make_uniform_container <- function(data){
  container <- list()
  container$data <- data
  container$metadata <- list(n = nrow(data), type = "uniform")
  saveRDS(container, file = paste0("Data_Inputs/uniform_",nrow(data),".rds"))
}

uniform_list <- list(uniform_10000, uniform_5000, uniform_4000, uniform_3000, uniform_2000,
                     uniform_1000)

lapply(uniform_list, make_uniform_container)


