# Plot time taken vs number of points for the models
# Points simulated from MAP 2009 raster

library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Set working directory to the "Time_vs_Points" directory
setwd("")

# Load data
# -------------------------------------------------------------------------
points <- list(1000, 2000, 3000, 4000, 5000, 10000)
GPBoost_points <- list(1000, 2000, 3000, 4000)


load_data <- function(points, model){
  filename <- sprintf("Data_Outputs/%s_simulated_%s_uniform.rds", model, points)
  data <- readRDS(filename)
  return(data)
}

load_data_vecchia <- function(points, model){
  filename <- sprintf("Data_Outputs/%s_simulated_%s_uniformvecchia.rds", model, points)
  data <- readRDS(filename)
  return(data)
}

INLA_data <- lapply(points, {function(x) load_data(x, "INLA")})
SpRF_data <- lapply(points, {function(x) load_data(x, "SpRF")})
FRK_data <- lapply(points, {function(x) load_data(x, "FRK")})
GPBoost_data <- lapply(GPBoost_points, {function(x) load_data(x, "GPBoost")})
GPBoost_vecchia_data <- lapply(points, {function(x) load_data_vecchia(x, "GPBoost")})


# Get n vs time as a data frame for each model
extract_row <- function(dat, model){
  t <- dat$time$total
  units(t) <- "mins"
  row <- data.frame(n = dat$metadata$n, time = as.numeric(t), model = model)
}

INLA_df <- do.call(rbind.data.frame,  lapply(INLA_data, {function(x) extract_row(x, "INLA")}))
SpRF_df <- do.call(rbind.data.frame,  lapply(SpRF_data, {function(x) extract_row(x, "SpRF")}))
FRK_df <- do.call(rbind.data.frame,  lapply(FRK_data, {function(x) extract_row(x, "FRK")}))
GPBoost_df <- do.call(rbind.data.frame,  lapply(GPBoost_data, {function(x) extract_row(x, "GPBoost")}))
GPBoost_vecchia_df <- do.call(rbind.data.frame, lapply(GPBoost_vecchia_data, {function(x) extract_row(x, "GPBoost_vecchia")}))

all_models_df <- rbind.data.frame(INLA_df, SpRF_df, FRK_df, GPBoost_df, GPBoost_vecchia_df)


# Plot
# -------------------------------------------------------------------------
library(scales)
ggplot(data = all_models_df, mapping = aes(x = n, y = time, colour = model, shape = model)) +
  geom_point(size = 2) +
  geom_line() +
  scale_shape_manual(values = c("INLA" = 15, "SpRF" = 3, "FRK" = 16, "GPBoost" = 17, "GPBoost_vecchia" = 5)) +
  scale_color_manual(values = c("INLA" = "#00BFC4", "SpRF" = "#C77CFF", "FRK" = "#F8766D", "GPBoost" = "#7CAE00", "GPBoost_vecchia" = "#CD9600")) +
  xlab("Number of points") +
  ylab("Time (mins)")








