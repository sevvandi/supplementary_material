# Train a GPBoost model on the 2009 Kenya malaria prevalence data and produce
# a map of predictions
# Experiment with the Vecchia approximation



library(gpboost)
library(dplyr)
library(readr)
library(rgdal)
library(sp)
library(raster)

# Set working directory to the "GPBoost_vecchia" directory
set


# Set up the training data and coordinates
#------------------------------------------------------------------------
dat <- readRDS("../../Kenya/Data_Inputs/Kenya_pf_pr_2009.rds")
dat <- dat %>% mutate(x = longitude, y = latitude)

# GPBoost requires the coordinates as a matrix
coords <- as.matrix(dat[ ,c('x','y')])

# Set up and train the model
#------------------------------------------------------------------------
mv <- 30
mvp <- 300

gp_model <- GPModel(gp_coords = coords,
                    cov_function = "exponential",
                    vecchia_approx = TRUE,
                    num_neighbors = mv
                    )

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
               pred_latent = TRUE,
               num_neighbors_pred = mvp
               )
prev_pred <- pred$fixed_effect + pred$random_effect_mean
var_pred<- pred$random_effect_cov
lower_and_upper[, 1] <- prev_pred - sqrt(pred$random_effect_cov)
lower_and_upper[, 2] <- prev_pred + sqrt(pred$random_effect_cov)
t_predict_end <- Sys.time()

# Connect the vector y_pred with the prediction locations
GPBoost_prediction_data <- data.frame(grid@coords, prev_pred, var_pred, lower = lower_and_upper[,1], upper = lower_and_upper[,2])

# Plotting
#------------------------------------------------------------------------

kenyaBorder <- readRDS("../../Kenya/Data_Inputs/Kenya_border_simple.rds")
plot_common_theme <- theme(
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  plot.margin = unit(c(-0.6,0,-0.6,0), 'cm')
)


# We have to clip the GPBoost data as it has small negative values
GPBoost_prediction_data$clipped <- clamp(GPBoost_prediction_data$prev_pred, 0, 1)

GPBoost_pred_plot <- ggplot(data = GPBoost_prediction_data, mapping = aes(x = x, y = y, fill = clipped, color = clipped)) +
  geom_tile() +
  geom_polygon(data = kenyaBorder, aes(x = long, y = lat, group = group), colour = "black", size = 0.6, fill = NA)+
  scale_fill_distiller(palette = "Spectral", limits = c(0,0.8), name = "Prevalence") +
  scale_color_distiller(palette = "Spectral", limits = c(0,0.8), name = "Prevalence") +
  theme_bw() +
  coord_quickmap() +
  plot_common_theme +
  theme(
    legend.position = "left"
  )

GPBoost_pred_plot
im_width <- 10
im_height <- 8
im_res <- 300
png(paste0("Figures/Kenya_mv",mv,"_mvp",mvp,".png"), units = "cm", width = im_width, height = im_height, res = im_res)
GPBoost_pred_plot
dev.off()

