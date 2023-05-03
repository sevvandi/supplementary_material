# Run INLA over Africa with a spherical mesh
# Use the data simulated by adding noise to the map raster
# Save the whole model after training to examine posterior data

# This script should be run from the command line with a single command
# line argument giving the path to a chosen data file
# eg: Data_Inputs/noise_added_data_sd_0.4.rds

# Load and set up data
# -------------------------------------------------------------------------
command_args <- commandArgs(trailingOnly = TRUE)
input_file <- command_args[1]

data_container <- readRDS(input_file)
# This is a list containing, $data (dataframe), $metadata (list)

t_begin <- Sys.time()

library(dplyr)
library(readr)
library(INLA)
library(raster)

# No parallelisation
inla.setOption(num.threads = "1")

# Set the working directory to the INLA_Noise directory
setwd()

# Set the random seed
set.seed(1)

# Grid of points for prediction
load("../../Africa/Data_Inputs/Africa_grids_mh.RData")
grid <- grid_cut_mh

dat <- data_container$data
coo <- cbind(dat$longitude, dat$latitude)

# Make the spherical mesh
#------------------------------------------------------------------------
# Convert longitude/latitude to 3D cartesian and input into inla.mesh.2d,
# as suggested in Bayesian Spatial Modelling with R-INLA (Lindgren and Rue)
t_mesh_begin <- Sys.time()

loc.cartesian <- inla.mesh.map(loc = coo, projection = "longlat", inverse = TRUE)
# Need to convert max.edge and cutoff to radians (otherwise will get a coarse mesh)
mesh <- inla.mesh.2d(loc = loc.cartesian, max.edge = c(0.5, 4)*pi/180, cutoff = 0.01*pi/180 )

t_mesh_end <- Sys.time()


# Convert training and prediction coordinates to 3D cartesian
#------------------------------------------------------------------------
train_coord_longlat <- cbind(dat$longitude, dat$latitude)
train_coord_cartesian <- inla.mesh.map(train_coord_longlat, projection = "longlat", inverse = TRUE)

predict_coords_longlat <- grid@coords
predict_coord_cartesian <- inla.mesh.map(predict_coords_longlat, projection = "longlat", inverse = TRUE)


# Set up the INLA model
#------------------------------------------------------------------------
spde <- inla.spde2.matern(mesh = mesh, alpha = 2)
indexs <- inla.spde.make.index("s", n.spde = spde$n.spde )

# Matrix for estimation stack
A <- inla.spde.make.A(mesh = mesh, loc = train_coord_cartesian)

# Matrix for prediction stack
Ap <- inla.spde.make.A(mesh = mesh, loc = predict_coord_cartesian)

# Stack for estimation
# For the noisy data, use dat$noisy_positive
stk.e <- inla.stack(tag = "est",
                    data = list(y = dat$noisy_positive, numtrials = dat$examined),
                    A = list(1, A),
                    effects = list(data.frame(b0 = rep(1, nrow(train_coord_cartesian))), s = indexs))

#stack for prediction
stk.p <- inla.stack(tag = "pred",
                    data = list(y = NA, numtrials = NA),
                    A = list(1, Ap),
                    effects = list(data.frame(b0 = rep(1, nrow(predict_coord_cartesian))), s = indexs))

stk.full <- inla.stack(stk.e, stk.p)

formula <- y ~ 0 + b0 + f(s, model = spde)

# Train the model
t_train_begin <- Sys.time()
res <- inla(formula, family = "binomial", Ntrials = numtrials,
            control.family = list(link = "logit"),
            data = inla.stack.data(stk.full),
            control.predictor = list(compute = TRUE,
                                     link = 1,
                                     A = inla.stack.A(stk.full)),
            verbose=TRUE,
            quantiles =c(0.025,0.25,0.5,0.75, 0.975),
            control.inla = list(int.strategy = "eb"))
t_train_end <- Sys.time()

# Prediction
#------------------------------------------------------------------------
t_predict_begin <- Sys.time()
index.pred <- inla.stack.index(stk.full, "pred")$data
post.mean.logit <- res$summary.linear.predictor[index.pred,'mean']
post.conf.logit <- res$summary.linear.predictor[index.pred, c('0.025quant','0.25quant', '0.5quant', '0.75quant', '0.975quant')]
# Apply inverse logit
pred <- exp(post.mean.logit)/(1 + exp(post.mean.logit))
pred_confs <- exp(post.conf.logit)/(1 + exp(post.conf.logit))
t_predict_end <- Sys.time()

# Save prediction data
#------------------------------------------------------------------------
# Join the predictions to the original longitude and latitude coordinates
INLA_prediction_data <- cbind.data.frame(predict_coords_longlat, pred, pred_confs)

t_end <- Sys.time()
t_tot <- t_end - t_begin
t_train <- t_train_end - t_train_begin
t_predict <- t_predict_end - t_predict_begin
t_mesh <- t_mesh_end - t_mesh_begin

INLA_times <- data.frame(total = t_tot,
                         mesh = t_mesh,
                         training = t_train,
                         prediction = t_predict)

# Extract data on marginals:

# do.transf = TRUE means it will also include marginal densities of
# range and kappa etc, rather than of log(range) etc...
hyper_marginals <- inla.spde2.result(res, 's', spde, do.transf=TRUE)

# Intercept b0 posterior data
intercept_marginal <- res$marginals.fixed[[1]]
intercept_summary <- res$summary.fixed

# Combine data for saving into list
result <- list(data = INLA_prediction_data, time = INLA_times,
            metadata = data_container$metadata,
            hyperparam_marginals = hyper_marginals,
            intercept_marginal = intercept_marginal,
            intercept_summary = intercept_summary)


output_file <- "Data_Outputs/INLA_simulated"
# Add metadata to file name
for (i in result$metadata){
  output_file <- paste0(output_file,"_",i)
}
output_file <- paste0(output_file, ".rds")

saveRDS(result, output_file)






