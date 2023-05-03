# Goal: Run INLA over Africa with a spherical mesh with data
# supplied by command line argument

# This script should be run from the command line with a single command
# line argument giving the path to a chosen data file
# eg: Data_Inputs/uniform_1000.rds
command_args <- commandArgs(trailingOnly = TRUE)
input_file <- command_args[1]
data_container <- readRDS(input_file)
#structure list, $data (dataframe), $metadata (list)

# Set the working directory to the "Africa_Prediction_Maps directory"INLA_likelihoods" directory
setwd()


t_begin <- Sys.time()

library(dplyr)
library(readr)
library(INLA)
library(raster)

# No parallelisation
inla.setOption(num.threads = "1")

# Set the random seed
set.seed(1)

# Load the prediction points grid
load("Data_Inputs/Africa_grids_mh.RData")
grid <- grid_cut_mh

dat <- data_container$data
coo <- cbind(dat$longitude, dat$latitude)

# Make the spherical mesh
#------------------------------------------------------------------------
# Convert longitude/latitude to 3D cartesian and input into inla.mesh.2d,
# as suggested in Bayesian Spatial Modelling with R-INLA (Lindgren and Rue)
t_mesh_begin <- Sys.time()

loc.cartesian <- inla.mesh.map(loc = coo, projection = "longlat", inverse = TRUE)
# Need to convert max.edge and cutoff to radians (otherwise get a coarse mesh)
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

#stack for estimation
stk.e <- inla.stack(tag = "est",
                    data = list(y = dat$positive, numtrials = dat$examined),
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
res <- inla(formula, family = "betabinomial", Ntrials = numtrials,
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

# Compute the mean. Note we are using median for prediction
mean <- res$summary.fitted.values[index.pred, 'mean']

# Quantiles
post.conf.logit <- res$summary.linear.predictor[index.pred, c('0.025quant','0.25quant', '0.5quant', '0.75quant', '0.975quant')]
pred_confs <- exp(post.conf.logit)/(1 + exp(post.conf.logit))

# Use median for prediction
pred <- pred_confs$`0.5quant`

# SD of the predictive distribution
pred_SD <- res$summary.fitted.values[index.pred,'sd']

t_predict_end <- Sys.time()



# Save prediction data
#------------------------------------------------------------------------
# Join the predictions to the original longitude and latitude coordinates
INLA_prediction_data <- cbind.data.frame(predict_coords_longlat, pred, pred_confs, mean, pred_SD)
names(INLA_prediction_data)[names(INLA_prediction_data) == "pred_SD"] <- "sd"

INLA_prediction_data$iqr <- INLA_prediction_data$'0.75quant' -  INLA_prediction_data$'0.25quant'

t_end <- Sys.time()
t_tot <- t_end - t_begin
t_train <- t_train_end - t_train_begin
t_predict <- t_predict_end - t_predict_begin
t_mesh <- t_mesh_end - t_mesh_begin

INLA_times <- data.frame(total = t_tot,
                         mesh = t_mesh,
                         training = t_train,
                         prediction = t_predict)

result <- list(data = INLA_prediction_data, time = INLA_times, metadata = data_container$metadata)
result$metadata$model <- "INLA"
result$metadata <- result$metadata[c("model", "n", "type")]

output_file <- "Data_Outputs/"
file_tail <- do.call(paste, c(result$metadata, sep = "_"))
output_file <- paste0("Data_Outputs/", file_tail, "betabinomial.rds")

saveRDS(result, output_file)






