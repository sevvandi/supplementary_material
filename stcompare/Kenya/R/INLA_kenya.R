# Train an INLA model on the 2009 Kenya malaria prevalence data and produce
# a map of predictions
t_begin <- Sys.time()

library(dplyr)
library(readr)
library(INLA)
library(raster)

# Set the working directory to the Kenya directory
setwd()

# No parallelisation
inla.setOption(num.threads = "1")


# Set up the data and coordinates
#------------------------------------------------------------------------
dat <- readRDS("Data_Inputs/Kenya_pf_pr_2009.rds")
coords <- cbind(dat$longitude, dat$latitude)

# Build the mesh
t_mesh_begin <- Sys.time()
mesh <- inla.mesh.2d(loc = coords, max.edge = c(0.5, 4), cutoff = 0.01)
t_mesh_end <- Sys.time()

# Plot the mesh
plot(mesh)
points(cbind(dat$longitude, dat$latitude), col = "red")

spde <- inla.spde2.matern(mesh = mesh, alpha = 2)
indexs <- inla.spde.make.index("s", n.spde = spde$n.spde )

# Generate a grid of points over Kenya to do prediction at
kenya <- getData('GADM', country = 'KENYA', level = 1)
grid <- makegrid(kenya, cellsize = 0.1)
colnames(grid) <- c("x", "y")
coordinates(grid) = ~x+y
proj4string(grid) <- proj4string(kenya) # Give it the WGS84 CRS

# Remove points lying outside of Kenya
kya <- kenya[kenya$NAME_0 == "Kenya", ]
grid <- grid[kya, ]

# Extract the matrix of remaining coordinates
predict_coords <- grid@coords

train <- dat

# Create the training coordinates
train_coords <- as.matrix(coords)


# Set up the INLA model
#------------------------------------------------------------------------
# Matrix for estimation stack
A <- inla.spde.make.A(mesh = mesh, loc = train_coords)

# Matrix for prediction stack
Ap <- inla.spde.make.A(mesh = mesh, loc = predict_coords)

# Stack for estimation
stk.e <- inla.stack(tag = "est",
                    data = list(y = dat$positive, numtrials = dat$examined),
                    A = list(1, A),
                    effects = list(data.frame(b0 = rep(1, nrow(train_coords))), s = indexs))

# Stack for prediction
stk.p <- inla.stack(tag = "pred",
                    data = list(y = NA, numtrials = NA),
                    A = list(1, Ap),
                    effects = list(data.frame(b0 = rep(1, nrow(predict_coords))), s = indexs))

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
t_end <- Sys.time()

t_tot <- t_end - t_begin
t_train <- t_train_end - t_train_begin
t_predict <- t_predict_end - t_predict_begin
t_mesh <- t_mesh_end - t_mesh_begin

INLA_times <- data.frame(total = t_tot,
                         mesh = t_mesh,
                         training = t_train,
                         prediction = t_predict)

INLA_prediction_data <- cbind.data.frame(predict_coords, pred, pred_confs, pred_SD)

INLA_metadata <- list(model = "INLA")

result <- list(data = INLA_prediction_data, time = INLA_times, metadata = INLA_metadata)

saveRDS(result, file = "Prediction_Maps/Data_Outputs/INLA_prediction_data.rds")

