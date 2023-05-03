# Run the cross validation for 10 and 50 folds with INLA
# -----------------------------------------------------------------------
library(dplyr)
library(readr)
library(malariaAtlas)
library(leaflet)
library(INLA)
library(raster)

# Set working directory to the "Kenya" directory
setwd()
dat <- readRDS("Data_Inputs/Kenya_pf_pr_2009.rds")

# Modelling
# ----------------------------------------------------------------------

# INLA mesh
coo <- cbind(dat$longitude, dat$latitude)
mesh <- inla.mesh.2d(loc = coo, max.edge = c(0.5, 4), cutoff = 0.01)

plot(mesh)
points(coo, col = "red")

spde <- inla.spde2.matern(mesh = mesh, alpha = 2)
indexs <- inla.spde.make.index("s", n.spde = spde$n.spde )

# Data frame to hold cross validation results
preds_all <- matrix(0, nrow=nrow(dat), ncol=10)
preds_all <- as.data.frame(preds_all)
names(preds_all) <- c("Actual", "Pred", "fold", "0.025quant", "0.25quant","0.5quant","0.75quant","0.975quant","sd", "Mean")

preds_conf <- matrix(0, nrow=nrow(dat), ncol=5)
preds_conf <- as.data.frame(preds_conf)

# Choose 10 or 50 folds
#foldsfile <- read_csv("Cross_Validation/Data_Outputs/Kmeans_10_clusters_as_folds.csv")
foldsfile <- read_csv("Cross_Validation/Data_Outputs/Kmeans_50_clusters_as_folds.csv")
folds <- foldsfile$clust

for(kk in 1:length(unique(folds))){
  testinds <- which(folds == kk)
  traininds <- which(folds != kk)

  test <- dat[testinds, ]
  train <- dat[traininds, ]
  preds_all$fold[testinds] <- kk

  # Create the training and testing coordinates
  train_coo <- dat[traininds, c("longitude", "latitude")]
  test_coo <- dat[testinds, c("longitude", "latitude")]

  train_coo <- as.matrix(train_coo)
  test_coo <- as.matrix(test_coo)

  A <- inla.spde.make.A(mesh = mesh, loc = train_coo)
  Ap <- inla.spde.make.A(mesh = mesh, loc = test_coo)

  # Stack for estimation stk.e
  stk.e <- inla.stack(tag = "est",
                      data = list(y = train$positive, numtrials = train$examined),
                      A = list(1, A),
                      effects = list(data.frame(b0 = rep(1, nrow(train_coo))), s = indexs))  #

  # Stack for prediction stk.p
  stk.p <- inla.stack(tag = "pred",
                      data = list(y = NA, numtrials = NA),
                      A = list(1, Ap),
                      effects = list(data.frame(b0 = rep(1, nrow(test_coo))), s = indexs))

  # stk.full has stk.e and stk.p
  stk.full <- inla.stack(stk.e, stk.p)

  formula <- y ~ 0 + b0 + f(s, model = spde)

  res <- inla(formula, family = "binomial", Ntrials = numtrials,
              control.family = list(link = "logit"),
              data = inla.stack.data(stk.full),
              control.predictor = list(compute = TRUE, link = 1, A = inla.stack.A(stk.full)), verbose=TRUE,
              quantiles =c(0.025,0.25,0.5,0.75, 0.975),
              control.inla = list(int.strategy = "eb"))

  # Prediction
  index.pred <- inla.stack.index(stk.full, "pred")$data
  post.conf.logit <- res$summary.linear.predictor[index.pred, c('0.025quant','0.25quant', '0.5quant', '0.75quant', '0.975quant')]
  pred_confs <- exp(post.conf.logit)/(1 + exp(post.conf.logit))

  # Use median for prediction
  pred <- pred_confs$'0.5quant'

  # Get the mean of the predictive distribution for interval analysis
  mean <- res$summary.fitted.values[index.pred, 'mean']

  obs <- test$pr

  # SD of the response (transformed from linear predictor)
  sd <- res$summary.fitted.values[index.pred, 'sd']

  preds_all$Actual[testinds] <- obs
  preds_all$Pred[testinds] <- pred
  preds_all$Mean[testinds] <- mean
  preds_all$sd[testinds] <- sd
  preds_all[testinds, c('0.025quant','0.25quant', '0.5quant', '0.75quant', '0.975quant')] <- pred_confs
}



# Analysis of outputs

mse <- mean((preds_all$Actual - preds_all$Pred)^2)
rmse <- sqrt(mse)
# rmse 10 folds 0.1811979
# rmse 50 folds 0.1237354

correlation <- cor(preds_all$Actual, preds_all$Pred)
# cor 10 folds 0.2353249
# cor 50 folds 0.6833653

# For intervals, check which actual values lie within 1SD of the mean (not the median)
within1SD <- (preds_all$Actual <= preds_all$Mean + preds_all$sd) & (preds_all$Actual >= preds_all$Mean - preds_all$sd)
within2SD <- (preds_all$Actual <= preds_all$Mean + 2*preds_all$sd) & (preds_all$Actual >= preds_all$Mean - 2*preds_all$sd)


sum(within1SD)/nrow(preds_all)
# within 1sd 10 folds 0.7815789 (was 0.75 for median)
# within 1sd 50 folds 0.8552632 (was 0.8105263 for median)

sum(within2SD)/nrow(preds_all)
# within 2sd 10 folds 0.8921053 (was 0.8710526 for median)
# within 2sd 50 folds 0.9631579 (was 0.9578947 for median)

band <- rep("Out", nrow(preds_all))
band[within1SD ] <- "In_1SD"
band[!within1SD & within2SD] <- "In_2SD"
preds_all <- cbind.data.frame(preds_all, within1SD, within2SD, band)

#write.csv(preds_all, "Cross_Validation/Data_Outputs/INLA_10fold_CV.csv", row.names = FALSE)
write.csv(preds_all, "Cross_Validation/Data_Outputs/INLA_50fold_CV.csv", row.names = FALSE)


