# Run the cross validation for 10 and 50 folds with FRK
# -----------------------------------------------------------------------
library(sp)
library(ggplot2)
library(dplyr)
library(FRK)
library(readr)
library(RColorBrewer)
library(raster)
library(blockCV)
opts_FRK$set("progress",FALSE) # no progress bars
opts_FRK$set("parallel",0L)

# Set working directory to the "Kenya" directory
setwd()
# -----------------------------------------------------------------------
# Read data and get Kenya shapefile

set.seed(1)
kenya <- getData('GADM', country = 'KENYA', level = 1)
grid <- makegrid(kenya, cellsize = 0.1)
colnames(grid) <- c("x", "y")
coordinates(grid) <- ~x + y

dat <- readRDS("Data_Inputs/Kenya_pf_pr_2009.rds")
dat <- dat %>% mutate(x = longitude, y = latitude)
zdf <- data.frame(x = dat$x, y = dat$y, positive = dat$positive, k_Z = dat$examined)

coordinates(zdf) = ~x+y # change into an sp object



# Generate BAUs (Basic Areal Unit)
GridBAUs1 <- auto_BAUs(manifold = plane(), # 2D plane
                       cellsize = c(0.1,0.1), # BAU cellsize
                       type = "grid", # grid (not hex)
                       data = grid, # data around which to create BAUs
                       convex=-0.05, # border buffer factor
                       nonconvex_hull=FALSE) # convex hull
GridBAUs1$fs <- 1 # fine-scale variation at BAU level

# Basis functions
G <- auto_basis(manifold = plane(), # 2D plane
                data = zdf, # meuse data
                nres = 2, # number of resolutions
                type = "Gaussian", # type of basis function
                regular = 1) # place regularly in domain

# Choose 10 or 50 folds
#foldsfile <- read.csv("Cross_Validation/Data_Outputs/Kmeans_10_clusters_as_folds.csv")
foldsfile <- read.csv("Cross_Validation/Data_Outputs/Kmeans_50_clusters_as_folds.csv")
folds <- foldsfile$clust

# Data frame to hold cross validation results
preds_all <- matrix(0, nrow = nrow(dat), ncol = 4)
preds_all <- as.data.frame(preds_all)
colnames(preds_all) <- c("Actual", "Pred", "sd", "fold")
preds_all$Actual <- dat$pr
inds_all <- predicted_vals <- rep(0, dim(dat)[1])

# Cross Validation
# -----------------------------------------------------------------------
for(kk in 1:length(unique(folds))){
  testinds <- which(folds == kk)
  traininds <- which(folds != kk)

  zdf2 <- data.frame(x = dat$x, y = dat$y, positive = dat$positive, k_Z = dat$examined)
  ztrain <- zdf2[traininds, ]
  ztest <- zdf2[testinds, ]

  preds_all$fold[testinds] <- kk
  coordinates(ztrain) = ~x+y # change into an sp object
  coordinates(ztest) = ~x+y # change into an sp object

  # SRE Model specified
  f <- positive ~ 1  # formula for SRE model
  S <- SRE(f = f, # formula
           data = list(ztrain), # list of datasets
           BAUs = GridBAUs1, # BAUs
           basis = G, # basis functions
           response = "binomial",
           link = "logit",
           K_type = "precision") # do not average data over BAUs

  # Fit SRE Model
  S <- SRE.fit(S, # SRE model
               method = "TMB")

  # Predict SRE model
  GridBAUs2 <- predict(S, obs_fs = FALSE)
  mu_sam <- apply(GridBAUs2$MC$prob_samples, 1, mean)
  sd_sam <- apply(GridBAUs2$MC$prob_samples, 1, sd)
  BAUs_df <- data.frame(x = GridBAUs2$newdata$x,  y = GridBAUs2$newdata$y, mu = mu_sam, sd = sd_sam)

  # Get the predictions of test set
  x_y <- as.matrix(dat[ ,c("longitude", "latitude")])
  x_y_test <- x_y[testinds, ]
  df <- BAUs_df
  dp_matrix<- df[ , 1:2]
  y_pred <- df[ ,3]
  sd_pred <- df[ ,4]

  min_vals <- apply(dp_matrix, 2, min)
  dp_matrix2 <- sweep(dp_matrix, 2, min_vals, FUN = "/")
  if(length(testinds) == 1){
    x_y_2 <- x_y_test/min_vals
    x_y_2 <- matrix(x_y_2, ncol = 2)
  }else{
    x_y_2 <- sweep(x_y_test, 2, min_vals, FUN = "/")
  }



  for(ii in 1:length(testinds)){
    x_y_vals <- as.vector(x_y_2[ii, ])
    minus_vec <- sweep(dp_matrix2, 2, x_y_vals)
    abs_vals <- apply(minus_vec, 1, function(x) sum(abs(x)))
    inds_all[testinds[ii]] <- which.min(abs_vals)
    preds_all$Pred[testinds[ii]] <- y_pred[which.min(abs_vals)]
    preds_all$sd[testinds[ii]] <- sd_pred[which.min(abs_vals)]
  }

}

# Analysis of outputs
# -----------------------------------------------------------------------
within1SD <- (preds_all$Actual <= preds_all$Pred + preds_all$sd) & (preds_all$Actual >= preds_all$Pred - preds_all$sd)
within2SD <- (preds_all$Actual <= preds_all$Pred + 2*preds_all$sd) & (preds_all$Actual >= preds_all$Pred - 2*preds_all$sd)

sum(within1SD)/length(within1SD)
# 10 folds 0.8342105
# 50 folds 0.7447368
sum(within2SD)/length(within2SD)
# 10 folds 0.9236842
# 50 folds 0.85


band <- rep("Out", nrow(preds_all))
band[within1SD ] <- "In_1SD"
band[!within1SD & within2SD ] <- "In_2SD"

preds_all <- cbind.data.frame(preds_all, within1SD, within2SD, band)


#write.csv(preds_all, "Cross_Validation/Data_Outputs/FRK_10_fold_CV.csv", row.names = FALSE)
write.csv(preds_all, "Cross_Validation/Data_Outputs/FRK_50_fold_CV.csv", row.names = FALSE)



mse <- mean((preds_all$Actual - preds_all$Pred)^2)
rmse <- sqrt(mse)
# rmse 10 folds 0.1253898
# rmse 50 folds 0.1226318

cor(preds_all$Actual, preds_all$Pred)
# cor 10 folds  0.6614204
# cor 50 folds 0.7024212
