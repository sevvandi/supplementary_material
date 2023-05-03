# Run the cross validation for 10 and 50 folds with GPBoost
# -----------------------------------------------------------------------
library(gpboost)
library(dplyr)
library(readr)
library(rgdal)
library(RColorBrewer)
library(sp)
library(raster)
library(leaflet)
library(ggplot2)

# Set working directory to the "Kenya" directory
setwd()
# -----------------------------------------------------------------------
dat <- readRDS("Data_Inputs/Kenya_pf_pr_2009.rds")
dat <- dat %>% mutate(x = longitude, y = latitude)
coords <- as.matrix(dat[ ,c('x','y')])

# Choose from 10 or 50 folds
#foldsfile <- read_csv("Cross_Validation/Data_Outputs/Kmeans_10_clusters_as_folds.csv")
foldsfile <- read_csv("Cross_Validation/Data_Outputs/Kmeans_50_clusters_as_folds.csv")
folds <- foldsfile$clust

intercept <- matrix(rep(1, dim(dat)[1]), ncol=1)

# Matrix to store cross validation outputs
preds_all <- matrix(0, nrow = nrow(dat), ncol = 5)
preds_all <- as.data.frame(preds_all)
colnames(preds_all) <- c("Actual", "Pred", "Var", "sd", "fold")
preds_all$Actual <- dat$pr


gp_model <- GPModel(gp_coords = coords,
                    cov_function = "exponential")
intercept <- matrix(rep(1, dim(dat)[1]), ncol=1)


for(kk in 1:length(unique(folds))){
  testinds <- which(folds == kk)
  traininds <- which(folds != kk)

  preds_all$fold[testinds] <- kk
  coordstr <- coords[traininds, ]
  intercepttr <- matrix(intercept[traininds])
  labelstr <- dat$pr[traininds]

  gp_model <- GPModel(gp_coords = coordstr,cov_function = "exponential")

  # Training
  bst <- gpboost(data = intercepttr,
                        label = labelstr,
                        gp_model = gp_model,
                        nrounds = 247,
                        learning_rate = 0.01,
                        max_depth = 6,
                        min_data_in_leaf = 5,
                        num_leaves = 2^10,
                        objective = "regression_l2",
                        verbose = 0)

  X_test <- matrix(intercept[testinds], ncol=1)
  if(length(testinds)== 1){
    gp_coords_pred_mt <- matrix(coords[testinds, ], ncol = 2)
  }else{
    gp_coords_pred_mt <- coords[testinds, ]
  }

  # Prediction
  pred <- predict(bst, data = X_test, gp_coords_pred = gp_coords_pred_mt,
                  predict_var = TRUE, pred_latent = TRUE)

  preds_all$Pred[testinds] <- pred$fixed_effect + pred$random_effect_mean
  preds_all$Var[testinds] <- pred$random_effect_cov
  preds_all$sd[testinds] <- sqrt(pred$random_effect_cov)
}

# Results analysis
# -----------------------------------------------------------------------

within1SD <- (preds_all$Actual <= preds_all$Pred + preds_all$sd) & (preds_all$Actual >= preds_all$Pred - preds_all$sd)
within2SD <- (preds_all$Actual <= preds_all$Pred + 2*preds_all$sd) & (preds_all$Actual >= preds_all$Pred - 2*preds_all$sd)

sum(within1SD)/length(within1SD)
# 10 folds 0.8421053
# 50 folds 0.8105263

sum(within2SD)/length(within2SD)
#10 folds 0.9552632
# 50 folds 0.9105263


band <- rep("Out", nrow(preds_all))
band[within1SD ] <- "In_1SD"
band[!within1SD & within2SD ] <- "In_2SD"

preds_all <- cbind.data.frame(preds_all, within1SD, within2SD, band)
#write.csv(preds_all, "Cross_Validation/Data_Outputs/GPBoost_10_fold_CV.csv", row.names = FALSE)
write.csv(preds_all, "Cross_Validation/Data_Outputs/GPBoost_50_fold_CV.csv", row.names = FALSE)


mse <- mean((preds_all$Actual - preds_all$Pred)^2)
rmse <- sqrt(mse)
# rmse 10 folds 0.1274024
# rmse 50 folds 0.1100741


cor(preds_all$Actual, preds_all$Pred)
# cor 10 folds 0.6459994
# cor 50 folds 0.7511449

