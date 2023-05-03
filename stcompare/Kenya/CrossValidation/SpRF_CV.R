# Run the cross validation for 10 and 50 folds with SpRF
# -----------------------------------------------------------------------
library(ranger)
library(GSIF)
library(rgdal)
library(raster)
library(readr)
library(sp)
library(dplyr)
library(leaflet)
library(sf)
library(tmap)
library(mapview)
library(RColorBrewer)
library(ggplot2)

# Set working directory to the "Kenya" directory
setwd()

# ----------------------------------------------------------------------
dat <- readRDS("Data_Inputs/Kenya_pf_pr_2009.rds")

dat <- dat %>% mutate(x = longitude, y = latitude)
class(dat) <- "data.frame"
coordinates(dat) = ~x+y

# making a grid for prediction
set.seed(1)
kenya <- getData('GADM', country = 'KENYA', level = 1)
grid <- makegrid(kenya, cellsize = 0.1)
colnames(grid) <- c("x", "y")
grid$falsevals <- rnorm(dim(grid)[1])
coordinates(grid) = ~x+y
gridded(grid) = TRUE
kya <- kenya[kenya$NAME_0 == "Kenya", ]

# Calculate buffer distances
# ----------------------------------------------------------------------
grid_dist0 <- GSIF::buffer.dist(dat["pr"], grid[1], as.factor(1:nrow(dat)))

dn0 <- paste(names(grid_dist0), collapse="+")
fm0 <- as.formula(paste("pr ~ ", dn0))
ov.pr <- over(dat["pr"], grid_dist0)
rm.pr <- cbind(dat@data["pr"], ov.pr)

# Choose 10 or 50 folds
#foldsfile <- read.csv("Cross_Validation/Data_Outputs/Kmeans_10_clusters_as_folds.csv")
foldsfile <- read.csv("Cross_Validation/Data_Outputs/Kmeans_50_clusters_as_folds.csv")
folds <- foldsfile$clust

# Data frame to store results
preds_all <- matrix(0, nrow = nrow(dat), ncol = 11)
preds_all <- as.data.frame(preds_all)
colnames(preds_all) <- c("Actual", "Pred", "fold", "Q_0.025", "Q_0.25", "Q_0.5", "Q_0.75", "Q_0.975", "iqr","sd", "Mean")

for(kk in 1:length(unique(folds))){
  testinds <- which(folds == kk)
  traininds <- which(folds != kk)
  train <- rm.pr[traininds, ]
  test <- rm.pr[testinds, -1]
  preds_all$fold[testinds] <- kk

  mod <- ranger(fm0, train, quantreg=TRUE, num.trees=500, seed=1)
  quantiles <- c(0.025, 0.25, 0.5, 0.75, 0.975)

  pred_quant <- predict(mod, test, type="quantiles", quantiles=quantiles)$predictions
  # Mean of response
  pred_response <- predict(mod, test, type = "response")$predictions

  preds_all$Actual[testinds] <- dat@data$pr[testinds]
  preds_all$Pred[testinds] <-  pred_quant[,3]
  preds_all$Mean[testinds] <-  pred_response
  preds_all[testinds, c("Q_0.025", "Q_0.25", "Q_0.5", "Q_0.75", "Q_0.975")] <- pred_quant
  preds_all$iqr[testinds] <-  pred_quant[ ,4] - pred_quant[ ,2]

}

# Approximate standard deviation
preds_all$sd <-preds_all$iqr/1.34898

# Results analysis
# ----------------------------------------------------------------------

mse <- mean((preds_all$Actual - preds_all$Pred)^2)
rmse <- sqrt(mse)
# 10 folds 0.1320414
# 50 folds 0.1214263

cor(preds_all$Actual, preds_all$Pred)
# 10 folds 0.6413216
# 50 folds 0.7023788

# For intervals, check which actual values lie within 1SD of the mean (not the median)
within1SD <- (preds_all$Actual <= preds_all$Mean + preds_all$sd) & (preds_all$Actual >= preds_all$Mean - preds_all$sd)

within2SD <- (preds_all$Actual <= preds_all$Mean + 2*preds_all$sd) & (preds_all$Actual >= preds_all$Mean - 2*preds_all$sd)

sum(within1SD)/nrow(preds_all)

sum(within1SD)/nrow(preds_all)
# within 1sd 10 folds 0.3710526 (Note was 0.7342105 with median)
# within 1sd 50 folds 0.2815789 (Note was 0.6921053 with median)

sum(within2SD)/nrow(preds_all)
# within 2sd 10 folds 0.55 (Note was 0.8157895 with median)
# within 2sd 50 folds 0.4289474 (Note was 0.8105263 with median)


band <- rep("Out", nrow(preds_all))
band[within1SD ] <- "In_1SD"
band[!within1SD & within2SD ] <- "In_2SD"
preds_all <- cbind.data.frame(preds_all, within1SD, within2SD, band)

#write.csv(preds_all, "Cross_Validation/Data_Outputs/SpRF_10fold_CV.csv", row.names = FALSE)
write.csv(preds_all, "Cross_Validation/Data_Outputs/SpRF_50fold_CV.csv", row.names = FALSE)

