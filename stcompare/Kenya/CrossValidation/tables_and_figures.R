# -----------------------------------------------------------------------
# TASK 1  : PLOT KENYA AND THE KMEANS POINTS
# TASK 2  : PLOT PREDICTIONS VS ACTUALS - 10 FOLD
# TASK 3  : PLOT PREDICTIONS VS ACTUALS - 50 FOLD
# TASK 4  : PLOT ACUAL PREVALENCE VALUES AND THE DENSITY OF POINTS
# TASK 5  : PLOT ERRORS VALUES AND THE DENSITY OF POINTS - 10 FOLD
# TASK 6  : PLOT ERRORS VALUES AND THE DENSITY OF POINTS - 50 FOLD
# TASK 7  : TABLE DATA - FOR DIFFERENT DENSITY POINTS - 10 FOLD
# TASK 8  : TABLE DATA - FOR DIFFERENT DENSITY POINTS - 50 FOLD
# TASK 9  : TABLE DATA - FOR INTERVALS - 10 FOLD
# TASK 10 : TABLE DATA - FOR INTERVALS - 50 FOLD
# TASK 11 : GRAPHS FOR INTERVALS - 10 FOLDS
# TASK 12 : GRAPHS FOR INTERVALS - 50 FOLDS
# TASK 13 : EXPLORE INTERVAL WIDTH AND DENSITY - 10 FOLDS
# TASK 14 : EXPLORE INTERVAL WIDTH AND DENSITY - 50 FOLDS
# -----------------------------------------------------------------------

# -----------------------------------------------------------------------
# TASK 1 : PLOT KENYA AND THE KMEANS POINTS
# -----------------------------------------------------------------------
library(dplyr)
library(readr)
library(malariaAtlas)
library(leaflet)
library(raster)

# Set working directory to the "Kenya" directory
setwd()


KNY_shp <- readRDS("Data_Inputs/Kenya_border_simple.rds")

dat <- readRDS("Data_Inputs/Kenya_pf_pr_2009.rds")

# 10 folds
foldsfile <- read.csv("Cross_Validation/Data_Outputs/Kmeans_10_clusters_as_folds.csv")
folds <- foldsfile$clust
df <- data.frame(latitude = dat$latitude, longitude = dat$longitude, cluster = folds)

factpal <- colorFactor(rainbow(10), df$cluster)

leaflet(KNY_shp) %>%
  addPolygons(fillOpacity = 0) %>%
  addTiles() %>%
  addCircles(lng = df$longitude, lat = df$latitude, color = ~factpal(df$cluster))


foldsfile <- read.csv("Cross_Validation/Data_Outputs/Kmeans_50_clusters_as_folds.csv")
folds <- foldsfile$clust

df <- data.frame(latitude = dat$latitude, longitude = dat$longitude, cluster = folds)

factpal <- colorFactor(rainbow(50), df$cluster)

leaflet(KNY_shp) %>%
  addPolygons(fillOpacity = 0) %>%
  addTiles() %>%
  addCircles(lng = df$longitude, lat = df$latitude, color = ~factpal(df$cluster)) %>%
  addLegend("bottomright", pal = factpal, values = ~df$cluster, title = "Fold")


# -----------------------------------------------------------------------
# TASK 2 : PLOT PREDICTIONS VS ACTUALS - 10 FOLD
# -----------------------------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)

dat_rf <- read_csv("Cross_Validation/Data_Outputs/SpRF_10fold_CV.csv")
df <- data.frame(model = "SpRF", Actual = dat_rf$Actual, Predicted = dat_rf$Pred)
round(sqrt(mean((df$Actual - df$Predicted)^2)), 3)
round(cor(df$Actual, df$Predicted), 3)
round(sum(abs(df$Actual - df$Predicted) <= 0.05)/length(df$Actual)*100, 3)
round(sum(abs(df$Actual - df$Predicted) <= 0.1)/length(df$Actual)*100, 3)
round(sum(abs(df$Actual - df$Predicted) <= 0.2)/length(df$Actual)*100, 3)

dat_gpboost <- read_csv("Cross_Validation/Data_Outputs/GPBoost_10_fold_CV.csv")
df1 <- data.frame(model = "GPBoost", Actual = dat_gpboost$Actual, Predicted = dat_gpboost$Pred)
round(sqrt(mean((df1$Actual - df1$Predicted)^2)), 3)
round(cor(df1$Actual, df1$Predicted), 3)
round(sum(abs(df1$Actual - df1$Predicted) <= 0.05)/length(df1$Actual)*100, 3)
round(sum(abs(df1$Actual - df1$Predicted) <= 0.1)/length(df1$Actual)*100, 3)
round(sum(abs(df1$Actual - df1$Predicted) <= 0.2)/length(df1$Actual)*100, 3)

df_all <- bind_rows(df, df1)

dat_inla <- read_csv("Cross_Validation/Data_Outputs/INLA_10fold_CV.csv")
df2 <- data.frame(model = "INLA", Actual = dat_inla$Actual, Predicted = dat_inla$Pred)
round(sqrt(mean((df2$Actual - df2$Predicted)^2)), 3)
round(cor(df2$Actual, df2$Predicted), 3)
round(sum(abs(df2$Actual - df2$Predicted) <= 0.05)/length(df2$Actual)*100, 3)
round(sum(abs(df2$Actual - df2$Predicted) <= 0.1)/length(df2$Actual)*100, 3)
round(sum(abs(df2$Actual - df2$Predicted) <= 0.2)/length(df2$Actual)*100, 3)

df_all <- bind_rows(df_all, df2)

dat_frk <- read_csv("Cross_Validation/Data_Outputs/FRK_10_fold_CV.csv")
df3 <- data.frame(model = "FRK", Actual = dat_frk$Actual, Predicted = dat_frk$Pred)
round(sqrt(mean((df3$Actual - df3$Predicted)^2)), 3)
round(cor(df3$Actual, df3$Predicted), 3)
round(sum(abs(df3$Actual - df3$Predicted) <= 0.05)/length(df3$Actual)*100, 3)
round(sum(abs(df3$Actual - df3$Predicted) <= 0.1)/length(df3$Actual)*100, 3)
round(sum(abs(df3$Actual - df3$Predicted) <= 0.2)/length(df3$Actual)*100, 3)

df_all <- bind_rows(df_all, df3)

df_all$model <- factor(df_all$model, levels = c("INLA","GPBoost","SpRF","FRK"))
ggplot(df_all, aes(Actual, Predicted)) +
  geom_point() + facet_wrap(~model) +
  geom_abline(slope = 1) +
  theme_bw()

identical(dat_rf$Actual, dat_gpboost$Actual)
identical(dat_rf$Actual, dat_inla$Actual)
identical(dat_rf$Actual, dat_frk$Actual)

dim(dat_rf)
point_id <- rep( paste("P", 1:dim(dat_rf)[1], sep=""), 4)
df_all <- bind_cols(df_all, point_id)
df_all <- df_all %>% rename(point_id = '...4')

ggplot(df_all, aes(Actual, Predicted)) +
  geom_point(aes(color = point_id)) +
  facet_wrap(~model) +
  geom_abline(slope = 1) +
  theme_bw() +
  theme(legend.position = "none")

foldsfile <- read.csv("Cross_Validation/Data_Outputs/Kmeans_10_clusters_as_folds.csv")
folds <- foldsfile$clust

pal <- rainbow(10)

df_all <- df_all %>% mutate(Fold = as.factor(rep(folds, 4)))

pred_vs_actual_plot <- ggplot(df_all, aes(x = Actual, y = Predicted, color = Fold)) +
  geom_point() +
  facet_wrap(~model) +
  geom_abline(slope = 1) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  scale_color_manual(values=pal)

im_width <- 12
im_height <- 10
im_res <- 300

png("Cross_Validation/Figures/10_fold_cv_pred_vs_actual.png", units = "cm", width = im_width, height = im_height, res = im_res)
pred_vs_actual_plot
dev.off()



ggplot(df_all, aes(x = Actual, y = Predicted, color = model, shape = model)) +
  geom_point() +
  facet_wrap(~Fold, ncol = 5) +
  geom_abline(slope = 1) +
  theme_bw()


# -----------------------------------------------------------------------
# TASK 3 : PLOT PREDICTIONS VS ACTUALS - 50 FOLD
# -----------------------------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)

dat_rf <- read_csv("Cross_Validation/Data_Outputs/SpRF_50fold_CV.csv")
df <- data.frame(model = "SpRF", Actual = dat_rf$Actual, Predicted = dat_rf$Pred)
round(sqrt(mean((df$Actual - df$Predicted)^2)), 3)
round(cor(df$Actual, df$Predicted), 3)
round(sum(abs(df$Actual - df$Predicted) <= 0.05)/length(df$Actual)*100, 3)
round(sum(abs(df$Actual - df$Predicted) <= 0.1)/length(df$Actual)*100, 3)
round(sum(abs(df$Actual - df$Predicted) <= 0.2)/length(df$Actual)*100, 3)

dat_gpboost <- read_csv("Cross_Validation/Data_Outputs/GPBoost_50_fold_CV.csv")
df1 <- data.frame(model = "GPBoost", Actual = dat_gpboost$Actual, Predicted = dat_gpboost$Pred)
round(sqrt(mean((df1$Actual - df1$Predicted)^2)), 3)
round(cor(df1$Actual, df1$Predicted), 3)
round(sum(abs(df1$Actual - df1$Predicted) <= 0.05)/length(df1$Actual)*100, 3)
round(sum(abs(df1$Actual - df1$Predicted) <= 0.1)/length(df1$Actual)*100, 3)
round(sum(abs(df1$Actual - df1$Predicted) <= 0.2)/length(df1$Actual)*100, 3)

df_all <- bind_rows(df, df1)

dat_inla <- read_csv("Cross_Validation/Data_Outputs/INLA_50fold_CV.csv")
df2 <- data.frame(model = "INLA", Actual = dat_inla$Actual, Predicted = dat_inla$Pred)
round(sqrt(mean((df2$Actual - df2$Predicted)^2)), 3)
round(cor(df2$Actual, df2$Predicted), 3)
round(sum(abs(df2$Actual - df2$Predicted) <= 0.05)/length(df2$Actual)*100, 3)
round(sum(abs(df2$Actual - df2$Predicted) <= 0.1)/length(df2$Actual)*100, 3)
round(sum(abs(df2$Actual - df2$Predicted) <= 0.2)/length(df2$Actual)*100, 3)

df_all <- bind_rows(df_all, df2)

dat_frk <- read_csv("Cross_Validation/Data_Outputs/FRK_50_fold_CV.csv")
df3 <- data.frame(model = "FRK", Actual = dat_frk$Actual, Predicted = dat_frk$Pred)
round(sqrt(mean((df3$Actual - df3$Predicted)^2)), 3)
round(cor(df3$Actual, df3$Predicted), 3)
round(sum(abs(df3$Actual - df3$Predicted) <= 0.05)/length(df3$Actual)*100, 3)
round(sum(abs(df3$Actual - df3$Predicted) <= 0.1)/length(df3$Actual)*100, 3)
round(sum(abs(df3$Actual - df3$Predicted) <= 0.2)/length(df3$Actual)*100, 3)

df_all <- bind_rows(df_all, df3)
df_all$model <- factor(df_all$model, levels = c("INLA", "GPBoost", "SpRF", "FRK"))

ggplot(df_all, aes(Actual, Predicted)) +
  geom_point() + facet_wrap(~model) +
  geom_abline(slope = 1) +
  theme_bw()

identical(dat_rf$Actual, dat_gpboost$Actual)
identical(dat_rf$Actual, dat_inla$Actual)
identical(dat_rf$Actual, dat_frk$Actual)


point_id <- rep( paste("P", 1:dim(dat_rf)[1], sep=""), 4)
df_all <- bind_cols(df_all, point_id)
df_all <- df_all %>% rename(point_id = '...4')

ggplot(df_all, aes(Actual, Predicted)) +
  geom_point(aes(color = point_id)) +
  facet_wrap(~model) +
  geom_abline(slope = 1) +
  theme_bw() +
  theme(legend.position = "none")

foldsfile <- read.csv("Cross_Validation/Data_Outputs/Kmeans_50_clusters_as_folds.csv")
folds <- foldsfile$clust

pal <- rainbow(50)

df_all <- df_all %>% mutate(Fold = as.factor(rep(folds, 4)))


pred_vs_actual_plot <- ggplot(df_all, aes(x = Actual, y = Predicted, color = Fold)) +
  geom_point() +
  facet_wrap(~model) +
  geom_abline(slope = 1) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "none") +
  scale_color_manual(values=pal)

im_width <- 10
im_height <- 10
im_res <- 300

png("Cross_Validation/Figures/50_fold_cv_pred_vs_actual.png", units = "cm", width = im_width, height = im_height, res = im_res)
pred_vs_actual_plot
dev.off()

ggplot(df_all, aes(x = Actual, y = Predicted, color = model, shape = model)) +
  geom_point() +
  facet_wrap(~Fold, ncol = 7) +
  geom_abline(slope = 1) +
  theme_bw()


# -----------------------------------------------------------------------
# TASK 4 : PLOT ACUAL PREVALENCE VALUES AND THE DENSITY OF POINTS
# -----------------------------------------------------------------------

library(dplyr)
library(readr)
library(malariaAtlas)
library(leaflet)
library(raster)
library(ggplot2)

library(KernSmooth)

#KNY_shp <- getShp(country = "Kenya", admin_level = c("admin0"))
KNY_shp <- readRDS("Kenya_border_simple.rds")
dat <- readRDS("Data_Inputs/Kenya_pf_pr_2009.rds")

pal <- colorNumeric(palette = rev(topo.colors(10)), domain = dat$pr)
pal <- colorNumeric(palette = colorRamp(c('blue', 'orange', 'red'), interpolate="linear"),
                    domain = dat$pr)

leaflet(KNY_shp) %>%
  addPolygons(fillOpacity = 0) %>%
  addTiles() %>%
  addCircles(lng = dat$longitude, lat = dat$latitude, color = ~pal(dat$pr)) %>%
  addLegend("bottomright", pal = pal, values = dat$pr, title = "Prevalence")


# COMPUTE DENSITY (now it has been computed, just load in the results)
dat <- readRDS("Data_Inputs/Kenya_pf_pr_2009.rds")
# df <- matrix(c(dat$longitude, dat$latitude), ncol = 2)
# est <- bkde2D(df, bandwidth = 0.1 )
#
# gr <- data.frame(with(est, expand.grid(x1,x2)), as.vector(est$fhat))
# names(gr) <- c("xgr", "ygr", "zgr")
#
# # Fit a model
# mod <- loess(zgr~xgr*ygr, data=gr)
#
# # Apply the model to the original data to estimate density at that point
# pointdens <- predict(mod, newdata=data.frame(xgr=df[ ,1], ygr=df[ ,2]))
# pointdens[which.min(pointdens)] <- 0
# min(pointdens)
# pointdens <- pointdens*10
# df2 <- cbind.data.frame(df, pointdens)
# colnames(df2)[1:2] <- c("longitude", "latitude")
# write.csv(df2, "Cross_Validation/Data_Outputs/KDE_of_observations.csv", row.names = FALSE)

df2 <- read_csv("Cross_Validation/Data_Outputs/KDE_of_observations.csv")

pal <- colorNumeric(palette = colorRamp(c('blue', 'orange', 'red'), interpolate="linear"),
                    domain = df2$pointdens)

leaflet(KNY_shp) %>%
  addPolygons(fillOpacity = 0) %>%
  addTiles() %>%
  addCircles(lng = df2$longitude, lat = df2$latitude, color = ~pal(df2$pointdens)) %>%
  addLegend("bottomright", pal = pal, values = df2$pointdens, title = "Density")


common_theme <- theme(axis.title= element_text(size = 18),
                      legend.text = element_text(size = 16),
                      legend.title = element_text(size = 18))

foldsfile <- read.csv("Cross_Validation/Data_Outputs/Kmeans_10_clusters_as_folds.csv")
folds <- foldsfile$clust
inds <- which(dat$pr != 0 )
dfpl <- data.frame(Density = df2$pointdens[inds], Prevalence = dat$pr[inds], Fold = as.factor(folds[inds]) )
# dfpl <- data.frame(Density = pointdens, Prevalence = dat$pr, Fold = as.factor(folds) )
density_actual_10 <- ggplot(dfpl, aes(x = Density, y = Prevalence, color = Fold)) +
  geom_point(size = 2) +
  theme_bw() +
  common_theme



foldsfile <- read.csv("Cross_Validation/Data_Outputs/Kmeans_50_clusters_as_folds.csv")
folds <- foldsfile$clust
inds <- which(dat$pr != 0 )
dfpl <- data.frame(Density = df2$pointdens[inds], Prevalence = dat$pr[inds], Fold = as.factor(folds[inds]) )
#dfpl <- data.frame(Density = pointdens, Prevalence = dat$pr, Fold = as.factor(folds) )
density_actual_50 <- ggplot(dfpl, aes(x = Density, y = Prevalence, color = Fold)) +
  geom_point(size = 2) +
  theme_bw() +
  common_theme

im_height <- 12
im_width <- 24
im_res <- 300
png("Cross_Validation/Figures/density_actuals_both.png", units = "cm", height = im_height, width = im_width, res = im_res)
gridExtra::grid.arrange(density_actual_10,density_actual_50, nrow = 1, widths = c(0.4, 0.45))
dev.off()




# -----------------------------------------------------------------------
# TASK 5 : PLOT ERRORS VALUES AND THE DENSITY OF POINTS - 10 FOLD
# -----------------------------------------------------------------------
df2 <- read.csv("Cross_Validation/Data_Outputs/KDE_of_observations.csv")
pointdens <- df2$pointdens

foldsfile <- read.csv("Cross_Validation/Data_Outputs/Kmeans_10_clusters_as_folds.csv")
folds <- as.factor(foldsfile$clust)

dat_mod <- read_csv("Cross_Validation/Data_Outputs/SpRF_10fold_CV.csv")
df <- data.frame(Actual = dat_mod$Actual,
                 Predicted = dat_mod$Pred,
                 Density = pointdens,
                 Error = abs(dat_mod$Actual - dat_mod$Pred ),
                 Fold = folds,
                 model = 'SpRF')
dat_all <- df

dat_mod <- read_csv("Cross_Validation/Data_Outputs/GPBoost_10_fold_CV.csv")
df <- data.frame(Actual = dat_mod$Actual,
                 Predicted = dat_mod$Pred,
                 Density = pointdens,
                 Error = abs(dat_mod$Actual - dat_mod$Pred ),
                 Fold = folds,
                 model = 'GPBoost')
dat_all <- bind_rows(dat_all, df)


dat_mod <- read_csv("Cross_Validation/Data_Outputs/INLA_10fold_CV.csv")
df <- data.frame(Actual = dat_mod$Actual,
                 Predicted = dat_mod$Pred,
                 Density = pointdens,
                 Error = abs(dat_mod$Actual - dat_mod$Pred ),
                 Fold = folds,
                 model = 'INLA')
dat_all <- bind_rows(dat_all, df)


dat_mod <- read_csv("Cross_Validation/Data_Outputs/FRK_10_fold_CV.csv")
df <- data.frame(Actual = dat_mod$Actual,
                 Predicted = dat_mod$Pred,
                 Density = pointdens,
                 Error = abs(dat_mod$Actual - dat_mod$Pred ),
                 Fold = folds,
                 model = 'FRK')
dat_all <- bind_rows(dat_all, df)

dat_all$model <- factor(dat_all$model, levels = c("INLA", "GPBoost", "SpRF", "FRK"))

 ggplot(dat_all, aes(x = Density, y = Error, color = Fold)) +
  geom_point() +
  facet_wrap(~model) +
  theme_bw()


# -----------------------------------------------------------------------
# TASK 6 : PLOT ERRORS VALUES AND THE DENSITY OF POINTS - 50 FOLD
# -----------------------------------------------------------------------
df2 <- read.csv("Cross_Validation/Data_Outputs/KDE_of_observations.csv")
pointdens <- df2$pointdens

foldsfile <- read.csv("Cross_Validation/Data_Outputs/Kmeans_50_clusters_as_folds.csv")
folds <- as.factor(foldsfile$clust)

dat_mod <- read_csv("Cross_Validation/Data_Outputs/SpRF_50fold_CV.csv")
df <- data.frame(Actual = dat_mod$Actual,
                 Predicted = dat_mod$Pred,
                 Density = pointdens,
                 Error = abs(dat_mod$Actual - dat_mod$Pred),
                 Fold = folds,
                 model = 'SpRF')
dat_all <- df

dat_mod <- read_csv("Cross_Validation/Data_Outputs/GPBoost_50_fold_CV.csv")
df <- data.frame(Actual = dat_mod$Actual,
                 Predicted = dat_mod$Pred,
                 Density = pointdens,
                 Error = abs(dat_mod$Actual - dat_mod$Pred ),
                 Fold = folds,
                 model = 'GPBoost')
dat_all <- bind_rows(dat_all, df)


dat_mod <- read_csv("Cross_Validation/Data_Outputs/INLA_50fold_CV.csv")
df <- data.frame(Actual = dat_mod$Actual,
                 Predicted = dat_mod$Pred,
                 Density = pointdens,
                 Error = abs(dat_mod$Actual - dat_mod$Pred ),
                 Fold = folds,
                 model = 'INLA')
dat_all <- bind_rows(dat_all, df)


dat_mod <- read_csv("Cross_Validation/Data_Outputs/FRK_50_fold_CV.csv")
df <- data.frame(Actual = dat_mod$Actual,
                 Predicted = dat_mod$Pred,
                 Density = pointdens,
                 Error = abs(dat_mod$Actual - dat_mod$Pred),
                 Fold = folds,
                 model = 'FRK')
dat_all <- bind_rows(dat_all, df)

dat_all$model <- factor(dat_all$model, levels = c("INLA", "GPBoost", "SpRF", "FRK"))

ggplot(dat_all, aes(x = Density, y = Error, color = Fold)) +
  geom_point() +
  facet_wrap(~model) +
  theme_bw()


# -----------------------------------------------------------------------
# TASK 7 : TABLE DATA - FOR DIFFERENT DENSITY POINTS - 10 FOLD
# -----------------------------------------------------------------------
library(dplyr)
library(readr)


densities <- read_csv("Cross_Validation/Data_Outputs/KDE_of_observations.csv")
# ---------------------------------------------------------------
# LOW DENSITIES
dat_rf <- read_csv("Cross_Validation/Data_Outputs/SpRF_10fold_CV.csv")
df <- data.frame(model = "SpRF", Actual = dat_rf$Actual, Predicted = dat_rf$Pred, Density = densities$pointdens )
inds <- which(df$Density <=0.2)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_gpboost <- read_csv("Cross_Validation/Data_Outputs/GPBoost_10_fold_CV.csv")
df <- data.frame(model = "GPBoost", Actual = dat_gpboost$Actual, Predicted = dat_gpboost$Pred, Density = densities$pointdens)
inds <- which(df$Density <=0.2)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_inla <- read_csv("Cross_Validation/Data_Outputs/INLA_10fold_CV.csv")
df <- data.frame(model = "INLA", Actual = dat_inla$Actual, Predicted = dat_inla$Pred, Density = densities$pointdens)
inds <- which(df$Density <=0.2)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_frk <- read_csv("Cross_Validation/Data_Outputs/FRK_10_fold_CV.csv")
df <- data.frame(model = "FRK", Actual = dat_frk$Actual, Predicted = dat_frk$Pred, Density = densities$pointdens)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


# ---------------------------------------------------------------
# MEDIUM DENSITIES
dat_rf <- read_csv("Cross_Validation/Data_Outputs/SpRF_10fold_CV.csv")
df <- data.frame(model = "SpRF", Actual = dat_rf$Actual, Predicted = dat_rf$Pred, Density = densities$pointdens )
inds <- which((df$Density >0.2) &(df$Density  <=0.4))
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_gpboost <- read_csv("Cross_Validation/Data_Outputs/GPBoost_10_fold_CV.csv")
df <- data.frame(model = "GPBoost", Actual = dat_gpboost$Actual, Predicted = dat_gpboost$Pred, Density = densities$pointdens)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_inla <- read_csv("Cross_Validation/Data_Outputs/INLA_10fold_CV.csv")
df <- data.frame(model = "INLA", Actual = dat_inla$Actual, Predicted = dat_inla$Pred, Density = densities$pointdens)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_frk <- read_csv("Cross_Validation/Data_Outputs/FRK_10_fold_CV.csv")
df <- data.frame(model = "FRK", Actual = dat_frk$Actual, Predicted = dat_frk$Pred, Density = densities$pointdens)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


# ---------------------------------------------------------------
# HIGH DENSITIES
dat_rf <- read_csv("Cross_Validation/Data_Outputs/SpRF_10fold_CV.csv")
df <- data.frame(model = "SpRF", Actual = dat_rf$Actual, Predicted = dat_rf$Pred, Density = densities$pointdens )
inds <- which(df$Density  > 0.4)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_gpboost <- read_csv("Cross_Validation/Data_Outputs/GPBoost_10_fold_CV.csv")
df <- data.frame(model = "GPBoost", Actual = dat_gpboost$Actual, Predicted = dat_gpboost$Pred, Density = densities$pointdens)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_inla <- read_csv("Cross_Validation/Data_Outputs/INLA_10fold_CV.csv")
df <- data.frame(model = "INLA", Actual = dat_inla$Actual, Predicted = dat_inla$Pred, Density = densities$pointdens)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_frk <- read_csv("Cross_Validation/Data_Outputs/FRK_10_fold_CV.csv")
df <- data.frame(model = "FRK", Actual = dat_frk$Actual, Predicted = dat_frk$Pred, Density = densities$pointdens)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


# -----------------------------------------------------------------------
# TASK 8 : TABLE DATA - FOR DIFFERENT DENSITY POINTS - 50 FOLD
# -----------------------------------------------------------------------
densities <- read_csv("Cross_Validation/Data_Outputs/KDE_of_observations.csv")

# ---------------------------------------------------------------
# LOW DENSITIES
dat_rf <- read_csv("Cross_Validation/Data_Outputs/SpRF_50fold_CV.csv")
df <- data.frame(model = "SpRF", Actual = dat_rf$Actual, Predicted = dat_rf$Pred, Density = densities$pointdens )
inds <- which(df$Density <=0.2)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_gpboost <- read_csv("Cross_Validation/Data_Outputs/GPBoost_50_fold_CV.csv")
df <- data.frame(model = "GPBoost", Actual = dat_gpboost$Actual, Predicted = dat_gpboost$Pred, Density = densities$pointdens)
inds <- which(df$Density <=0.2)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_inla <- read_csv("Cross_Validation/Data_Outputs/INLA_50fold_CV.csv")
df <- data.frame(model = "INLA", Actual = dat_inla$Actual, Predicted = dat_inla$Pred, Density = densities$pointdens)
inds <- which(df$Density <=0.2)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_frk <- read_csv("Cross_Validation/Data_Outputs/FRK_50_fold_CV.csv")
df <- data.frame(model = "FRK", Actual = dat_frk$Actual, Predicted = dat_frk$Pred, Density = densities$pointdens)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


# ---------------------------------------------------------------
# MEDIUM DENSITIES
dat_rf <- read_csv("Cross_Validation/Data_Outputs/SpRF_50fold_CV.csv")
df <- data.frame(model = "SpRF", Actual = dat_rf$Actual, Predicted = dat_rf$Pred, Density = densities$pointdens )
inds <- which((df$Density >0.2) & (df$Density <=0.4))
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_gpboost <- read_csv("Cross_Validation/Data_Outputs/GPBoost_50_fold_CV.csv")
df <- data.frame(model = "GPBoost", Actual = dat_gpboost$Actual, Predicted = dat_gpboost$Pred, Density = densities$pointdens)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_inla <- read_csv("Cross_Validation/Data_Outputs/INLA_50fold_CV.csv")
df <- data.frame(model = "INLA", Actual = dat_inla$Actual, Predicted = dat_inla$Pred, Density = densities$pointdens)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_frk <- read_csv("Cross_Validation/Data_Outputs/FRK_50_fold_CV.csv")
df <- data.frame(model = "FRK", Actual = dat_frk$Actual, Predicted = dat_frk$Pred, Density = densities$pointdens)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


# ---------------------------------------------------------------
# HIGH DENSITIES
dat_rf <- read_csv("Cross_Validation/Data_Outputs/SpRF_50fold_CV.csv")
df <- data.frame(model = "SpRF", Actual = dat_rf$Actual, Predicted = dat_rf$Pred, Density = densities$pointdens )
inds <- which(df$Density > 0.4)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)



df <- data.frame(model = "GPBoost", Actual = dat_gpboost$Actual, Predicted = dat_gpboost$Pred, Density = densities$pointdens)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_inla <- read_csv("Cross_Validation/Data_Outputs/INLA_50fold_CV.csv")
df <- data.frame(model = "INLA", Actual = dat_inla$Actual, Predicted = dat_inla$Pred, Density = densities$pointdens)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)


dat_frk <- read_csv("Cross_Validation/Data_Outputs/FRK_50_fold_CV.csv")
df <- data.frame(model = "FRK", Actual = dat_frk$Actual, Predicted = dat_frk$Pred, Density = densities$pointdens)
round(sqrt(mean((df$Actual[inds] - df$Predicted[inds])^2)), 3)
round(cor(df$Actual[inds], df$Predicted[inds]), 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.05)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.1)/length(inds)*100, 3)
round(sum(abs(df$Actual[inds] - df$Predicted[inds]) <= 0.2)/length(inds)*100, 3)



# -----------------------------------------------------------------------
# TASK 9 : TABLE DATA - FOR INTERVALS - 10 FOLD
# -----------------------------------------------------------------------
dat_rf <- read_csv("Cross_Validation/Data_Outputs/SpRF_10fold_CV.csv")
df <- dat_rf
width <- df$sd
round(mean(width), 3)
round(sd(width), 3)
round(sum(df$within1SD)/length(df$within1SD)*100, 3)
round(sum(df$within2SD)/length(df$within2SD)*100, 3)
mean(df$Pred[df$band == "Out" & df$sd != 0])
sum(df$band == "Out")

dat_gpboost <- read_csv("Cross_Validation/Data_Outputs/GPBoost_10_fold_CV.csv")
df <- dat_gpboost
width <- df$sd
round(mean(width), 3)
round(sd(width), 3)
round(sum(df$within1SD)/length(df$within1SD)*100, 3)
round(sum(df$within2SD)/length(df$within2SD)*100, 3)


dat_inla <- read_csv("Cross_Validation/Data_Outputs/INLA_10fold_CV.csv")
df <- dat_inla
width <- df$sd
round(mean(width), 3)
round(sd(width), 3)
round(sum(df$within1SD)/length(df$within1SD)*100, 3)
round(sum(df$within2SD)/length(df$within2SD)*100, 3)


dat_frk <- read_csv("Cross_Validation/Data_Outputs/FRK_10_fold_CV.csv")
df <- dat_frk
width <- df$sd
round(mean(width), 3)
round(sd(width), 3)
round(sum(df$within1SD)/length(df$within1SD)*100, 3)
round(sum(df$within2SD)/length(df$within2SD)*100, 3)


# -----------------------------------------------------------------------
# TASK 10 : TABLE DATA - FOR INTERVALS - 50 FOLD
# -----------------------------------------------------------------------
dat_rf <- read_csv("Cross_Validation/Data_Outputs/SpRF_50fold_CV.csv")
df <- dat_rf
width <- df$sd
round(mean(width), 3)
round(sd(width), 3)
round(sum(df$within1SD)/length(df$within1SD)*100, 3)
round(sum(df$within2SD)/length(df$within2SD)*100, 3)


dat_gpboost <- read_csv("Cross_Validation/Data_Outputs/GPBoost_50_fold_CV.csv")
df <- dat_gpboost
width <- df$sd
round(mean(width), 3)
round(sd(width), 3)
round(sum(df$within1SD)/length(df$within1SD)*100, 3)
round(sum(df$within2SD)/length(df$within2SD)*100, 3)


dat_inla <- read_csv("Cross_Validation/Data_Outputs/INLA_50fold_CV.csv")
df <- dat_inla
width <- df$sd
round(mean(width), 3)
round(sd(width), 3)
round(sum(df$within1SD)/length(df$within1SD)*100, 3)
round(sum(df$within2SD)/length(df$within2SD)*100, 3)


dat_frk <- read_csv("Cross_Validation/Data_Outputs/FRK_50_fold_CV.csv")
df <- dat_frk
width <- df$sd
round(mean(width), 3)
round(sd(width), 3)
round(sum(df$within1SD)/length(df$within1SD)*100, 3)
round(sum(df$within2SD)/length(df$within2SD)*100, 3)


# -----------------------------------------------------------------------
# TASK 11 : GRAPHS FOR INTERVALS - 10 FOLDS
# -----------------------------------------------------------------------
library(ggplot2)
library(gridExtra)

# Common theme for plots
common_theme <- theme(axis.title=element_text(size=12),
                      legend.text=element_text(size=10))

# Only make the interval plots, not the second point plots
im_height = 8
im_width = 8
im_res = 300

dat_rf <- read_csv("Cross_Validation/Data_Outputs/SpRF_10fold_CV.csv")
df <- dat_rf



# Note we've been checking if points lie within 1SD of the mean, so I think
# it makes sense to have the mean rather than median prediction in the plot
g1 <- ggplot(df, aes(x = Actual, y = Mean, color = band, shape = band)) +
  geom_point() +
  geom_abline(slope = 1) +
  geom_errorbar(ymin = pmax(0, df$Mean - df$sd), ymax = pmin(1,df$Mean + df$sd)) +
  scale_color_manual(values = c("forestgreen", "blue", "red")) +
  theme_bw()  +
  theme(legend.position = "none") +
  common_theme +
  ylab("Prediction") +
  ylim(min(0, min(df$Mean)), min(1, max(df$Mean + df$sd)))
g1

# g2 <- ggplot(df, aes(x = Actual, y = Pred, color = band, shape = band)) +
#   geom_point() +
#   geom_abline(slope = 1) +
#   scale_color_manual(values = c("forestgreen", "blue", "red")) +
#   theme_bw() +
#   ylab("Predicted") +
#   common_theme +
#   ylim(min(0, min(df$Mean)), min(1, max(df$Mean + df$sd)))

png(filename = "Cross_Validation/Figures/SpRF_10_fold_intervals.png", units = "cm", width = im_width, height = im_height, res = im_res)
g1
dev.off()

dat_inla <- read_csv("Cross_Validation/Data_Outputs/INLA_10fold_CV.csv")
df <- dat_inla
g1 <- ggplot(df, aes(x = Actual, y = Mean, color = band, shape = band)) +
  geom_point() +
  geom_abline(slope = 1) +
  geom_errorbar(ymin = pmax(0, df$Mean - df$sd), ymax = pmin(1,df$Mean + df$sd)) +
  scale_color_manual(values = c("forestgreen", "blue", "red")) +
  theme_bw()  +
  common_theme +
  theme(legend.position = "none") +
  ylab("Prediction") +
  ylim(min(0, min(df$Mean)), min(1, max(df$Mean + df$sd)))
g1


# g2 <- ggplot(df, aes(x = Actual, y = Pred, color = band, shape = band)) +
#   geom_point() +
#   geom_abline(slope = 1) +
#   scale_color_manual(values = c("forestgreen", "blue", "red")) +
#   theme_bw() +
#   common_theme +
#   ylab("Predicted") +
#  ylim(min(0, min(df$Mean)), min(1, max(df$Mean + df$sd)))

png(filename = "Cross_Validation/Figures/INLA_10_fold_intervals.png", units = "cm", width = im_width, height = im_height, res = im_res)
g1
dev.off()

dat_gpboost <- read_csv("Cross_Validation/Data_Outputs/GPBoost_10_fold_CV.csv")
df <- dat_gpboost
g1 <- ggplot(df, aes(x = Actual, y = Pred, color = band, shape = band)) +
  geom_point() +
  geom_abline(slope = 1) +
  geom_errorbar(ymin = pmax(0, df$Pred - df$sd), ymax = pmin(1,df$Pred + df$sd)) +
  scale_color_manual(values = c("forestgreen", "blue", "red")) +
  theme_bw()  +
  common_theme +
  theme(legend.position = "none") +
  ylab("Prediction") +
  ylim(min(0, min(df$Pred)), min(1, max(df$Pred + df$sd)))
g1

# g2 <- ggplot(df, aes(x = Actual, y = Pred, color = band, shape = band)) +
#   geom_point() +
#   geom_abline(slope = 1) +
#   scale_color_manual(values = c("forestgreen", "blue", "red")) +
#   theme_bw() +
#   common_theme +
#   ylab("Predicted") +
#   ylim(min(0, min(df$Pred)), min(1, max(df$Pred + df$sd)))

png(filename = "Cross_Validation/Figures/GPBoost_10_fold_intervals.png", units = "cm", width = im_width, height = im_height, res = im_res)
g1
dev.off()



dat_frk <- read_csv("Cross_Validation/Data_Outputs/FRK_10_fold_CV.csv")
df <- dat_frk
g1 <- ggplot(df, aes(x = Actual, y = Pred, color = band, shape = band)) +
  geom_point() +
  geom_abline(slope = 1) +
  geom_errorbar(ymin = pmax(0, df$Pred - df$sd), ymax = pmin(1,df$Pred + df$sd)) +
  scale_color_manual(values = c("forestgreen", "blue", "red")) +
  theme_bw()  +
  common_theme +
  theme(legend.position = "none") +
  ylab("Prediction") +
  ylim(min(0, min(df$Pred)), min(1, max(df$Pred + df$sd)))
g1


# g2 <- ggplot(df, aes(x = Actual, y = Pred, color = band, shape = band)) +
#   geom_point() +
#   geom_abline(slope = 1) +
#   scale_color_manual(values = c("forestgreen", "blue", "red")) +
#   theme_bw() +
#   common_theme +
#   ylab("Predicted") +
#   ylim(min(0, min(df$Pred)), min(1, max(df$Pred + df$sd)))

png(filename = "Cross_Validation/Figures/FRK_10_fold_intervals.png", units = "cm", width = im_width, height = im_height, res = im_res)
g1
dev.off()


# -----------------------------------------------------------------------
# TASK 12 : GRAPHS FOR INTERVALS - 50 FOLDS
# -----------------------------------------------------------------------
# Common theme for plots
common_theme <- theme(axis.title=element_text(size=12),
                      legend.text=element_text(size=10))

# Only make the interval plots, not the second point plots
im_height = 8
im_width = 8
im_res = 300

dat_rf <- read_csv("Cross_Validation/Data_Outputs/SpRF_50fold_CV.csv")
df <- dat_rf

# Note we've been checking if points lie within 1SD of the mean, so I think
# it makes sense to have the mean rather than median prediction in the plot
g1 <- ggplot(df, aes(x = Actual, y = Mean, color = band, shape = band)) +
  geom_point() +
  geom_abline(slope = 1) +
  geom_errorbar(ymin = pmax(0, df$Mean - df$sd), ymax = pmin(1,df$Mean + df$sd)) +
  scale_color_manual(values = c("forestgreen", "blue", "red")) +
  theme_bw()  +
  theme(legend.position = "none") +
  common_theme +
  ylab("Prediction") +
  ylim(min(0, min(df$Mean)), min(1, max(df$Mean + df$sd)))
g1

# g2 <- ggplot(df, aes(x = Actual, y = Pred, color = band, shape = band)) +
#   geom_point() +
#   geom_abline(slope = 1) +
#   scale_color_manual(values = c("forestgreen", "blue", "red")) +
#   theme_bw() +
#   ylab("Predicted") +
#   common_theme +
#   ylim(min(0, min(df$Mean)), min(1, max(df$Mean + df$sd)))

png(filename = "Cross_Validation/Figures/SpRF_50_fold_intervals.png", units = "cm", width = im_width, height = im_height, res = im_res)
g1
dev.off()

dat_inla <- read_csv("Cross_Validation/Data_Outputs/INLA_50fold_CV.csv")
df <- dat_inla
g1 <- ggplot(df, aes(x = Actual, y = Mean, color = band, shape = band)) +
  geom_point() +
  geom_abline(slope = 1) +
  geom_errorbar(ymin = pmax(0, df$Mean - df$sd), ymax = pmin(1,df$Mean + df$sd)) +
  scale_color_manual(values = c("forestgreen", "blue", "red")) +
  theme_bw()  +
  common_theme +
  theme(legend.position = "none") +
  ylab("Prediction") +
  ylim(min(0, min(df$Mean)), min(1, max(df$Mean + df$sd)))
g1


# g2 <- ggplot(df, aes(x = Actual, y = Pred, color = band, shape = band)) +
#   geom_point() +
#   geom_abline(slope = 1) +
#   scale_color_manual(values = c("forestgreen", "blue", "red")) +
#   theme_bw() +
#   common_theme +
#   ylab("Predicted") +
#  ylim(min(0, min(df$Mean)), min(1, max(df$Mean + df$sd)))

png(filename = "Cross_Validation/Figures/INLA_50_fold_intervals.png", units = "cm", width = im_width, height = im_height, res = im_res)
g1
dev.off()

dat_gpboost <- read_csv("Cross_Validation/Data_Outputs/GPBoost_50_fold_CV.csv")
df <- dat_gpboost
g1 <- ggplot(df, aes(x = Actual, y = Pred, color = band, shape = band)) +
  geom_point() +
  geom_abline(slope = 1) +
  geom_errorbar(ymin = pmax(0, df$Pred - df$sd), ymax = pmin(1,df$Pred + df$sd)) +
  scale_color_manual(values = c("forestgreen", "blue", "red")) +
  theme_bw()  +
  common_theme +
  theme(legend.position = "none") +
  ylab("Prediction") +
  ylim(min(0, min(df$Pred)), min(1, max(df$Pred + df$sd)))
g1

# g2 <- ggplot(df, aes(x = Actual, y = Pred, color = band, shape = band)) +
#   geom_point() +
#   geom_abline(slope = 1) +
#   scale_color_manual(values = c("forestgreen", "blue", "red")) +
#   theme_bw() +
#   common_theme +
#   ylab("Predicted") +
#   ylim(min(0, min(df$Pred)), min(1, max(df$Pred + df$sd)))

png(filename = "Cross_Validation/Figures/GPBoost_50_fold_intervals.png", units = "cm", width = im_width, height = im_height, res = im_res)
g1
dev.off()



dat_frk <- read_csv("Cross_Validation/Data_Outputs/FRK_50_fold_CV.csv")
df <- dat_frk
g1 <- ggplot(df, aes(x = Actual, y = Pred, color = band, shape = band)) +
  geom_point() +
  geom_abline(slope = 1) +
  geom_errorbar(ymin = pmax(0, df$Pred - df$sd), ymax = pmin(1,df$Pred + df$sd)) +
  scale_color_manual(values = c("forestgreen", "blue", "red")) +
  theme_bw()  +
  common_theme +
  theme(legend.position = "none") +
  ylab("Prediction") +
  ylim(min(0, min(df$Pred)), min(1, max(df$Pred + df$sd)))
g1


# g2 <- ggplot(df, aes(x = Actual, y = Pred, color = band, shape = band)) +
#   geom_point() +
#   geom_abline(slope = 1) +
#   scale_color_manual(values = c("forestgreen", "blue", "red")) +
#   theme_bw() +
#   common_theme +
#   ylab("Predicted") +
#   ylim(min(0, min(df$Pred)), min(1, max(df$Pred + df$sd)))

png(filename = "Cross_Validation/Figures/FRK_50_fold_intervals.png", units = "cm", width = im_width, height = im_height, res = im_res)
g1
dev.off()


# -----------------------------------------------------------------------
# TASK 13 : EXPLORE INTERVAL WIDTH AND DENSITY - 10 FOLDS
# -----------------------------------------------------------------------
densities <- read_csv("Cross_Validation/Data_Outputs/KDE_of_observations.csv")
dens <- densities$pointdens

dat_rf <- read_csv("Cross_Validation/Data_Outputs/SpRF_10fold_CV.csv")
width <- dat_rf$sd
df <- data.frame(Model = "SpRF", Density = dens, Width = width)
dfall <- df

dat_gpboost <- read_csv("Cross_Validation/Data_Outputs/GPBoost_10_fold_CV.csv")
width <- dat_gpboost$sd
df <- data.frame(Model = "GPBoost", Density = dens, Width = width)
dfall <- bind_rows(dfall, df)

dat_inla <- read_csv("Cross_Validation/Data_Outputs/INLA_10fold_CV.csv")
width <- dat_inla$sd
df <- data.frame(Model = "INLA", Density = dens, Width = width)
dfall <- bind_rows(dfall, df)

dat_frk <- read_csv("Cross_Validation/Data_Outputs/FRK_10_fold_CV.csv")
width <- dat_frk$sd
df <- data.frame(Model = "FRK", Density = dens, Width = width)
dfall <- bind_rows(dfall, df)


foldsfile <- read.csv("Cross_Validation/Data_Outputs/Kmeans_10_clusters_as_folds.csv")
Fold <- foldsfile$clust
dfall <- dfall %>% bind_cols(Fold = rep(as.factor(Fold), 4))

dfall$Model <- factor(dfall$Model, levels = c("INLA","GPBoost","SpRF", "FRK"))

ggplot(dfall, aes(x = Density, y = Width, color = Fold)) +
  geom_point() +
  facet_wrap(~Model) +
  theme_bw()

# -----------------------------------------------------------------------
# TASK 14 : EXPLORE INTERVAL WIDTH AND DENSITY - 50 FOLDS
# -----------------------------------------------------------------------
densities <- read_csv("Cross_Validation/Data_Outputs/KDE_of_observations.csv")
dens <- densities$pointdens

dat_rf <- read_csv("Cross_Validation/Data_Outputs/SpRF_50fold_CV.csv")
width <- dat_rf$sd
df <- data.frame(Model = "SpRF", Density = dens, Width = width)
dfall <- df

dat_gpboost <- read_csv("Cross_Validation/Data_Outputs/GPBoost_50_fold_CV.csv")
width <- dat_gpboost$sd
df <- data.frame(Model = "GPBoost", Density = dens, Width = width)
dfall <- bind_rows(dfall, df)

dat_inla <- read_csv("Cross_Validation/Data_Outputs/INLA_50fold_CV.csv")
width <- dat_inla$sd
df <- data.frame(Model = "INLA", Density = dens, Width = width)
dfall <- bind_rows(dfall, df)

dat_frk <- read_csv("Cross_Validation/Data_Outputs/FRK_50_fold_CV.csv")
width <- dat_frk$sd
df <- data.frame(Model = "FRK", Density = dens, Width = width)
dfall <- bind_rows(dfall, df)


foldsfile <- read.csv("Cross_Validation/Data_Outputs/Kmeans_50_clusters_as_folds.csv")
Fold <- foldsfile$clust
dfall <- dfall %>% bind_cols(Fold = rep(as.factor(Fold), 4))

dfall$Model <- factor(dfall$Model, levels = c("INLA","GPBoost","SpRF", "FRK"))

ggplot(dfall, aes(x = Density, y = Width, color = Fold)) +
  geom_point() +
  facet_wrap(~Model) +
  theme_bw()
