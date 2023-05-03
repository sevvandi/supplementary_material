# From Task 15: Make 10 and 50 folds using K means clustering for cross validation
# Use only the pf data
# -----------------------------------------------------------------------
library(rgdal)
library(leaflet)
library(dplyr)
library(ggplot2)

# Set working directory to the "Kenya" directory
setwd()

dat <- readRDS("Data_Inputs/Kenya_pf_pr_2009.rds")
dat <- dat %>% mutate(x = longitude, y = latitude)
zdf <- data.frame(x = dat$x, y = dat$y)
coordinates(zdf) = ~x+y # change into an sp object

proj4string(zdf) <- CRS("+proj=longlat +datum=WGS84")
UTM.kenya <- spTransform(zdf, CRS("+init=epsg:32737"))
xx <- data.frame(x = UTM.kenya$x, y = UTM.kenya$y)

set.seed(1)
kmeansobj <- kmeans(xx, centers = 10, iter.max = 10, nstart = 5)
ggplot(xx, aes(x, y)) + geom_point(aes(color = as.factor(kmeansobj$cluster)))

dat_all <- cbind.data.frame(dat, cluster = as.factor(kmeansobj$cluster))
library(RColorBrewer)
pal <- brewer.pal(10, "Spectral") # topo.colors(10)

leaflet(dat_all) %>%  addProviderTiles(providers$OpenStreetMap) %>%
  addCircles(lng = ~longitude, lat = ~latitude, color = pal[as.factor(kmeansobj$cluster)] )

clust_10 <- data.frame(clust = kmeansobj$cluster)
write.csv(clust_10, "Cross_Validation/Data_Outputs/Kmeans_10_clusters_as_folds.csv", row.names = FALSE)


set.seed(1)
kmeansobj <- kmeans(xx, centers = 50, iter.max = 10, nstart = 5)
ggplot(xx, aes(x, y)) + geom_point(aes(color = as.factor(kmeansobj$cluster)))

clust_50 <- data.frame(clust = kmeansobj$cluster)
write.csv(clust_50, "Cross_Validation/Data_Outputs/Kmeans_50_clusters_as_folds.csv", row.names = FALSE)

