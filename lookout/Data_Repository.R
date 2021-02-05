# ---------------------------------------------------------------------
# TASK 0 - LOAD LIBRARIES
# TASK 1 - GMEAN OF SENSITIVITY AND SPECIFICITY
# TASK 2 - FMEASURES
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# TASK 0 - LOAD LIBRARIES
# ---------------------------------------------------------------------
library(tidyr)
library(ggplot2)
library(lvplot)

# ---------------------------------------------------------------------
# TASK 1 - GMEAN OF SENSITIVITY AND SPECIFICITY
# ---------------------------------------------------------------------


dat <- read.csv("C:/Users/Sevvandi/Documents/repos/nlout/Supp_Mat/Data_Repository_Results.csv", sep="," )


# Differences - gmean
# G-Mean
colnums <- grep( "sensitivity" ,colnames(dat))
dat1 <- dat[ ,c(colnums)]
colnums <- grep( "specificity" ,colnames(dat))
dat2 <- dat[ ,c(colnums)]
gmean <- sqrt(dat1*dat2)


regobj <- regexpr("_", colnames(gmean)[1:3])
colnames(gmean) <- substring(colnames(gmean), 1, (regobj[1:3]-1) )

# Remove entries which are zero for all 3 algorithms
inds <- which(apply(gmean, 1, sum) ==0)
gmean <- gmean[-inds, ]
dat <- dat[-inds, ]


gmean2 <- cbind.data.frame(dat[ ,1], (gmean[ ,1] - gmean[ ,2]), (gmean[ ,1] - gmean[ ,3]) )
colnames(gmean2) <- c("filename", "look-hd", "look-stray")
dfl <- pivot_longer(gmean2, cols=2:3)
colnames(dfl)[2:3] <- c("Algorithm", "Gmean")


ggplot(dfl, aes(x=Algorithm, y=Gmean)) + geom_boxplot()

g1 <- ggplot(dfl, aes(x=Algorithm, y=Gmean)) + lvplot::geom_lv(aes(fill = ..LV..), width.method = "area", width=0.3)+ lvplot::scale_fill_lv()  + xlab("Algorithm") + ylab("Gmean")
g1

dim(gmean2)
dim(dat1)[1] - dim(gmean2)[1]
apply(gmean2[ ,-1], 2, median)


# lookout - HDoutliers
t.test(gmean2[ ,2])


# lookout - stray
t.test(gmean2[ ,3])

# ---------------------------------------------------------------------
# TASK 2 - FMEASURES
# ---------------------------------------------------------------------
dat <- read.csv("C:/Users/Sevvandi/Documents/repos/nlout/Supp_Mat/Data_Repository_Results.csv", sep="," )

# Precision
colnums <- grep( "true_pos" ,colnames(dat))
dat1 <- dat[ ,c(colnums)]

colnums <- grep( "false_pos" ,colnames(dat))
dat2 <- dat[ ,c(colnums)]

datsum <- dat1 + dat2
precision <- matrix(0, nrow=nrow(dat1), ncol=3)

for(i in 1:3){
  precision[ ,i] <- ifelse(datsum[ ,i] ==0, 0, dat1[ ,i]/datsum[ ,i])
}


# Recall
colnums <- grep( "true_pos" ,colnames(dat))
dat1 <- dat[ ,c(colnums)]

colnums <- grep( "false_neg" ,colnames(dat))
dat2 <- dat[ ,c(colnums)]

recall <- dat1/(dat1 + dat2 )


# F-measure
fmeasure <- function(precision, recall){
  if((precision==0) & (recall==0)){
    fmeas <- 0
  }else{
    fmeas <- 2*precision*recall/(precision + recall)
  }
  return(fmeas)
}

fmeas <- matrix(0, nrow=nrow(precision), ncol=3)
for(ll in 1:3){
  fmeas[ ,ll] <- mapply(fmeasure, precision[ ,ll], recall[ ,ll]) #
}

# Remove entries which have zeros for all 3 algorithms
inds <- which(apply(fmeas, 1, sum) ==0)
fmeas <- fmeas[-inds, ]
dat <- dat[-inds, ]


dd <- fmeas

regobj <- regexpr("_", colnames(dd)[1:3])
colnames(dd)[1:3] <- substring(colnames(dd)[1:3], 1, (regobj[1:3]-1) )
dat3 <- cbind.data.frame(dat[ ,1], (dd[ ,1] - dd[ ,2]), (dd[ ,1] - dd[ ,3]))
colnames(dat3) <- c("filename", "look-hd", "look-stray")


dfl <- pivot_longer(dat3, cols=2:3)
colnames(dfl)[2:3] <- c("Algorithm", "fmeas")


g2 <- ggplot(dfl, aes(x=Algorithm, y=fmeas)) + lvplot::geom_lv(aes(fill = ..LV..), width.method = "area", width = 0.2) + lvplot::scale_fill_lv()  + xlab("Algorithm") + ylab("F measure")
g2

gridExtra::grid.arrange(g1, g2)

apply(dat3[ ,-1], 2, median)

# lookout - HDoutliers
t.test(dat3[ ,2])

# lookout - stray
t.test(dat3[ ,3])
