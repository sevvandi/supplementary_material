#   THIS FILE CONTAINS EXPERIMENTS
# ---------------------------------------------------------------------------
# TASK 1  : EXAMPLE - ANNULUS WITH OUTLIERS IN THE MIDDLE
# TASK 2  : EXPERIMENT 1 - TWO NORMAL DISTRIBUTIONS, ONE MOVING AWAY BIT BY BIT
# TASK 3  : EXPERIMENT 2 - ANNULUS - ONE DISTRIBUTION MOVING IN
# TASK 4  : EXPERIMENT 3 - 3 NORMAL DISTRIBUTIONS - BIMODAL -  WITH ONE MOVING INTO THE TROUGH
# TASK 5  : EXPERIMENT 4 - EXPLORE TIME COMPLEXITY
# ---------------------------------------------------------------------------
source("ML_AD_Methods.R")

# ---------------------------------------------------------------------------
# TASK 1 : ANNULUS WITH OUTLIERS IN THE MIDDLE
# ---------------------------------------------------------------------------
library(outlierensembles)
library(ggplot2)
library(tidyr)
library(pROC)
library(gridExtra)
library(grid)
library(e1071)
library(h2o)
library(stray)
library(here)

set.seed(1)
r1 <-runif(803)
r2 <-rnorm(803, mean=5)
theta = 2*pi*r1;
R1 <- 2
R2 <- 2
dist = r2+R2;
x =  dist * cos(theta)
y =  dist * sin(theta)

X <- data.frame(
  x1 = x,
  x2 = y
)
labs <- c(rep(0,800), rep(1,3))
ensemblescores <- matrix(0, nrow=nrow(X), ncol=8)

nn <- dim(X)[1]
knn_auc <- lof_auc <- cof_auc <- rep(0, 10)
mu <-  0
z <- cbind(rnorm(3,mu, sd=0.2), rnorm(3,0, sd=0.2))
X[801:803, 1:2] <- z
ggplot(X, aes(x1, x2)) + geom_point()


y1 <- DDoutlier::KNN_AGG(X,  k_min=10, k_max=20)
y2 <- DDoutlier::LOF(X, k=10)
y3 <- DDoutlier::COF(X, k=10)
y4 <- DDoutlier::INFLO(X, k=10)
y5 <- DDoutlier::KDEOS(X, k_min=10, k_max=20)
y6 <- DDoutlier::LDF(X, k=10)
y7 <- DDoutlier::LDOF(X, k=10)

# stray
y8 <- stray::find_HDoutliers(X)

# autoencoder scores
autoencsc <- autoencoder_AD2(X)

# SVM scores
svmsc <- svm_AD(X)
svmsc <- as.vector(svmsc)

# isolation forest scores
isosc <- isolation_AD(X)

Y <- cbind.data.frame(y1, y2, y3, y4, y5, y6$LDF, y7, y8$out_scores,  autoencsc, svmsc, isosc)
ens1 <- outlierensembles::irt_ensemble(Y)
df <- cbind.data.frame(X, ens1$scores)
colnames(df)[3] <- "IRT"
ggplot(df, aes(x1, x2)) + geom_point(aes(color=IRT))  +  scale_color_gradient(low="yellow", high="red")
ensemblescores[ ,1] <- ens1$scores

# greedy 1
ens2 <- outlierensembles::greedy_ensemble(Y, kk=3)
df <- cbind.data.frame(X, ens2$scores)
colnames(df)[3] <- "Greedy"
ggplot(df, aes(x1, x2)) + geom_point(aes(color=Greedy)) +  scale_color_gradient(low="yellow", high="red")
ensemblescores[ ,2] <- ens2$scores

# greedy 2
ens21 <- outlierensembles::greedy_ensemble(Y, kk=10)
df <- cbind.data.frame(X, ens21$scores)
colnames(df)[3] <- "Greedy"
ggplot(df, aes(x1, x2)) + geom_point(aes(color=Greedy)) +  scale_color_gradient(low="yellow", high="red")
ensemblescores[ ,8] <- ens21$scores


# greedy mean
scores <- matrix(0, nrow= nrow(X), ncol=6)
for(ll in 1:10){
  ens2m <- outlierensembles::greedy_ensemble(Y, kk=ll)
  scores[ ,(ll-4)] <- ens2m$scores
}
meanscores <- apply(scores, 1, mean)
df <- cbind.data.frame(X, meanscores)
colnames(df)[3] <- "GreedyAvg"
ggplot(df, aes(x1, x2)) + geom_point(aes(color=GreedyAvg)) +  scale_color_gradient(low="yellow", high="red")
ensemblescores[ ,3] <- meanscores


# ICWA
ens3 <- outlierensembles::icwa_ensemble(Y)
df <- cbind.data.frame(X, ens3)
colnames(df)[3] <- "ICWA"
ggplot(df, aes(x1, x2)) + geom_point(aes(color=ICWA)) +  scale_color_gradient(low="yellow", high="red")
ensemblescores[ ,4] <- ens3


# Max
ens4 <- outlierensembles::max_ensemble(Y)
df <- cbind.data.frame(X, ens4)
colnames(df)[3] <- "Max"
ggplot(df, aes(x1, x2)) + geom_point(aes(color=Max)) +  scale_color_gradient(low="yellow", high="red")
ensemblescores[ ,5] <- ens4


# threshold sum
ens5 <- outlierensembles::threshold_ensemble(Y)
df <- cbind.data.frame(X, ens5)
colnames(df)[3] <- "Threshold"
ggplot(df, aes(x1, x2)) + geom_point(aes(color=Threshold)) +  scale_color_gradient(low="yellow", high="red")
ensemblescores[ ,6] <- ens5


# average
ens6 <- outlierensembles::average_ensemble(Y)
df <- cbind.data.frame(X, ens6)
colnames(df)[3] <- "Average"
ggplot(df, aes(x1, x2)) + geom_point(aes(color=Average)) +  scale_color_gradient(low="yellow", high="red")
ensemblescores[ ,7] <- ens6


# NORMALIZING ENSEMBLE SCORES TO THE INTERVAL [0,1]
maxs <- apply(ensemblescores, 2, max)
mins <- apply(ensemblescores, 2, min)
divs <- maxs - mins
ensemblescores <- as.data.frame(ensemblescores)
ensemblescores <- sweep(ensemblescores, 2, mins)
ensemblescores <- sweep(ensemblescores, 2, divs, "/")
colnames(ensemblescores) <- c("IRT", "Greedy1", "GreedyAvg", "ICWA", "Max", "Thresh", "Average", "Greedy2")


df <- cbind.data.frame(X, ensemblescores)
dflong <- pivot_longer(df, cols=3:10)
colnames(dflong)[3:4] <- c("Algo", "Score")
ggplot(dflong, aes(x1,x2)) + geom_point(aes(color=Score)) + facet_wrap(~Algo) +   scale_color_gradient(low="yellow", high="red") + theme_bw()


# THE POINT APPROX AT (5, 7.5)
ind <- which((X$x1>5)&(X$x2>7.5))
maxs <- apply(Y, 2, max)
mins <- apply(Y, 2, min)
divs <- maxs - mins
Y2 <- as.data.frame(Y)
Y2 <- sweep(Y2, 2, mins)
Y2 <- sweep(Y2, 2, divs, "/")
Y2[ind, ]



full_mat <- round(ens1$model$param, 1)


# -------------------------------------------------------------------------
# TASK 2  : EXPERIMENT 1 - TWO NORMAL DISTRIBUTIONS, ONE MOVING AWAY BIT BY BIT
# -------------------------------------------------------------------------
library(pROC)
library(ggplot2)
library(tidyr)
library(outlierensembles)
library(gridExtra)
library(grid)
library(e1071)
library(h2o)

values <- rep(0, 10)
pp <- 10

autoenc_all <- iso_all <- svm_all <- matrix(0, nrow=pp, ncol=10)

thres_ens <- icwa_ens <- max_ens <- gr_ens_mean <- gr_ens <- averages <- irt_ensemble <-  matrix(0, nrow=pp, ncol=10)
labs <- c(rep(0,400), rep(1,5))
set.seed(123)
for(kk in 1:pp){ # pp
  x2 <- rnorm(405)
  x3 <- rnorm(405)
  x4 <- rnorm(405)
  x5 <- rnorm(405)
  x6 <- rnorm(405)
  x1_1 <- rnorm(400)

  for(i in 1:10){
    mu2 <- 2+(i-1)*0.5
    x1_2 <- rnorm(5, mean=mu2, sd=0.2)
    x1 <- c(x1_1, x1_2)
    X <- cbind(x1,x2,x3,x4,x5,x6)


    # DDoutlier scores
    knnsc <- DDoutlier::KNN_AGG(X)
    lofsc <- DDoutlier::LOF(X)
    cofsc <- DDoutlier::COF(X)
    inflosc <- DDoutlier::INFLO(X)
    kdeossc<- DDoutlier::KDEOS(X)
    ldfsc <- DDoutlier::LDF(X)$LDF
    ldofsc <- DDoutlier::LDOF(X)

    # STRAY
    str <- stray::find_HDoutliers(X)

    # autoencoder scores
    autoencsc <- autoencoder_AD2(X)

    # SVM scores
    svmsc <- svm_AD(X)
    svmsc <- as.vector(svmsc)

    # isolation forest scores
    isosc <- isolation_AD(X)

    Y <- cbind(knnsc, lofsc, cofsc, inflosc, kdeossc, ldfsc, ldofsc, str$out_scores, autoencsc, svmsc, isosc)

    maxs <- apply(Y, 2, max)
    mins <- apply(Y, 2, min)
    divs <- maxs - mins
    Y <- as.data.frame(Y)
    Yori <- Y
    Y <- sweep(Y, 2, mins, FUN="-")
    Y <- sweep(Y, 2, divs, FUN="/")

    # IRT Ensemble
    modout <- outlierensembles::irt_ensemble(Y)
    irt_ens_scores <- modout$scores
    roc_obj <- pROC::roc(labs, irt_ens_scores, direction = "<")
    irt_ensemble[kk, i] <- roc_obj$auc


    # Taking the mean
    Ymean <- apply(Y, 1, mean)
    roc_obj <- pROC::roc(labs, Ymean, direction = "<")
    averages[kk, i] <- roc_obj$auc

    # Greedy Ensemble - Schubert et al
    gr_obj <- outlierensembles::greedy_ensemble(Y, kk=5)
    gr_score <- gr_obj$scores
    roc_obj <- pROC::roc(labs, gr_score, direction = "<")
    gr_ens[kk, i] <- roc_obj$auc

    # Greedy Ensemble mean - Schubert et al
    gr_score_one <- matrix(0, nrow=dim(Y)[1], ncol=10)
    for(jj in 1:10){
      gr_obj <- outlierensembles::greedy_ensemble(Y, kk=jj)
      gr_score_one[ ,jj] <- gr_obj$scores

    }
    gr_score_mean <- apply(gr_score_one, 1, mean)
    roc_obj <- pROC::roc(labs, gr_score_mean, direction = "<")
    gr_ens_mean[kk, i] <- roc_obj$auc

    # ICWA Ensemble
    icwa_score <- outlierensembles::icwa_ensemble(Y)
    roc_obj <- pROC::roc(labs, icwa_score, direction = "<")
    icwa_ens[kk, i] <- roc_obj$auc

    # MAX Ensemble
    max_score <- outlierensembles::max_ensemble(Y)
    roc_obj <- pROC::roc(labs, max_score, direction = "<")
    max_ens[kk, i] <- roc_obj$auc

    # Threshold Ensemble
    thres_score <- outlierensembles::threshold_ensemble(Y)
    roc_obj <- pROC::roc(labs, thres_score, direction = "<")
    thres_ens[kk, i] <- roc_obj$auc

    # ----- STORE ISOLATION FOREST, AUTOENCODERS AND SVM OUTPUT
    # AUTOENCODER
    roc_obj <- pROC::roc(labs, autoencsc, direction = "<")
    autoenc_all[kk, i] <- roc_obj$auc

    # SVM
    roc_obj <- pROC::roc(labs, svmsc, direction = "<")
    svm_all[kk, i] <- roc_obj$auc

    # ISOLATION FOREST
    roc_obj <- pROC::roc(labs, isosc, direction = "<")
    iso_all[kk, i] <- roc_obj$auc

  }
}

colnames(full_mat) <- paste(colnames(full_mat), "_EX0", sep="")
ex1mat <-  round(modout$model$param, 1)
colnames(ex1mat) <- paste(colnames(ex1mat), "_EX1", sep="")
irtparas <- cbind.data.frame(full_mat, ex1mat)
rownames(irtparas) <- rownames(round(modout$model$param, 1))
write.csv(irtparas, "Data_Output/IRT_Parameters.csv", row.names = FALSE)

t.test(as.vector(irt_ensemble - averages))
t.test(as.vector(irt_ensemble - gr_ens))
t.test(as.vector(irt_ensemble - gr_ens_mean))
t.test(as.vector(irt_ensemble - icwa_ens))
t.test(as.vector(irt_ensemble - max_ens))
t.test(as.vector(irt_ensemble - thres_ens))

t.test(as.vector(irt_ensemble - autoenc_all))
t.test(as.vector(irt_ensemble - svm_all))
t.test(as.vector(irt_ensemble - iso_all))

irt_ensemble_mean <- apply(irt_ensemble, 2, mean)
averages_mean <- apply(averages, 2, mean)
greedy_mean_several_k <- apply(gr_ens_mean, 2, mean)
greedy_mean_one_k <- apply(gr_ens, 2, mean)
icwa_mean <- apply(icwa_ens, 2, mean)
max_mean <- apply(max_ens, 2, mean)
thres_mean <- apply(thres_ens, 2, mean)

autoenc_mean <- apply(autoenc_all, 2, mean)
svm_mean <- apply(svm_all, 2, mean)
iso_mean <- apply(iso_all, 2, mean)

df <- cbind.data.frame(1:10, irt_ensemble_mean, averages_mean, greedy_mean_several_k, greedy_mean_one_k, icwa_mean, max_mean, thres_mean)
colnames(df) <- c("Iteration", "IRT", "Average", "Greedy_Avg", "Greedy", "ICWA", "Max", "Thresh")


# PLOTTING ------------------------------------------------------
# AUC values
colnum <- dim(df)[2]
dfl <- pivot_longer(df, cols=2:colnum)
colnames(dfl)[2] <- "Method"
gperf <- ggplot(dfl, aes(Iteration, value)) + geom_line(aes(color=Method), size=1.2) + ylab("AUC") + scale_x_discrete(limits = 1:10) + theme_bw()
gperf

dfl1 <- dfl

# ---------------- iteration 10 & 5
set.seed(123)
x2 <- rnorm(405)
x3 <- rnorm(405)
x4 <- rnorm(405)
x5 <- rnorm(405)
x6 <- rnorm(405)
x1_1 <- rnorm(400)
i <- 10
mu2 <- 2+(i-1)*0.5
x1_2 <- rnorm(5, mean=mu2, sd=0.2)
x1 <- c(x1_1, x1_2)
X <- cbind.data.frame(x1,x2,x3,x4,x5,x6)

anomaly <- as.factor(labs)
X <- cbind.data.frame(X, anomaly)
g1 <- ggplot(X, aes(x1, x2, color= anomaly )) + geom_point() + theme_bw()

i <- 5
mu2 <- 2+(i-1)*0.5
x1_2 <- rnorm(5, mean=mu2, sd=0.2)
x1 <- c(x1_1, x1_2)
X <- cbind.data.frame(x1,x2,x3,x4,x5,x6)


g2 <- ggplot(X, aes(x1, x2, color= anomaly )) + geom_point() + theme_bw() +  theme(legend.position = "none")

myplot1 <- gridExtra::arrangeGrob(g1, bottom = textGrob("(b)", x = unit(0.4, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))
myplot2 <- gridExtra::arrangeGrob(g2, bottom = textGrob("(a)", x = unit(0.5, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))
myplot3 <- gridExtra::arrangeGrob(gperf, bottom = textGrob("(c)", x = unit(0.4, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))


gridExtra::grid.arrange(myplot2, myplot1, myplot3, nrow=2, layout_matrix= rbind(c(1,2), c(3,3)), widths=c(1,1.5))


# -------------------------------------------------------------------------
# TASK 3  : EXPERIMENT 2 - ANNULUS - ONE DISTRIBUTION MOVING IN
# -------------------------------------------------------------------------
values <- rep(0, 10)
pp <- 10

autoenc_all <- iso_all <- svm_all <- matrix(0, nrow=pp, ncol=10)

thres_ens <- icwa_ens <- max_ens <- gr_ens_mean <- gr_ens <- averages <- irt_ensemble <-  matrix(0, nrow=pp, ncol=10)
labs <- c(rep(0,800), rep(1,5))

set.seed(123)
for(kk in 1:pp){
  r1 <-runif(805)
  r2 <-rnorm(805, mean=5)
  theta = 2*pi*r1;
  R1 <- 2
  R2 <- 2
  dist = r2+R2;
  x =  dist * cos(theta)
  y =  dist * sin(theta)

  X <- data.frame(
    x1 = x,
    x2 = y,
    x3 <- rnorm(805),
    x4 <- rnorm(805)
  )
  labs <- c(rep(0,800), rep(1,5))
  nn <- dim(X)[1]

  for(i in 1:10){
    mu2 <-  5 - (i-1)*0.5
    z <- cbind(rnorm(5,mu2, sd=0.1), rnorm(5,0, sd=0.1))

    X[801:805, 1:2] <- z

    # DDoutlier scores
    knnsc <- DDoutlier::KNN_AGG(X)
    lofsc <- DDoutlier::LOF(X)
    cofsc <- DDoutlier::COF(X)
    inflosc <- DDoutlier::INFLO(X)
    kdeossc<- DDoutlier::KDEOS(X)
    ldfsc <- DDoutlier::LDF(X)$LDF
    ldofsc <- DDoutlier::LDOF(X)

    # STRAY
    str <- stray::find_HDoutliers(X)

    # autoencoder scores
    autoencsc <- autoencoder_AD2(X)

    # SVM scores
    svmsc <- svm_AD(X)
    svmsc <- as.vector(svmsc)

    # isolation forest scores
    isosc <- isolation_AD(X)

    Y <- cbind(knnsc, lofsc, cofsc, inflosc, kdeossc, ldfsc, ldofsc, str$out_scores, autoencsc, svmsc, isosc)

    maxs <- apply(Y, 2, max)
    mins <- apply(Y, 2, min)
    divs <- maxs - mins
    Y <- as.data.frame(Y)
    Y <- sweep(Y, 2, mins)
    Y <- sweep(Y, 2, divs, "/")

    # IRT Ensemble
    modout <- outlierensembles::irt_ensemble(Y)
    irt_ens_scores <- modout$scores
    roc_obj <- pROC::roc(labs, irt_ens_scores, direction = "<")
    irt_ensemble[kk, i] <- roc_obj$auc

    # Taking the mean
    Ymean <- apply(Y, 1, mean)
    roc_obj <- pROC::roc(labs, Ymean, direction = "<")
    averages[kk, i] <- roc_obj$auc

    # Greedy Ensemble - Schubert et al
    gr_obj <- outlierensembles::greedy_ensemble(Y, kk=5)
    gr_score <- gr_obj$scores
    roc_obj <- pROC::roc(labs, gr_score, direction = "<")
    gr_ens[kk, i] <- roc_obj$auc

    # Greedy Ensemble mean - Schubert et al
    gr_score_one <- matrix(0, nrow=dim(Y)[1], ncol=10)
    for(jj in 1:10){
      gr_obj <- outlierensembles::greedy_ensemble(Y, kk=jj)
      gr_score_one[ ,jj] <- gr_obj$scores

    }
    gr_score_mean <- apply(gr_score_one, 1, mean)
    roc_obj <- pROC::roc(labs, gr_score_mean, direction = "<")
    gr_ens_mean[kk, i] <- roc_obj$auc

    # ICWA Ensemble
    icwa_score <- outlierensembles::icwa_ensemble(Y)
    roc_obj <- pROC::roc(labs, icwa_score, direction = "<")
    icwa_ens[kk, i] <- roc_obj$auc

    # MAX Ensemble
    max_score <- outlierensembles::max_ensemble(Y)
    roc_obj <- pROC::roc(labs, max_score, direction = "<")
    max_ens[kk, i] <- roc_obj$auc

    # Threshold Ensemble
    thres_score <- outlierensembles::threshold_ensemble(Y)
    roc_obj <- pROC::roc(labs, thres_score, direction = "<")
    thres_ens[kk, i] <- roc_obj$auc

    # ----- STORE ISOLATION FOREST, AUTOENCODERS AND SVM OUTPUT
    # AUTOENCODER
    roc_obj <- pROC::roc(labs, autoencsc, direction = "<")
    autoenc_all[kk, i] <- roc_obj$auc

    # SVM
    roc_obj <- pROC::roc(labs, svmsc, direction = "<")
    svm_all[kk, i] <- roc_obj$auc

    # ISOLATION FOREST
    roc_obj <- pROC::roc(labs, isosc, direction = "<")
    iso_all[kk, i] <- roc_obj$auc

  }
}

ex2_paras <- round(modout$model$param, 1)
colnames(ex2_paras) <- paste(colnames(ex2_paras), "_EX2", sep="")
irtparas <- cbind(irtparas, ex2_paras)
write.csv(irtparas, "Data_Output/IRT_Parameters.csv", row.names = FALSE)

t.test(as.vector(irt_ensemble - averages))
t.test(as.vector(irt_ensemble - gr_ens))
t.test(as.vector(irt_ensemble - gr_ens_mean))
t.test(as.vector(irt_ensemble - icwa_ens))
t.test(as.vector(irt_ensemble - max_ens))
t.test(as.vector(irt_ensemble - thres_ens))

t.test(as.vector(irt_ensemble - autoenc_all))
t.test(as.vector(irt_ensemble - svm_all))
t.test(as.vector(irt_ensemble - iso_all))

irt_ensemble_mean <- apply(irt_ensemble, 2, mean)
averages_mean <- apply(averages, 2, mean)
greedy_mean_several_k <- apply(gr_ens_mean, 2, mean)
greedy_mean_one_k <- apply(gr_ens, 2, mean)
icwa_mean <- apply(icwa_ens, 2, mean)
max_mean <- apply(max_ens, 2, mean)
thres_mean <- apply(thres_ens, 2, mean)

autoenc_mean <- apply(autoenc_all, 2, mean)
svm_mean <- apply(svm_all, 2, mean)
iso_mean <- apply(iso_all, 2, mean)

df <- cbind.data.frame(1:10, irt_ensemble_mean, averages_mean, greedy_mean_several_k, greedy_mean_one_k, icwa_mean, max_mean, thres_mean)
colnames(df) <- c("Iteration", "IRT",  "Average", "Greedy_Avg", "Greedy", "ICWA", "Max", "Thresh")


# PLOTTING --------------------------------------------
colnum <- dim(df)[2]
dfl <- pivot_longer(df, cols=2:colnum)
colnames(dfl)[2] <- "Method"
gperf <- ggplot(dfl, aes(Iteration, value)) + geom_line(aes(color=Method), size=1.2) + ylab("AUC") + scale_x_discrete(limits = 1:10) + theme_bw()
gperf
dfl2 <- dfl

# ---------------- iteration 10 & 5
set.seed(123)
r1 <-runif(805)
r2 <-rnorm(805, mean=5)
theta = 2*pi*r1;
R1 <- 2
R2 <- 2
dist = r2+R2;
x =  dist * cos(theta)
y =  dist * sin(theta)

X <- data.frame(
  x1 = x,
  x2 = y,
  x3 <- rnorm(805),
  x4 <- rnorm(805)
)
anomaly <- as.factor(c(rep(0,800), rep(1,5)))
nn <- dim(X)[1]

i <- 10
mu2 <-  5 - (i-1)*0.5
z <- cbind(rnorm(5,mu2, sd=0.1), rnorm(5,0, sd=0.1))
X[801:805, 1:2] <- z

g2 <- ggplot(X, aes(x1, x2, color= anomaly )) + geom_point() + theme_bw()
g2

i <- 5
mu2 <-  5 - (i-1)*0.5
z <- cbind(rnorm(5,mu2, sd=0.1), rnorm(5,0, sd=0.1))
X[801:805, 1:2] <- z


g1 <- ggplot(X, aes(x1, x2, color= anomaly )) + geom_point() + theme_bw() +  theme(legend.position = "none")
g1

myplot1 <- gridExtra::arrangeGrob(g1, bottom = textGrob("(a)", x = unit(0.55, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))
myplot2 <- gridExtra::arrangeGrob(g2, bottom = textGrob("(b)", x = unit(0.4, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))
myplot3 <- gridExtra::arrangeGrob(gperf, bottom = textGrob("(c)", x = unit(0.4, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))


gridExtra::grid.arrange(myplot1, myplot2, myplot3, nrow=2, layout_matrix= rbind(c(1,2), c(3,3)), widths=c(1,1.3), heights=c(1,1.5))
# 500 375 - dims


# --------------------------------------------------------------------------
# TASK 3  : 3 NORMAL DISTRIBUTIONS - BIMODAL -  WITH ONE MOVING INTO THE TROUGH
# --------------------------------------------------------------------------

pp <- 10

autoenc_all <- iso_all <- svm_all <- matrix(0, nrow=pp, ncol=10)

thres_ens <- icwa_ens <- max_ens <- gr_ens_mean <- gr_ens <- averages <- irt_ensemble <-  matrix(0, nrow=pp, ncol=10)

set.seed(123)
for(kk in 1:pp){
  X <- data.frame(
    x1 = c(rnorm(405,mean=5), rnorm(400, mean=-5)),
    x2 = rnorm(805),
    x3 = rnorm(805),
    x4 = rnorm(805),
    x5 = rnorm(805),
    x6 = rnorm(805)
  )
  labs <- c(rep(0,400), rep(1,5), rep(0,400))

  for(i in 1:10){
    mu2 <-  3 - (i)*0.3
    x1_2 <- rnorm(5, mean=mu2, sd=0.2)
    X[401:405, 1] <- x1_2

    # DDoutlier scores
    knnsc <- DDoutlier::KNN_AGG(X)
    lofsc <- DDoutlier::LOF(X)
    cofsc <- DDoutlier::COF(X)
    inflosc <- DDoutlier::INFLO(X)
    kdeossc<- DDoutlier::KDEOS(X)
    ldfsc <- DDoutlier::LDF(X)$LDF
    ldofsc <- DDoutlier::LDOF(X)

    # STRAY
    str <- stray::find_HDoutliers(X)

    # autoencoder scores
    autoencsc <- autoencoder_AD2(X)

    # SVM scores
    svmsc <- svm_AD(X)
    svmsc <- as.vector(svmsc)

    # isolation forest scores
    isosc <- isolation_AD(X)

    Y <- cbind(knnsc, lofsc, cofsc, inflosc, kdeossc, ldfsc, ldofsc, str$out_scores, autoencsc, svmsc, isosc)

    maxs <- apply(Y, 2, max)
    mins <- apply(Y, 2, min)
    divs <- maxs - mins
    Y <- as.data.frame(Y)
    Y <- sweep(Y, 2, mins)
    Y <- sweep(Y, 2, divs, "/")

    # IRT Ensemble
    modout <- outlierensembles::irt_ensemble(Y)
    irt_ens_scores <- modout$scores
    roc_obj <- pROC::roc(labs, irt_ens_scores, direction = "<")
    irt_ensemble[kk, i] <- roc_obj$auc


    # Taking the mean
    Ymean <- apply(Y, 1, mean)
    roc_obj <- pROC::roc(labs, Ymean, direction = "<")
    averages[kk, i] <- roc_obj$auc

    # Greedy Ensemble - Schubert et al
    gr_obj <- outlierensembles::greedy_ensemble(Y, kk=5)
    gr_score <- gr_obj$scores
    roc_obj <- pROC::roc(labs, gr_score, direction = "<")
    gr_ens[kk, i] <- roc_obj$auc

    # Greedy Ensemble mean - Schubert et al
    gr_score_one <- matrix(0, nrow=dim(Y)[1], ncol=10)
    for(jj in 1:10){
      gr_obj <- outlierensembles::greedy_ensemble(Y, kk=jj)
      gr_score_one[ ,jj] <- gr_obj$scores

    }
    gr_score_mean <- apply(gr_score_one, 1, mean)
    roc_obj <- pROC::roc(labs, gr_score_mean, direction = "<")
    gr_ens_mean[kk, i] <- roc_obj$auc

    # ICWA Ensemble
    icwa_score <- outlierensembles::icwa_ensemble(Y)
    roc_obj <- pROC::roc(labs, icwa_score, direction = "<")
    icwa_ens[kk, i] <- roc_obj$auc

    # MAX Ensemble
    max_score <- outlierensembles::max_ensemble(Y)
    roc_obj <- pROC::roc(labs, max_score, direction = "<")
    max_ens[kk, i] <- roc_obj$auc

    # Threshold Ensemble
    thres_score <- outlierensembles::threshold_ensemble(Y)
    roc_obj <- pROC::roc(labs, thres_score, direction = "<")
    thres_ens[kk, i] <- roc_obj$auc

    # ----- STORE ISOLATION FOREST, AUTOENCODERS AND SVM OUTPUT
    # AUTOENCODER
    roc_obj <- pROC::roc(labs, autoencsc, direction = "<")
    autoenc_all[kk, i] <- roc_obj$auc

    # SVM
    roc_obj <- pROC::roc(labs, svmsc, direction = "<")
    svm_all[kk, i] <- roc_obj$auc

    # ISOLATION FOREST
    roc_obj <- pROC::roc(labs, isosc, direction = "<")
    iso_all[kk, i] <- roc_obj$auc

  }
}

round(modout$model$param, 1)
ex3_paras <- round(modout$model$param, 1)
colnames(ex3_paras) <- paste(colnames(ex3_paras), "_EX3", sep="")
irtparas <- cbind(irtparas, ex3_paras)
write.csv(irtparas, "Data_Output/IRT_Parameters.csv", row.names = FALSE)

t.test(as.vector(irt_ensemble - averages))
t.test(as.vector(irt_ensemble - gr_ens))
t.test(as.vector(irt_ensemble - gr_ens_mean))
t.test(as.vector(irt_ensemble - icwa_ens))
t.test(as.vector(irt_ensemble - max_ens))
t.test(as.vector(irt_ensemble - thres_ens))


irt_ensemble_mean <- apply(irt_ensemble, 2, mean)
averages_mean <- apply(averages, 2, mean)
greedy_mean_several_k <- apply(gr_ens_mean, 2, mean)
greedy_mean_one_k <- apply(gr_ens, 2, mean)
icwa_mean <- apply(icwa_ens, 2, mean)
max_mean <- apply(max_ens, 2, mean)
thres_mean <- apply(thres_ens, 2, mean)

df <- cbind.data.frame(1:10, irt_ensemble_mean,  averages_mean, greedy_mean_several_k, greedy_mean_one_k, icwa_mean, max_mean, thres_mean)
colnames(df) <- c("Iteration", "IRT", "Average", "Greedy_Avg", "Greedy", "ICWA", "Max", "Thresh")


# PLOTTING ------------------------------------------
colnum <- dim(df)[2]
dfl <- pivot_longer(df, cols=2:colnum)
colnames(dfl)[2] <- "Method"
gperf <- ggplot(dfl, aes(Iteration, value)) + geom_line(aes(color=Method), size=1.2) + ylab("AUC") + scale_x_discrete(limits = 1:10) + theme_bw()
dfl3 <- dfl
gperf


# ---------------- iteration 10 & 5
set.seed(123)
X <- data.frame(
  x1 = c(rnorm(405,mean=5), rnorm(400, mean=-5)),
  x2 = rnorm(805),
  x3 = rnorm(805),
  x4 = rnorm(805),
  x5 = rnorm(805),
  x6 = rnorm(805)
)
labs <- c(rep(0,400), rep(1,5), rep(0,400))
anomaly <- as.factor(labs)
i <- 10
mu2 <-  3 - (i-1)*0.3
x1_2 <- rnorm(5, mean=mu2, sd=0.2)
X[401:405, 1] <- x1_2

g2 <- ggplot(X, aes(x1, x2, color= anomaly )) + geom_point() + theme_bw()
g2


i <- 5
mu2 <-  3 - (i-1)*0.3
x1_2 <- rnorm(5, mean=mu2, sd=0.2)
X[401:405, 1] <- x1_2

g1 <- ggplot(X, aes(x1, x2, color= anomaly )) + geom_point() + theme_bw() +  theme(legend.position = "none")
g1

myplot1 <- gridExtra::arrangeGrob(g1, bottom = textGrob("(a)", x = unit(0.52, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))
myplot2 <- gridExtra::arrangeGrob(g2, bottom = textGrob("(b)", x = unit(0.39, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))
myplot3 <- gridExtra::arrangeGrob(gperf, bottom = textGrob("(c)", x = unit(0.4, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))


gridExtra::grid.arrange(myplot1, myplot2, myplot3, nrow=2, layout_matrix= rbind(c(1,2), c(3,3)), widths=c(1,1.3), heights=c(1,1.5))
# 500 375 - dims


# ---------------------------------------------------------------------------
# TASK 5  : EXPERIMENT 4 - EXPLORE TIME COMPLEXITY
# ---------------------------------------------------------------------------

df2 <- read.csv(paste(here(), "/anomalyensembles/Time_Taken_for_Experiments.csv", sep=""))

x <- df2$N*df2$iter*df2$n
y <- df2$V1

df3 <- cbind.data.frame(x, y)
ggplot(df3, aes(x, y)) + geom_point() + geom_smooth(method="lm",formula=y~x,  se=FALSE) + xlab(TeX("$N\\times n\\times e$")) + ylab("Time taken (s)") + theme_bw()

