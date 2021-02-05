# ---------------------------------------------------------------------
# TASK 0 - LOAD LIBRARIES
# TASK 1 - EXAMPLE 3
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# TASK 0 - LOAD LIBRARIES
# ---------------------------------------------------------------------
library(lookout)
library(HDoutliers)
library(stray)
library(ggplot2)
library(tidyr)


col_pal1 <- c("white", "#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026")

col_pal2 <- col_pal1 <- c("grey", "#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026")


diff_metrics <- function(act, pred){
  # positives to be denoted by 1 and negatives with 0
  n <- length(act)
  tp <- sum((act==1)&(pred==1))
  tn <- sum((act==0)&(pred==0))
  fp <- sum((act==0)&(pred==1))
  fn <- sum((act==1)&(pred==0))
  prec <- (tp+tn)/n

  sn <- tp/(tp + fn)
  sp <- tn/(tn + fp)

  if((tp+fp)==0){
    precision <- 0
  }else{
    precision <- tp/(tp+fp)
  }

  recall <- tp/(tp + fn)
  if((precision==0) & (recall==0)){
    fmeasure <- 0
  }else{
    fmeasure <- 2*precision*recall/(precision + recall)
  }



  gmean <- sqrt(sn*sp)

  out <- data.frame(N=n, true_pos = tp, true_neg= tn, false_pos = fp, false_neg= fn, accuracy = prec, sensitivity = sn, specificity = sp, gmean=gmean, precision=precision, recall=recall, fmeasure=fmeasure)

  return(out)
}


# ---------------------------------------------------------------------
# TASK 1 - EXAMPLE 3
# ---------------------------------------------------------------------
values <- rep(0, 10)
pp <- 10
dd <- 19
hdoutliers_gmean <- hdoutliers_fmeasure <- lookout_fmeasure <- lookout_gmean <- stray_gmean <- stray_fmeasure <- matrix(0, nrow=pp, ncol=20)

labs <- c(rep(0, 499), 1)
set.seed(123)
for(kk in 1:pp){
  X <- runif(500)
  for(j in 1:dd){
    X <- cbind(X, runif(500))
  }
  colnames(X) <- paste("x", 1:20, sep="")
  nn <- dim(X)[1]

  for(i in 1:20){
    X[500, 1:i] <- rep(0.9, i)
    # STRAY
    strayout <- stray::find_HDoutliers(X, knnsearchtype = "kd_tree", alpha=0.05)
    straylabs <- rep(0, nn)
    straylabs[strayout$outliers] <- 1
    strayoutput <- diff_metrics(labs, straylabs)
    stray_gmean[kk, i] <- strayoutput$gmean
    stray_fmeasure[kk, i] <- strayoutput$fmeasure


    # LOOKOUT
    lookoutobj <- lookout(X, alpha=0.05, unitize = TRUE)
    lookoutlabs <- rep(0, nn)
    lookoutlabs[lookoutobj$outliers[ ,1]] <- 1
    lookoutput <- diff_metrics(labs, lookoutlabs)
    lookout_gmean[kk, i] <- lookoutput$gmean
    lookout_fmeasure[kk, i] <- lookoutput$fmeasure


    # HDOUTLIERS
    hdoutobj <- HDoutliers(X, alpha=0.05)
    hdoutlabs <- rep(0, nn)
    hdoutlabs[hdoutobj] <- 1
    hdoutput <- diff_metrics(labs, hdoutlabs)
    hdoutliers_gmean[kk, i] <- hdoutput$gmean
    hdoutliers_fmeasure[kk, i] <- hdoutput$fmeasure

  }
}
# lookoutobj$bandwidth

outnew <- persisting_outliers(X)
g2 <- autoplot(outnew) + geom_vline(xintercept = outnew$lookoutbw, linetype="dashed")

lookobj <- lookout(X, alpha=0.1)
strength <- (0.1 - lookobj$outlier_probability)/0.01
strength[strength < 0] <- 0

# PLOT F-MEASURE
str_mean <- apply(stray_fmeasure, 2, mean)
# str_mean
lookout_mean <-  apply(lookout_fmeasure, 2, mean)
# lookout_mean
hdoutliers_mean <-  apply(hdoutliers_fmeasure, 2, mean)
# hdoutliers_mean

df <- cbind.data.frame(1:20, str_mean, lookout_mean, hdoutliers_mean)
colnames(df) <- c("Iteration", "stray", "lookout", "HDoutliers")
dfl <- pivot_longer(df, cols=2:4)
colnames(dfl)[2] <- "Method"
g3 <- ggplot(dfl, aes(Iteration, value)) + geom_line(aes(color=Method), size=1) + ylab("Fmeasure")  + scale_x_continuous(breaks=2*(1:10)) + theme_bw() + theme(legend.position = "none") # + theme(axis.text.x = element_text(angle = 45))
# g3

# PLOT GEOMETRIC MEAN OF SENSITIVITY AND SPECIFICITY
str_mean <- apply(stray_gmean, 2, mean)
# str_mean
lookout_mean <-  apply(lookout_gmean, 2, mean)
# lookout_mean
hdoutliers_mean <-  apply(hdoutliers_gmean, 2, mean)
# hdoutliers_mean

df <- cbind.data.frame(1:20, str_mean, lookout_mean, hdoutliers_mean)
colnames(df) <- c("Iteration", "stray", "lookout", "HDoutliers")
dfl <- pivot_longer(df, cols=2:4)
colnames(dfl)[2] <- "Method"
g4 <- ggplot(dfl, aes(Iteration, value)) + geom_line(aes(color=Method), size=1) + ylab("Gmean") + scale_x_continuous(breaks=2*(1:10)) + theme_bw() #+  theme(axis.text.x = element_text(angle = 45))
# g4

gridExtra::grid.arrange(
  g2, g3, g4,
  ncol = 2, nrow = 2,
  layout_matrix = rbind(c(1,1), c(2,3)), widths=c(1, 1.5))
