# --------------------------------------------------------------------------------------
# TASK 0         : LOAD LIBRARIES AND FUNCTIONS
# EXPERIMENT  1  : SIMULATE ERDOS RENYI GRAPHS AND CHECK IF YOU GET THE ANOMALIES
# EXPERIMENT  2  : SIMULATE ERDOS RENYI GRAPHS AND CHECK IF YOU GET THE ANOMALIES - CHANGING PARAMETERS
# EXPERIMENT  3  : SIMULATE BARABASSI MODEL - PREFERRENTIAL ATTACHMENT - CHANGING PARAMETERS
# EXPERIMENT  3  : SIMULATE WATTS-STROGATZ SMALL WORLD MODEL- CHANGING PARAMETERS
# --------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------
# TASK 0         : LOAD LIBRARIES AND FUNCTIONS
# --------------------------------------------------------------------------------------
library(dplyr)
library(igraph)
library(lookout)
library(fable)
library(tidyr)
library(pcaPP)
library(oddnet)
library(pROC)
library(ggplot2)
library(latex2exp)
library(rTensor)
library(DDoutlier)

# Tensorsplat algorithm
# Koutra, D., Papalexakis, E. E. & Faloutsos, C. (2012), Tensorsplat: Spotting latent anomalies in time,
# in ‘Proceedings of the 2012 16th Panhellenic Conference on Informatics, PCI 2012’, pp. 144–149.

tensorsplat <- function(matlist, k = 5, alpha = 0.05 ){
  nn <- dim(matlist[[1]])[1]
  for(i in 1:length(matlist)){
    numrows <- dim(matlist[[i]])[1]
    nn <- max(nn, numrows)
  }

  indices <- c(length(matlist), nn, nn)
  arr <- array(0, dim = indices)
  for(i in 1:length(matlist)){
    numrows <- dim(matlist[[i]])[1]
    arr[i, 1:numrows, 1:numrows] <- as.matrix(matlist[[i]])
  }
  network_tensor <- rTensor::as.tensor(arr);

  cp_decomp <- rTensor::cp(network_tensor, num_components=1)
  temp_factors <- cp_decomp$U[[1]]
  lofout <- DDoutlier::LOF(temp_factors, k)
  alphath <- ceiling(length(lofout)*alpha)
  ord <- order(lofout, decreasing = TRUE)[1:alphath]

  tnspt <- list()
  tnspt$outliers <- ord
  tnspt$scores <- lofout
  tnspt
}

# LAD algorithm
# Huang, S., Hitti, Y., Rabusseau, G. & Rabbany, R. (2020), Laplacian change point detection for dynamic
# graphs, in ‘Proceedings of the ACM SIGKDD International Conference on Knowledge Discovery
# and Data Mining’, pp. 349–358.

context_matrix <- function(matlist, k = NULL){
  len <- length(matlist)
  if(is.null(k)){
    k <- min(10, ceiling(dim(matlist[[1]])[1]/2))
  }
  context <- matrix(0, nrow = k, ncol = len)
  for(ii in 1:len){
    adjacency <- matlist[[ii]]
    gr <- igraph::graph_from_adjacency_matrix(adjacency)
    laplacian <- igraph::laplacian_matrix(gr)
    svdout <- svd(laplacian, nu = k, nv = k)
    singvals <- svdout$d
    context[ ,ii] <- singvals[1:k]/(sqrt(sum(singvals[1:k]^2)))
  }
  context
}


lad_scores <- function(context, win_width ){
  timepoints <- dim(context)[2]
  num_times <- timepoints - win_width
  st <- 1
  en <- win_width
  scores <- rep(0, num_times)
  for(ii in 1:num_times){
    window <- context[ ,st:en]
    svdobj <- svd(window, nu = 1, nv = 1)
    norm_vec <- svdobj$u[ ,1]
    curr_vec <- context[ ,(en+1)]
    scores[ii] <- 1 - abs(sum(norm_vec*curr_vec)/sqrt(sum(norm_vec^2)*sum(curr_vec^2)))
    st <- st + 1
    en <- en + 1
  }
  scores
}


lad <- function(matlist, k = NULL, short_win, long_win, alpha = 0.05, from_file = NULL){
  # compute the context matrix
  if(!is.null(from_file)){
    filedat <- read.csv(from_file, header = FALSE)
    context <- t(filedat)
  }else{
    context <- context_matrix(matlist, k)
  }

  # computer short term scores
  anomaly_scores_short <- lad_scores(context, short_win)
  anomaly_scores_short <- c(rep(0, short_win), anomaly_scores_short)
  anomaly_scores_short <- c(0, diff(anomaly_scores_short))
  anomaly_scores_short[anomaly_scores_short < 0 ] <- 0

  anomaly_scores_long <- lad_scores(context, long_win)
  anomaly_scores_long <- c(rep(0, long_win), anomaly_scores_long)
  anomaly_scores_long <- c(0, diff(anomaly_scores_long))
  anomaly_scores_long[anomaly_scores_long < 0] <- 0

  alphath_pos <- ceiling(length(matlist)/20)
  anomalies_short <- (order(anomaly_scores_short, decreasing = TRUE))[1:alphath_pos]
  anomalies_long <- (order(anomaly_scores_long, decreasing = TRUE))[1:alphath_pos]

  structure(list(
    short_scores = anomaly_scores_short,
    long_scores = anomaly_scores_long,
    short_anomalies = anomalies_short,
    long_anomalies = anomalies_long,
    call = match.call()
  ), class='lad')

}


# --------------------------------------------------------------------------------------
# EXPERIMENT  1  : SIMULATE ERDOS RENYI GRAPHS AND CHECK IF YOU GET THE ANOMALIES
# --------------------------------------------------------------------------------------

set.seed(2022)
outp <- c(0.1, 0.15, 0.2, 0.25)
num_reps <- 10
num_networks <- 100
anom_lad <- anom_tens <-anom_oddnet <- matrix(0, ncol = 4, nrow = length(outp)*num_reps )

networks <- list()

for(kkk in 1:4){
  outpval <- outp[kkk]
  for(lll in 1:num_reps){
    mat <- matrix(0, nrow = num_networks, ncol = 24)
    p.or.m.seq <- rep(0.05, 100)
    p.or.m.seq[50] <- outpval
    labs <- rep(0, num_networks)
    labs[50] <- 1

    for(i in 1:num_networks){
      gr <- erdos.renyi.game(100, p.or.m = p.or.m.seq[i])
      networks[[i]] <- igraph::as_adjacency_matrix(gr)
    }
    preds_tens <- preds_lad <- preds_oddnet <- rep(0, num_networks)

    # Anomalies using oddnet
    oddnetobj <- anomalous_networks(networks, fast = TRUE)
    preds_oddnet[oddnetobj$outliers[ ,1]] <- 1
    oddnetscores <-1- oddnetobj$outlier_probability
    # ROC Values
    rocobj <- pROC::roc(labs, oddnetscores, direction = "<")
    oddnet_roc <- rocobj$auc

    # Anomalies using Tensorsplat
    tensorobj <- tensorsplat(networks)
    preds_tens[tensorobj$outliers] <- 1
    tnsptscores <- tensorobj$scores
    # ROC Values
    rocobj <- pROC::roc(labs, tnsptscores, direction = "<")
    tnspt_roc <- rocobj$auc

    # Anomalies using LAD
    ladobj <- lad(networks, short_win = 1, long_win = 2)
    preds_lad[ladobj$long_anomalies] <- 1
    ladscores <- ladobj$long_scores
    # ROC Values
    rocobj <- pROC::roc(labs, ladscores, direction = "<")
    lad_roc <- rocobj$auc


    mmm <- lll + (kkk-1)*num_reps
    anom_oddnet[mmm, ] <- c(kkk, lll, outpval, oddnet_roc)
    anom_lad[mmm, ] <- c(kkk, lll, outpval, lad_roc)
    anom_tens[mmm, ] <- c(kkk, lll, outpval,  tnspt_roc)
  }
}

colnames(anom_tens) <- colnames(anom_lad) <- colnames(anom_oddnet) <- c("kkk", "lll", "parameter", "ROCAUC")

df1 <- as.data.frame(anom_oddnet) %>%
  select(parameter, ROCAUC) %>%
  mutate(algorithm = "oddnet")
df2 <- as.data.frame(anom_lad) %>%
  select(parameter, ROCAUC) %>%
  mutate(algorithm = "LAD")
df3 <- as.data.frame(anom_tens) %>%
  select(parameter, ROCAUC) %>%
  mutate(algorithm = "Tensorsplat")

df <- bind_rows(df1, df2, df3)
ggplot(df, aes(as.factor(parameter), ROCAUC)) +
  geom_boxplot() +
  facet_wrap(~algorithm) +
  theme_bw() +
  geom_point() +
  xlab("Anomalous Network Edge Probability") +
  ylab("AUC")


# --------------------------------------------------------------------------------------
# EXPERIMENT  2  : SIMULATE ERDOS RENYI GRAPHS AND CHECK IF YOU GET THE ANOMALIES - CHANGING PARAMETERS
# --------------------------------------------------------------------------------------

set.seed(2022)
outp <- c(0.05, 0.1, 0.15, 0.2)
num_reps <- 10
num_networks <- 100
anom_lad <- anom_tens <-anom_oddnet <- matrix(0, ncol = 4, nrow = length(outp)*num_reps )

networks <- list()

for(kkk in 1:4){
  outpval <- outp[kkk]
  for(lll in 1:num_reps){
    mat <- matrix(0, nrow = num_networks, ncol = 24)
    p.or.m.seq <- seq(from = 0.05, to = 0.5, length.out = 100)
    p.or.m.seq[50] <- p.or.m.seq[50] + outpval
    labs <- rep(0, num_networks)
    labs[50] <- 1

    for(i in 1:num_networks){
      gr <- erdos.renyi.game(100, p.or.m = p.or.m.seq[i])
      networks[[i]] <- igraph::as_adjacency_matrix(gr)
    }
    preds_tens <- preds_lad <- preds_oddnet <- rep(0, num_networks)

    # Anomalies using oddnet
    oddnetobj <- anomalous_networks(networks, fast = TRUE)
    preds_oddnet[oddnetobj$outliers[ ,1]] <- 1
    oddnetscores <-1- oddnetobj$outlier_probability
    # ROC Values
    rocobj <- pROC::roc(labs, oddnetscores, direction = "<")
    oddnet_roc <- rocobj$auc

    # Anomalies using Tensorsplat
    tensorobj <- tensorsplat(networks)
    preds_tens[tensorobj$outliers] <- 1
    tnsptscores <- tensorobj$scores
    # ROC Values
    rocobj <- pROC::roc(labs, tnsptscores, direction = "<")
    tnspt_roc <- rocobj$auc

    # Anomalies using LAD
    ladobj <- lad(networks, short_win = 1, long_win = 2)
    preds_lad[ladobj$long_anomalies] <- 1
    ladscores <- ladobj$long_scores
    # ROC Values
    rocobj <- pROC::roc(labs, ladscores, direction = "<")
    lad_roc <- rocobj$auc

    mmm <- lll + (kkk-1)*num_reps
    anom_oddnet[mmm, ] <- c(kkk, lll, outpval, oddnet_roc)
    anom_lad[mmm, ] <- c(kkk, lll, outpval, lad_roc)
    anom_tens[mmm, ] <- c(kkk, lll, outpval,  tnspt_roc)
  }
}


colnames(anom_tens) <- colnames(anom_lad) <- colnames(anom_oddnet) <- c("kkk", "lll", "parameter", "ROCAUC")

df1 <- as.data.frame(anom_oddnet) %>%
  select(parameter, ROCAUC) %>%
  mutate(algorithm = "oddnet")
df2 <- as.data.frame(anom_lad) %>%
  select(parameter, ROCAUC) %>%
  mutate(algorithm = "LAD")
df3 <- as.data.frame(anom_tens) %>%
  select(parameter, ROCAUC) %>%
  mutate(algorithm = "Tensorsplat")

df <- bind_rows(df1, df2, df3)
ggplot(df, aes(as.factor(parameter), ROCAUC)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~algorithm) +
  theme_bw() +
  xlab(TeX("Anomalous Network Parameter $p_*$")) +
  ylab("AUC")

# --------------------------------------------------------------------------------------
# EXPERIMENT  3  : SIMULATE BARABASSI MODEL - PREFERRENTIAL ATTACHMENT - CHANGING PARAMETERS
# --------------------------------------------------------------------------------------


set.seed(2022)
outp <-  c(0.25, 0.3, 0.35, 0.4)
num_reps <- 10
num_networks <- 100
anom_lad <- anom_tens <-anom_oddnet <- matrix(0, ncol = 4, nrow = length(outp)*num_reps )

networks <- list()
for(kkk in 1:4){
  outpval <- outp[kkk]
  for(lll in 1:num_reps){
    mat <- matrix(0, nrow = num_networks, ncol = 24)
    power_seq <- seq(from = 1.1, to = 1.9, length.out = 100)
    power_seq[50] <- power_seq[50] + outpval
    labs <- rep(0, num_networks)
    labs[50] <- 1

    for(i in 1:num_networks){
      gr <- igraph::sample_pa(100, power = power_seq[i], directed = FALSE, m = 5)
      networks[[i]] <- igraph::as_adjacency_matrix(gr)
    }
    preds_tens <- preds_lad <- preds_oddnet <- rep(0, num_networks)

    # Anomalies using oddnet
    oddnetobj <- anomalous_networks(networks, fast = TRUE)
    preds_oddnet[oddnetobj$outliers[ ,1]] <- 1
    oddnetscores <-1- oddnetobj$outlier_probability
    # ROC Values
    rocobj <- pROC::roc(labs, oddnetscores, direction ="<")
    oddnet_roc <- rocobj$auc

    # Anomalies using Tensorsplat
    tensorobj <- tensorsplat(networks)
    preds_tens[tensorobj$outliers] <- 1
    tnsptscores <- tensorobj$scores
    # ROC Values
    rocobj <- pROC::roc(labs, tnsptscores, direction ="<")
    tnspt_roc <- rocobj$auc

    # Anomalies using LAD
    ladobj <- lad(networks, short_win = 1, long_win = 2)
    preds_lad[ladobj$long_anomalies] <- 1
    ladscores <- ladobj$long_scores
    # ROC Values
    rocobj <- pROC::roc(labs, ladscores, direction ="<")
    lad_roc <- rocobj$auc


    mmm <- lll + (kkk-1)*num_reps
    anom_oddnet[mmm, ] <- c(kkk, lll, outpval, oddnet_roc)
    anom_lad[mmm, ] <- c(kkk, lll, outpval, lad_roc)
    anom_tens[mmm, ] <- c(kkk, lll, outpval, tnspt_roc)
  }
}

colnames(anom_tens) <- colnames(anom_lad) <- colnames(anom_oddnet) <- c("kkk", "lll", "parameter", "ROCAUC")

df1 <- as.data.frame(anom_oddnet) %>%
  select(parameter, ROCAUC) %>%
  mutate(algorithm = "oddnet")
df2 <- as.data.frame(anom_lad) %>%
  select(parameter, ROCAUC) %>%
  mutate(algorithm = "LAD")
df3 <- as.data.frame(anom_tens) %>%
  select(parameter, ROCAUC) %>%
  mutate(algorithm = "Tensorsplat")

df <- bind_rows(df1, df2, df3)
ggplot(df, aes(as.factor(parameter), ROCAUC)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~algorithm) +
  theme_bw() +
  xlab(TeX("Anomalous Network Parameter $p_*$")) +
  ylab("AUC")


# --------------------------------------------------------------------------------------
# EXPERIMENT  4  : SIMULATE WATTS-STROGATZ SMALL WORLD MODEL- CHANGING PARAMETERS
# --------------------------------------------------------------------------------------

set.seed(2022)
outp <- c(0.05, 0.1, 0.15, 0.2)
num_reps <- 10
num_networks <- 100
anom_lad <- anom_tens <-anom_oddnet <- matrix(0, ncol = 4, nrow = length(outp)*num_reps )

networks <- list()

for(kkk in 1:4){
  outpval <- outp[kkk]
  for(lll in 1:num_reps){
    mat <- matrix(0, nrow = num_networks, ncol = 24)
    p.or.m.seq <- seq(from = 0.05, to = 0.3, length.out = 100)
    p.or.m.seq[50] <- p.or.m.seq[50] + outpval
    labs <- rep(0, num_networks)
    labs[50] <- 1

    for(i in 1:num_networks){
      gr <- igraph::sample_smallworld(1, 100, 5, p.or.m.seq[i])
      networks[[i]] <- igraph::as_adjacency_matrix(gr)
    }
    preds_tens <- preds_lad <- preds_oddnet <- rep(0, num_networks)

    # Anomalies using oddnet
    oddnetobj <- anomalous_networks(networks, fast = TRUE)
    preds_oddnet[oddnetobj$outliers[ ,1]] <- 1
    oddnetscores <-1- oddnetobj$outlier_probability
    # ROC Values
    rocobj <- pROC::roc(labs, oddnetscores, direction ="<")
    oddnet_roc <- rocobj$auc

    # Anomalies using Tensorsplat
    tensorobj <- tensorsplat(networks)
    preds_tens[tensorobj$outliers] <- 1
    tnsptscores <- tensorobj$scores
    # ROC Values
    rocobj <- pROC::roc(labs, tnsptscores, direction ="<")
    tnspt_roc <- rocobj$auc

    # Anomalies using LAD
    ladobj <- lad(networks, short_win = 1, long_win = 2)
    preds_lad[ladobj$long_anomalies] <- 1
    ladscores <- ladobj$long_scores
    # ROC Values
    rocobj <- pROC::roc(labs, ladscores, direction ="<")
    lad_roc <- rocobj$auc


    mmm <- lll + (kkk-1)*num_reps
    anom_oddnet[mmm, ] <- c(kkk, lll, outpval, oddnet_roc)
    anom_lad[mmm, ] <- c(kkk, lll, outpval, lad_roc)
    anom_tens[mmm, ] <- c(kkk, lll, outpval,  tnspt_roc)
  }
}


colnames(anom_tens) <- colnames(anom_lad) <- colnames(anom_oddnet) <- c("kkk", "lll", "parameter", "ROCAUC")

df1 <- as.data.frame(anom_oddnet) %>%
  select(parameter, ROCAUC) %>%
  mutate(algorithm = "oddnet")
df2 <- as.data.frame(anom_lad) %>%
  select(parameter, ROCAUC) %>%
  mutate(algorithm = "LAD")
df3 <- as.data.frame(anom_tens) %>%
  select(parameter, ROCAUC) %>%
  mutate(algorithm = "Tensorsplat")

df <- bind_rows(df1, df2, df3)
ggplot(df, aes(as.factor(parameter), ROCAUC)) +
  facet_wrap(~algorithm) +
  geom_boxplot() +
  geom_point() +
  theme_bw() +
  xlab(TeX("Anomalous Network Parameter $p_*$")) +
  ylab("AUC")
