irt_ensemble <- function(X){
  dd <- dim(X)[2]

  # normalize data to [0, 1]
  maxs <- apply(X, 2, max)
  mins <- apply(X, 2, min)
  divs <- maxs - mins
  X <- as.data.frame(X)
  X <- sweep(X, 2, mins)
  X <- sweep(X, 2, divs, "/")

  modout <- airt::cirtmodel(X, max.item = rep(1,dd), min.item = rep(0, dd))
  obj <- EstCRM::EstCRMperson(X,modout$model$param, min.item = rep(0,dd), max.item = rep(1,dd) )

  Z <- obj$thetas[ ,1:2]
  Z[ ,2] <- Z[ ,2] - min(Z[ ,2])
  colnames(Z)[2] <- "Ensemble_Score"

  colnames(modout$model$param) <- c("alpha", "beta", "gamma")

  out <- list()
  out$scores <- Z[ ,2]
  out$model <- modout$model

  return(out)
}


greedy_ensemble <- function(X, kk=5){
  # CODED FROM On Evaluation of Outlier Rankings and Outlier Scores
  # BY Erich Schubert, Remigius Wojdanowski, Arthur Zimek and Hans-Peter Kriegel
  # SECTION 4.3 GREEDY ENSEMBLE

  # normalize data to [0, 1]
  maxs <- apply(X, 2, max)
  mins <- apply(X, 2, min)
  divs <- maxs - mins
  X <- as.data.frame(X)
  X <- sweep(X, 2, mins)
  X <- sweep(X, 2, divs, "/")

  dd <- dim(X)[2]
  nn <- dim(X)[1]
  # CONSTRUCT THE TARGET VECTOR
  tgt <- c()
  for(i in 1:dd){
    xx <- X[ ,i]
    inds <- order(xx, decreasing = TRUE)[1:kk]
    tgt <- c(tgt, inds)
  }
  tgt <- unique(tgt)
  target <- rep(0, nn)
  target[tgt] <- 1

  # COMPUTE WEIGHTS FOR CORRELATION
  wts <- rep(1/(2*(nn - kk)), nn)
  wts[target==1] <- 1/(2*kk)
  cw <- rep(0, dd)
  for(ll in 1:dd){
    Z <- cbind(X[ ,ll], target)
    obj <- psych::cor.wt(Z, w=wts)
    cw[ll] <- obj$r[1,2]
  }

  # INITIALIZE ENSEMBLE WITH METHOD THAT HAS THE HIGHEST CORRELATION WITH TARGET
  ens <- X[ ,which.max(cw)]
  methods <- which.max(cw)
  ens_corr <- max(cw)
  cws <- order(cw)
  # Remove the last one as it is already in the ensemble
  cws <- cws[-length(cws)]

  # TEST IF ENSEMBLE CAN BE IMPROVED BY INCREASING THE CORRELATION WITH THE TARGET VECTOR BY INCLUDING THE NEXT METHOD FROM THE SORTED LIST
  for(jj in 1:length(cws)){
    ens_pr <- cbind(ens, X[ ,cws[jj]])
    score <- apply(ens_pr, 1, mean)
    Z <- cbind(score, target)
    obj <- psych::cor.wt(Z, w=wts)
    cor_val <- obj$r[1,2]
    if(cor_val > ens_corr){
      ens <- cbind(ens, X[ ,cws[jj]])
      methods <- c(methods, cws[jj])
    }
  }
  ens <- as.data.frame(ens)

  # OUTPUT
  out <- list()
  out$scores <- apply(ens, 1, mean)
  out$methods <- methods
  out$chosen <- X[ ,methods]
  return(out)
}


icwa_ensemble <- function(X){

  # normalize data to [0, 1]
  maxs <- apply(X, 2, max)
  mins <- apply(X, 2, min)
  divs <- maxs - mins
  X <- as.data.frame(X)
  X <- sweep(X, 2, mins)
  X <- sweep(X, 2, divs, "/")

  # Y is normalized to 0 - 1
  cory <- stats::cor(X)
  apcl <- apcluster::apcluster(cory)
  # apcl@clusters
  # colnames(cory)
  numclust <- length(apcl@clusters) # gives clusters

  clusts <- c()
  for(jj in 1:numclust){
    clusts <- c(clusts, length(apcl@clusters[[jj]]))
  }
  icwa <- rep(0, dim(X)[1])
  for(kk in 1:numclust){
    if(clusts[kk]>1){
      icwa_temp <- apply(X[ ,apcl@clusters[[kk]]], 1, mean)
    }else{
      icwa_temp <- X[ ,apcl@clusters[[kk]]]
    }
    icwa <- icwa + icwa_temp
  }
  icwa <- icwa/sum(1/clusts)
  return(icwa)
}


max_ensemble <- function(X){
  # normalize data to [0, 1]
  maxs <- apply(X, 2, max)
  mins <- apply(X, 2, min)
  divs <- maxs - mins
  X <- as.data.frame(X)
  X <- sweep(X, 2, mins)
  X <- sweep(X, 2, divs, "/")

  Y <- apply(X, 1, max)
  return(Y)
}


threshold_ensemble <- function(X){
  # normalize data to [0, 1]
  maxs <- apply(X, 2, max)
  mins <- apply(X, 2, min)
  divs <- maxs - mins
  X <- as.data.frame(X)
  X <- sweep(X, 2, mins)
  X <- sweep(X, 2, divs, "/")

  xmean <- apply(X, 2, mean)
  B <- apply(X, 1, function(x) x < xmean)
  B <- t(B)
  X[B==TRUE] <- 0
  Y <- apply(X, 1, sum)
  return(Y)
}


unitize <- function(X) {
  for (col in 1:NCOL(X)) {
    maxcol <- max(X[, col])
    mincol <- min(X[, col])
    if(maxcol!=mincol){
      X[, col] <- (X[, col] - mincol) / (maxcol - mincol)
    }
  }
  X
}

