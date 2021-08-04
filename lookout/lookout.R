## ----setup, include=FALSE------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE,
  cache = TRUE,
  messages = FALSE,
  warning = FALSE
)
library(tidyverse)
library(HDoutliers)
library(stray)
library(lookout)
library(lvplot)
# remotes::install_github("rrrlw/ggtda")
library(ggtda)
library(ggforce)
library(RANN)
library(TDAstats)
library(patchwork)
library(ggrepel)
library(latex2exp)
library(DDoutlier)
library(pROC)
library(R.matlab)

set.seed(2021)

diff_metrics <- function(act, pred) {
  # positives to be denoted by 1 and negatives with 0
  stopifnot(length(act) == length(pred))
  n <- length(act)
  tp <- sum((act == 1) & (pred == 1))
  tn <- sum((act == 0) & (pred == 0))
  fp <- sum((act == 0) & (pred == 1))
  fn <- sum((act == 1) & (pred == 0))
  prec <- (tp + tn) / n
  sn <- tp / (tp + fn)
  sp <- tn / (tn + fp)
  precision <- if_else(
    (tp + fp) == 0,
    0,
    tp / (tp + fp)
  )
  recall <- tp / (tp + fn)
  fmeasure <- if_else(
    (precision == 0) & (recall == 0),
    0,
    2 * precision * recall / (precision + recall)
  )
  tibble(
    N = n,
    true_pos = tp,
    true_neg = tn,
    false_pos = fp,
    false_neg = fn,
    accuracy = prec,
    sensitivity = sn,
    specificity = sp,
    gmean = sqrt(sn * sp),
    precision = precision,
    recall = recall,
    fmeasure = fmeasure
  )
}

col_pal2 <- col_pal1 <- c(
  "grey", "#ffffcc", "#ffeda0", "#fed976", "#feb24c",
  "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026"
)

diff_metrics_2 <- function(act, pred){
  # positives to be denoted by 1 and negatives with 0
  n <- length(act)
  tp <- sum((act==1)&(pred==1))
  tn <- sum((act==0)&(pred==0))
  fp <- sum((act==0)&(pred==1))
  fn <- sum((act==1)&(pred==0))

  sp <- tn/(tn + fp)

  out <- data.frame(N=n, true_pos = tp, true_neg= tn, false_pos = fp, false_neg= fn, specificity = sp)

  return(out)
}


## ----tetrahedron, fig.height=2.5, fig.cap="Left: points $p$, $q$, $r$, $s$ and $t$ with a proximity parameter $\\varepsilon = 0.5$ and the resulting Rips complex consisting of the tetrahedron $pqrs$, triangles $pqr$, $qrs$, $rsp$, $pqs$, edges $pq$, $qr$, $rs$, $sp$, $qs$, $pr$ and vertices $p$, $q$, $r$, $s$ and $t$. Right: eight points and the resulting Rips complex with $\\varepsilon=4/3$."----
d <- tibble(
  point = c("p", "q", "r", "s", "t"),
  x = c(0, 0.2, 0, 0.1, 0.75),
  y = c(0, 0.2, 0.2, 0, 0.75)
)
# compute the persistent homology
#ph <- as_tibble(TDAstats::calculate_homology(as.matrix(d[,2:3]), dim = 1))
#ph <- transform(ph, dim = as.factor(dimension))
prox <- 0.5

p1 <- ggplot(d, aes(x = x, y = y, label = point)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = prox / 2, fill = "aquamarine3") +
  geom_point() +
  geom_text(nudge_x = 0.08, nudge_y = 0.08) +
  stat_vietoris0() +
  stat_vietoris1(diameter = prox, alpha = .25) +
  stat_vietoris2(diameter = prox, fill = "darkgoldenrod")

X <- tibble(
  x = rnorm(8),
  y = rnorm(8)
)
# compute the persistent homology
#ph <- as.tibble(TDAstats::calculate_homology(as.matrix(X), dim = 1))
#ph <- transform(ph, dim = as.factor(dimension))

prox <- 4 / 3
p2 <- ggplot(X, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = prox / 2, fill = "aquamarine3") +
  geom_point() +
  stat_vietoris0() +
  stat_vietoris1(diameter = prox, alpha = .25) +
  stat_vietoris2(diameter = prox, fill = "darkgoldenrod")

p1 | p2


## ----annulus, fig.cap="Rips complexes resulting from different $\\varepsilon$ values.", fig.width=7, fig.height=4, output="100%"----
outer_radius <- 1
inner_radius <- 0.7
n <- 50
rho <- sqrt(runif(n, inner_radius^2, outer_radius^2))
theta <- runif(n, 0, 2 * pi)
X <- tibble(
  x = rho * cos(theta),
  y = rho * sin(theta)
)
#ph <- TDAstats::calculate_homology(as.matrix(X), dim = 1)
#ph <- as.tibble(ph)
#ph <- transform(ph, dim = as.factor(dimension))

prox <- 0.1
p3 <- ggplot(X, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = prox / 2, fill = "aquamarine3") +
  geom_point() +
  stat_vietoris0() +
  stat_vietoris1(diameter = prox, alpha = .25) +
  stat_vietoris2(diameter = prox, fill = "darkgoldenrod") +
  labs(subtitle = TeX(sprintf('$\\epsilon$ = %.1f', prox))) +
  xlim(-1.75,1.75) + ylim(-1.75,1.75)

prox <- 0.3
p4 <- ggplot(X, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = prox / 2, fill = "aquamarine3") +
  geom_point() +
  stat_vietoris0() +
  stat_vietoris1(diameter = prox, alpha = .25) +
  stat_vietoris2(diameter = prox, fill = "darkgoldenrod") +
  labs(subtitle = TeX(sprintf('$\\epsilon$ = %.1f', prox))) +
  xlim(-1.75,1.75) + ylim(-1.75,1.75)

prox <- 0.8
p5 <- ggplot(X, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = prox / 2, fill = "aquamarine3") +
  geom_point() +
  stat_vietoris0() +
  stat_vietoris1(diameter = prox, alpha = .25) +
  stat_vietoris2(diameter = prox, fill = "darkgoldenrod") +
  labs(subtitle = TeX(sprintf('$\\epsilon$ = %.1f', prox))) +
  xlim(-1.75,1.75) + ylim(-1.75,1.75)

prox <- 1.5
p6 <- ggplot(X, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = prox / 2, fill = "aquamarine3") +
  geom_point() +
  stat_vietoris0() +
  stat_vietoris1(diameter = prox, alpha = .25) +
  stat_vietoris2(diameter = prox, fill = "darkgoldenrod") +
  labs(subtitle = TeX(sprintf('$\\epsilon$ = %.1f', prox))) +
  xlim(-1.75,1.75) + ylim(-1.75,1.75)

(p3 | p4 | p5 | p6)


## ----barcodeandpersistence, fig.cap="Left: the barcode of the point cloud in Figure \\ref{fig:annulus}. Right: the persistence diagram.", dependson="annulus", out.width="75%", fig.align="center"----
# compute the persistent homology
ph1 <- TDAstats::calculate_homology(as.matrix(X), dim = 1)
# plot topological barcode
bcd <- plot_barcode(ph1)
# plot persistence diagram
pst <- plot_persist(ph1) + xlab("Birth") + ylab("Death")

bcd | pst


## ----include=FALSE-------------------------------------------------------------------------------------------------
longblueline <- ph1 %>% as.data.frame() %>% filter(dimension==1) %>% filter(row_number() == 1L)
beta <- ph1 %>%
  as.data.frame() %>%
  filter(birth <= 0.60, death >= 0.60)
beta0 <- beta %>% filter(dimension==0) %>% NROW()
beta1 <- beta %>% filter(dimension==1) %>% NROW()


## ----TDAKNN, fig.cap="Top left: A scatterplot of 1000 observations with most points falling on an annulus and some points near the center. Its TDA barcode on the top right and the violin plot of TDA death diameters and KNN distances at the bottom.", out.width="90%", fig.align='center'----
oo <- 10
outer_radius <- 1
inner_radius <- 0.7
n <- 1000 - oo
rho <- sqrt(abs(rnorm(n, mean = 5, sd = 1)))
theta <- runif(n, 0, 2 * pi)
X <- bind_rows(
  tibble(
    x = rho * cos(theta),
    y = rho * sin(theta)
  ),
  tibble(
    x = rnorm(oo, mean = 0, sd = 0.2),
    y = rnorm(oo, mean = 0, sd = 0.2)
  )
)
g1 <- ggplot(X, aes(x, y)) +
  geom_point() +
  theme_bw()

# KNN distances
phom <- calculate_homology(X, dim = 0)
top2 <- tail(phom[,"death"],2)
df <- tibble(
    Type = "TDA",
    Distance = phom[,"death"]
  )
nn_obj <- RANN::nn2(X, k = 10+1)
for (kk in c(1, 5, 10)) {
  df <- bind_rows(df,
    tibble(
      Type = rep(paste0("NN_", kk), NROW(nn_obj$nn.dists)),
      Distance = nn_obj$nn.dists[, kk+1]
    )
  )
}

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m - sd(x)
  ymax <- m + sd(x)
  return(c(y = m, ymin = ymin, ymax = ymax))
}
g3 <- ggplot(df, aes(x = Type, y = Distance, fill = Type)) +
  geom_violin(trim = FALSE) +
  stat_summary(fun.data = data_summary) +
  scale_fill_brewer(palette = "Dark2") +
  xlab("Distance Type") +
  theme_bw()

g2 <- plot_barcode(phom) + 
  geom_vline(xintercept = top2[1], lty = 2)

g4 <- plot_barcode(tail(phom, 20)) + 
  geom_vline(xintercept = top2[1], lty = 2)
# g4

(g1 / g3) | (g2 / g4)


## ----outlierpersistence, fig.height=3, fig.cap="Outliers at the center of the annulus in the left plot showing the outlier labels 1001--1005. The outlier persistence diagram on the right with the y-axis denoting the labels. The dashed line shows the lookout bandwidth $d_*$.", out.width="90%", fig.align="center"----
oo <- 5
outer_radius <- 1
inner_radius <- 0.7
n <- 1000
rho <- sqrt(abs(rnorm(n, mean = 5, sd = 1)))
theta <- runif(n, 0, 2 * pi)
X <- bind_rows(
  tibble(
    x = rho * cos(theta),
    y = rho * sin(theta)
  ),
  tibble(
    x = rnorm(oo, mean = 0, sd = 0.2),
    y = rnorm(oo, mean = 0, sd = 0.2)
  )
)

g1 <- ggplot(X, aes(x, y)) +
  geom_point() +
  geom_text_repel(
    data = tail(X, 5) %>% mutate(lab = 1001:1005),
    aes(x, y, label = lab)
  ) +
  theme_bw()

lookobj <- lookout(X)
outnew <- persisting_outliers(X, num_steps = 20)
g2 <- autoplot(outnew, alpha = 0.05) +
  geom_vline(xintercept = lookobj$bandwidth, linetype = "dashed")

g1 | g2


## ----outlierpersistence2, fig.width=6, fig.height=3, dependson="outlierpersistence", fig.cap="Outlier persistence over different bandwidth values and their strengths. The dashed line corresponds to the lookout bandwidth $d_*$.", dependson="outlierpersistence", out.width="80%", fig.align="center"----
autoplot(outnew) +
  geom_vline(xintercept = lookobj$bandwidth, linetype = "dashed")


## ----persistenceEx, fig.cap="Examples of outlier persistence and strength. Left: data with the outliers identified by lookout with $\\alpha = 0.1$ colored by its strength. Right: outlier persistence diagrams for these examples. The dashed line corresponds to the lookout bandwidth.", fig.height=6, fig.asp=1.2, out.width="100%"----
# ---------------------------------------------------------------------
# TASK 1 - EXAMPLE 1
# ---------------------------------------------------------------------
X <- bind_rows(
  tibble(
    x = rnorm(500),
    y = rnorm(500)
  ),
  tibble(
    x = rnorm(5, mean = 10, sd = 0.2),
    y = rnorm(5, mean = 10, sd = 0.2)
  )
)
outnew <- persisting_outliers(X, unitize = FALSE)
g2 <- autoplot(outnew) +
  geom_vline(xintercept = outnew$lookoutbw, linetype = "dashed") 

lookobj <- lookout(X, unitize = FALSE, alpha = 0.1)
X2 <- X %>%
  mutate(strength = pmax(0, (0.11 - lookobj$outlier_probability) / 0.01))
g1 <- ggplot(X2, aes(x, y)) +
  geom_point(aes(color = strength)) +
  scale_colour_gradientn(colours = col_pal2) +
  theme_bw() +
  theme(legend.position = "none")

extra_outlier <- X2 %>%
  filter(strength > 0, strength < 2)

# ---------------------------------------------------------------------
# TASK 2 - EXAMPLE 2
# ---------------------------------------------------------------------
X <- bind_rows(
  tibble(
    x = rnorm(500, mean = -10),
    y = rnorm(500),
  ),
  tibble(
    x = rnorm(500, mean = 10),
    y = rnorm(500)
  ),
  tibble(
    x = rnorm(5, sd = 0.2),
    y = rnorm(5, sd = 0.2)
  )
)

outnew <- persisting_outliers(X, unitize = FALSE)
g4 <- autoplot(outnew) +
  geom_vline(xintercept = outnew$lookoutbw, linetype = "dashed") +
  ylab("") +
  theme(legend.position = "none")

lookobj <- lookout(X, unitize = FALSE, alpha = 0.1)
X2 <- X %>%
  mutate(strength = pmax(0, (0.11 - lookobj$outlier_probability) / 0.01))
g3 <- ggplot(X2, aes(x, y)) +
  geom_point(aes(color = strength)) +
  scale_colour_gradientn(colours = col_pal2) +
  theme_bw() +
  ylab("") +
  theme(legend.position = "none")

# ---------------------------------------------------------------------
# TASK 3 - EXAMPLE 3
# ---------------------------------------------------------------------
X <- bind_rows(
  tibble(
    x = rnorm(500),
    y = rnorm(500)
  ),
  tibble(
    x = rnorm(100, sd = 0.7),
    y = rnorm(100, mean = 8, sd = 0.7)
  ),
  tibble(
    x = rnorm(100, mean = 8, sd = 0.7),
    y = rnorm(100, sd = 0.7)
  ),
  tibble(
    x = rnorm(3, mean = 6, sd = 0.2),
    y = rnorm(3, mean = 6, sd = 0.2)
  )
)
outnew <- persisting_outliers(X, unitize = FALSE)
g6 <- autoplot(outnew) +
  ylab("") +
  geom_vline(xintercept = outnew$lookoutbw, linetype = "dashed") +
  theme(legend.position = "none")

lookobj <- lookout(X, unitize = FALSE, alpha = 0.1)
X2 <- X %>%
  mutate(strength = pmax(0, (0.11 - lookobj$outlier_probability) / 0.01))
g5 <- ggplot(X2, aes(x, y)) +
  geom_point(aes(color = strength)) +
  ylab("") +
  scale_colour_gradientn(colours = col_pal2) +
  theme_bw() +
  theme(legend.position = "none")

# ---------------------------------------------------------------------
# TASK 4 - EXAMPLE 4
# ---------------------------------------------------------------------
X <- bind_rows(
  tibble(
    x = rnorm(500),
    y = rnorm(500)
  ),
  tibble(
    x = rnorm(100, sd = 0.7),
    y = rnorm(100, mean = 8, sd = 0.7)
  ),
  tibble(
    x = rnorm(100, mean = 8, sd = 0.7),
    y = rnorm(100, sd = 0.7)
  ),
  tibble(
    x = c(5,8,12),
    y = c(5, 7.5, 4)
  )
)
outnew <- persisting_outliers(X, unitize = FALSE)
g8 <- autoplot(outnew) +
  geom_vline(xintercept = outnew$lookoutbw, linetype = "dashed") +
  ylab("") +
  theme(legend.position = "none")

lookobj <- lookout(X, unitize = FALSE, alpha = 0.1)
X2 <- X %>%
  mutate(strength = pmax(0, (0.11 - lookobj$outlier_probability) / 0.01))
g7 <- ggplot(X2, aes(x, y)) +
  geom_point(aes(color = strength)) +
  ylab("") +
  scale_colour_gradientn(colours = col_pal2) +
  theme_bw() +
  theme(legend.position = "none")

# ---------------------------------------------------------------------
# TASK 5 - EXAMPLE 5
# ---------------------------------------------------------------------
X <- bind_rows(
  tibble(x = rnorm(1000, sd = 0.2)) %>%
    mutate(y = x^2 + rnorm(1000, sd = 0.01)),
  tibble(
    x = c(0, -0.2, 0.4),
    y = c(0.3, 0.4, 0.5)
  )
)

outnew <- persisting_outliers(X, unitize = FALSE)
g10 <- autoplot(outnew) +
  geom_vline(xintercept = outnew$lookoutbw, linetype = "dashed") +
  ylab("") +
  theme(legend.position = "none")

lookobj <- lookout(X, unitize = FALSE, alpha = 0.1)
X2 <- X %>%
  mutate(strength = pmax(0, (0.11 - lookobj$outlier_probability) / 0.01))
g9 <- ggplot(X2, aes(x, y)) +
  geom_point(aes(color = strength)) +
  ylab("") +
  scale_colour_gradientn(colours = col_pal2) +
  theme_bw() +
  theme(legend.position = "none")

(g1 / g3 / g5 / g7 / g9) |
  (g2 / g4 / g6 / g8 / g10)


## ----ComparisonEx1, fig.cap="Experiment 1 with outliers moving out from the normal distribution in $\\mathbb{R}^6$. Top left: first two dimensions in the last iteration and last repetition. Top right: performance comparison between lookout, HDoutliers, stray, kdeos and rdos using AUC, Fmeasure and Gmean over ten repetitions. In this experiment, stray and HDoutliers gave identical results. Bottom left: time taken for these five algorithms. Bottom right: corresponding outlier persistence plot.", out.width="80%", fig.align="center", message=FALSE----
values <- rep(0, 10)
pp <- 10
hdoutliers_gmean <- hdoutliers_fmeasure <- lookout_fmeasure <- lookout_gmean <-
  stray_gmean <- stray_fmeasure <- matrix(0, nrow = pp, ncol = 10)

lookout_roc <- kdeos_roc <- rdos_roc <- matrix(0, nrow=pp, ncol=10)

hdoutliers_time <- lookout_time <- stray_time <- kdeos_time <- rdos_time <- matrix(0, nrow=pp*10, ncol=5)

for (kk in seq(pp)) {
  X <- bind_cols(
    x2 = rnorm(405),
    x3 = rnorm(405),
    x4 = rnorm(405),
    x5 = rnorm(405),
    x6 = rnorm(405)
  )
  x1_1 <- rnorm(400)
  labs <- c(rep(0, 400), rep(1, 5))

  for (i in seq(10)) {
    x1_2 <- rnorm(5, mean = 2 + (i-1)*0.5, sd = 0.2)
    X <- X %>% mutate(x1 = c(x1_1, x1_2))

    ll <- (kk-1)*10 + i

    # STRAY
    tt <- system.time(strayout <- stray::find_HDoutliers(X, knnsearchtype = "kd_tree", alpha=0.05))
    straylabs <- rep(0, 405)
    straylabs[strayout$outliers] <- 1
    strayoutput <- diff_metrics(labs, straylabs)
    stray_gmean[kk, i] <- strayoutput$gmean
    stray_fmeasure[kk, i] <- strayoutput$fmeasure
    stray_time[ll, ] <- tt

    # LOOKOUT
    tt <- system.time(lookoutobj <- lookout(X, alpha=0.05, unitize = TRUE))
    lookoutlabs <- rep(0, 405)
    lookoutlabs[lookoutobj$outliers[ ,1]] <- 1
    lookoutput <- diff_metrics(labs, lookoutlabs)
    lookout_gmean[kk, i] <- lookoutput$gmean
    lookout_fmeasure[kk, i] <- lookoutput$fmeasure
    lookout_scores <- lookoutobj$outlier_scores
    roc_obj <- roc(labs, lookout_scores, direction="<")
    lookout_roc[kk, i] <- roc_obj$auc
    lookout_time[ll, ] <- tt

    # HDOUTLIERS
    tt <- system.time(hdoutobj <- HDoutliers(X, alpha=0.05))
    hdoutlabs <- rep(0, dim(X)[1])
    hdoutlabs[hdoutobj] <- 1
    hdoutput <- diff_metrics(labs, hdoutlabs)
    hdoutliers_gmean[kk, i] <- hdoutput$gmean
    hdoutliers_fmeasure[kk, i] <- hdoutput$fmeasure
    hdoutliers_time[ll, ] <- tt

    # KDEOS
    tt <- system.time(kdeos_scores <- DDoutlier::KDEOS(X)) # using default parameters
    roc_obj <- roc(labs, kdeos_scores, direction="<")
    kdeos_roc[kk, i] <- roc_obj$auc
    kdeos_time[ll, ] <- tt

    # RDOS
    tt <- system.time(rdos_scores <- DDoutlier::RDOS(X)) # using default parameters
    roc_obj <- roc(labs, rdos_scores, direction="<")
    rdos_roc[kk, i] <- roc_obj$auc
    rdos_time[ll, ] <- tt

  }
}

outnew <- persisting_outliers(X)
g2 <- autoplot(outnew) +
  geom_vline(xintercept = outnew$lookoutbw, linetype = "dashed")

lookobj <- lookout(X, alpha = 0.1)
X2 <- as_tibble(X) %>%
  mutate(strength = pmax(0, (0.1 - lookobj$outlier_probability) / 0.01))
g1 <- ggplot(X2, aes(x1, x2)) +
  geom_point(aes(color = strength)) +
  scale_colour_gradientn(colours = col_pal2) +
  theme_bw() +
  theme(legend.position = "none")

# PLOT F-MEASURE
str_mean <- colMeans(stray_fmeasure)
lookout_mean <- colMeans(lookout_fmeasure)
hdoutliers_mean <- colMeans(hdoutliers_fmeasure)

# Commented these lines as we're putting together the graphs - SK
# df <- tibble(
#     Iteration = seq(10),
#     stray = str_mean,
#     lookout = lookout_mean,
#     HDoutliers = hdoutliers_mean
#   ) %>%
#   pivot_longer(-Iteration, names_to = "Method")
# g3 <- ggplot(df, aes(Iteration, value)) +
#   geom_line(aes(color = Method), size = 1) +
#   ylab("Fmeasure") +
#   theme_bw() +
#   scale_x_continuous(breaks = 1:10) +
#   theme(legend.position = "none")

dfl1 <- tibble(
  Iteration = seq(10),
  stray = str_mean,
  lookout = lookout_mean,
  HDoutliers = hdoutliers_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Fmeasure')

# PLOT GEOMETRIC MEAN OF SENSITIVITY AND SPECIFICITY
str_mean <- colMeans(stray_gmean)
lookout_mean <- colMeans(lookout_gmean)
hdoutliers_mean <- colMeans(hdoutliers_gmean)

# df <- tibble(
#     Iteration = seq(10),
#     stray = str_mean,
#     lookout = lookout_mean,
#     HDoutliers = hdoutliers_mean
#   ) %>%
#   pivot_longer(-Iteration, names_to = "Method")
# g4 <- ggplot(df, aes(Iteration, value)) +
#   geom_line(aes(color = Method), size = 1) +
#   ylab("Gmean") +
#   scale_x_continuous(breaks = 1:10) +
#   theme_bw()

dfl2 <- tibble(
  Iteration = seq(10),
  stray = str_mean,
  lookout = lookout_mean,
  HDoutliers = hdoutliers_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Gmean')

# AUC FOR KDEOS, RDOS AND LOOKOUT
kdeos_mean <- colMeans(kdeos_roc)
lookout_mean <-  colMeans(lookout_roc)
rdos_mean <-  colMeans(rdos_roc)

dfl3 <- tibble(
  Iteration = seq(10),
  kdeos = kdeos_mean,
  lookout = lookout_mean,
  rdos = rdos_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'AUC')

dfl <- bind_rows(dfl1, dfl2, dfl3)

g3 <- ggplot(dfl, aes(x=Iteration, y=value)) + geom_line(aes(color=Algorithm), size=1) + ylab("Performance") + facet_wrap(~Metric) + scale_x_continuous(breaks=2*(1:5)) + theme_bw()

# TIME TAKEN

dftime <- tibble(
  Run = seq(100),
  stray = stray_time[ ,3],
  lookout = lookout_time[ ,3],
  HDoutliers = hdoutliers_time[ ,3], 
  kdeos = kdeos_time[ ,3], 
  rdos = rdos_time[ ,3]
) %>%
  pivot_longer(-Run, names_to = "Algorithm")

g4 <- ggplot(dftime, aes(Algorithm, value)) + geom_boxplot(outlier.shape = 20)  + ylab("Time (mins)") + coord_flip() + theme_bw()

(g1 | g3) / (g4 | g2)


## ----ComparisonEx2, fig.cap="Experiment 2 with outliers moving into the center of the annulus in $\\mathbb{R}^2$. Top left: points from the last iteration and repetition. Top right: performance comparison between lookout, HDoutliers, stray, kdeos and rdos using AUC, Fmeasure and Gmean over 10 repetitions. Bottom left: time taken for these five algorithms. Bottom right: the corresponding outlier persistence plot.", out.width="80%", fig.align="center", message=FALSE, fig.pos="b"----
values <- rep(0, 10)
pp <- 10
hdoutliers_gmean <- hdoutliers_fmeasure <- lookout_fmeasure <- lookout_gmean <-
  stray_gmean <- stray_fmeasure <- matrix(0, nrow = pp, ncol = 10)

lookout_roc <- kdeos_roc <- rdos_roc <- matrix(0, nrow=pp, ncol=10)

hdoutliers_time <- lookout_time <- stray_time <- kdeos_time <- rdos_time <- matrix(0, nrow=pp*10, ncol=5)

for (kk in seq(pp)) {
  nn <- 805
  r1 <- runif(nn)
  r2 <- rnorm(nn, mean = 5)
  theta <- 2 * pi * r1
  R2 <- 2
  dist <- r2 + R2
  X <- tibble(
    x1 = dist * cos(theta),
    x2 = dist * sin(theta)
  )
  labs <- c(rep(0, nn-5), rep(1, 5))

  for (i in seq(10)) {
    X[nn - 5 + seq(5), 1] <- rnorm(5, 5 - (i-1)*0.5, sd = 0.1)
    X[nn - 5 + seq(5), 2] <- rnorm(5, 0, sd = 0.1)

    ll <- (kk-1)*10 + i
    # STRAY
    tt <- system.time(strayout <- stray::find_HDoutliers(X, knnsearchtype = "kd_tree", alpha=0.05))
    straylabs <- rep(0, nn)
    straylabs[strayout$outliers] <- 1
    strayoutput <- diff_metrics(labs, straylabs)
    stray_gmean[kk, i] <- strayoutput$gmean
    stray_fmeasure[kk, i] <- strayoutput$fmeasure
    stray_time[ll, ] <- tt

    # LOOKOUT
    tt <- system.time(lookoutobj <- lookout(X, alpha=0.05, unitize = TRUE))
    lookoutlabs <- rep(0, nn)
    lookoutlabs[lookoutobj$outliers[ ,1]] <- 1
    lookoutput <- diff_metrics(labs, lookoutlabs)
    lookout_gmean[kk, i] <- lookoutput$gmean
    lookout_fmeasure[kk, i] <- lookoutput$fmeasure
    lookout_scores <- lookoutobj$outlier_scores
    roc_obj <- roc(labs, lookout_scores, direction="<")
    lookout_roc[kk, i] <- roc_obj$auc
    lookout_time[ll, ] <- tt

    # HDOUTLIERS
    tt <- system.time(hdoutobj <- HDoutliers(X, alpha=0.05))
    hdoutlabs <- rep(0, nn)
    hdoutlabs[hdoutobj] <- 1
    hdoutput <- diff_metrics(labs, hdoutlabs)
    hdoutliers_gmean[kk, i] <- hdoutput$gmean
    hdoutliers_fmeasure[kk, i] <- hdoutput$fmeasure
    hdoutliers_time[ll, ] <- tt

    # KDEOS
    tt <- system.time(kdeos_scores <- DDoutlier::KDEOS(X)) # using default parameters
    roc_obj <- roc(labs, kdeos_scores, direction="<")
    kdeos_roc[kk, i] <- roc_obj$auc
    kdeos_time[ll, ] <- tt

    # RDOS
    tt <- system.time(rdos_scores <- DDoutlier::RDOS(X)) # using default parameters
    roc_obj <- roc(labs, rdos_scores, direction="<")
    rdos_roc[kk, i] <- roc_obj$auc
    rdos_time[ll, ] <- tt

  }
}

outnew <- persisting_outliers(X)
g2 <- autoplot(outnew) + geom_vline(xintercept = outnew$lookoutbw, linetype = "dashed")

lookobj <- lookout(X, alpha = 0.1)
strength <- (0.1 - lookobj$outlier_probability) / 0.01
strength[strength < 0] <- 0

X2 <- cbind.data.frame(X, strength)
g1 <- ggplot(X2, aes(x1, x2)) +
  geom_point(aes(color = strength)) +
  scale_colour_gradientn(colours = col_pal2) +
  theme_bw() +
  theme(legend.position = "none")

# F-MEASURE
str_mean <- colMeans(stray_fmeasure)
lookout_mean <- colMeans(lookout_fmeasure)
hdoutliers_mean <- colMeans(hdoutliers_fmeasure)

dfl1 <- tibble(
  Iteration = seq(10),
  stray = str_mean,
  lookout = lookout_mean,
  HDoutliers = hdoutliers_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Fmeasure')

# GEOMETRIC MEAN OF SENSITIVITY AND SPECIFICITY
str_mean <- colMeans(stray_gmean)
lookout_mean <- colMeans(lookout_gmean)
hdoutliers_mean <- colMeans(hdoutliers_gmean)

dfl2 <- tibble(
  Iteration = seq(10),
  stray = str_mean,
  lookout = lookout_mean,
  HDoutliers = hdoutliers_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Gmean')

# AUC FOR KDEOS, RDOS AND LOOKOUT
kdeos_mean <- colMeans(kdeos_roc)
lookout_mean <-  colMeans(lookout_roc)
rdos_mean <-  colMeans(rdos_roc)

dfl3 <- tibble(
  Iteration = seq(10),
  kdeos = kdeos_mean,
  lookout = lookout_mean,
  rdos = rdos_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'AUC')

dfl <- bind_rows(dfl1, dfl2, dfl3)

g3 <- ggplot(dfl, aes(x=Iteration, y=value)) + geom_line(aes(color=Algorithm), size=1) + ylab("Performance") + facet_wrap(~Metric) + scale_x_continuous(breaks=2*(1:5)) + theme_bw()

# TIME TAKEN
dftime <- tibble(
  Run = seq(100),
  stray = stray_time[ ,3],
  lookout = lookout_time[ ,3],
  HDoutliers = hdoutliers_time[ ,3],
  kdeos = kdeos_time[ ,3],
  rdos = rdos_time[ ,3]
) %>%
  pivot_longer(-Run, names_to = "Algorithm")

g4 <- ggplot(dftime, aes(Algorithm, value)) + geom_boxplot(outlier.shape = 20)  + ylab("Time (mins)") + coord_flip() + theme_bw()

(g1 | g3) / (g4 | g2)


## ----ComparisonEx3, fig.cap="Experiment 3 with an outlier moving to $(0.9, 0.9, \\dots)$. Top: performance comparison between lookout, HDoutliers, stray, KDEOS and RDOS using AUC, Fmeasure and Gmean over ten repetitions. Bottom left: time taken for these five algorithms. Bottom right: corresponding outlier persistence plot.", out.width="80%", fig.align="center", message=FALSE, fig.pos="b"----
values <- rep(0, 10)
pp <- 10
dd <- 19
hdoutliers_gmean <- hdoutliers_fmeasure <- lookout_fmeasure <- lookout_gmean <-
  stray_gmean <- stray_fmeasure <- matrix(0, nrow = pp, ncol = 20)

lookout_roc <- kdeos_roc <- rdos_roc <- matrix(0, nrow=pp, ncol=20)

hdoutliers_time <- lookout_time <- stray_time <- kdeos_time <- rdos_time <- matrix(0, nrow=pp*20, ncol=5)

nn <- 500
labs <- c(rep(0, nn-1), 1)

for (kk in seq(pp)) {
  X <- matrix(runif(nn*(dd+1)), ncol=dd+1, nrow=nn)
  colnames(X) <- paste("x", 1:20, sep = "")

  for (i in seq(20)) {
    X[nn, seq(i)] <- rep(0.9, i)

     ll <- (kk-1)*10 + i
     # STRAY
    tt <- system.time(strayout <- stray::find_HDoutliers(X, knnsearchtype = "kd_tree", alpha=0.05))
    straylabs <- rep(0, nn)
    straylabs[strayout$outliers] <- 1
    strayoutput <- diff_metrics(labs, straylabs)
    stray_gmean[kk, i] <- strayoutput$gmean
    stray_fmeasure[kk, i] <- strayoutput$fmeasure
    stray_time[ll, ] <- tt

    # LOOKOUT
    tt <- system.time(lookoutobj <- lookout(X, alpha=0.05, unitize = TRUE))
    lookoutlabs <- rep(0, nn)
    lookoutlabs[lookoutobj$outliers[ ,1]] <- 1
    lookoutput <- diff_metrics(labs, lookoutlabs)
    lookout_gmean[kk, i] <- lookoutput$gmean
    lookout_fmeasure[kk, i] <- lookoutput$fmeasure
    lookout_scores <- lookoutobj$outlier_scores
    roc_obj <- roc(labs, lookout_scores, direction="<")
    lookout_roc[kk, i] <- roc_obj$auc
    lookout_time[ll, ] <- tt

    # HDOUTLIERS
    tt <- system.time(hdoutobj <- HDoutliers(X, alpha=0.05))
    hdoutlabs <- rep(0, nn)
    hdoutlabs[hdoutobj] <- 1
    hdoutput <- diff_metrics(labs, hdoutlabs)
    hdoutliers_gmean[kk, i] <- hdoutput$gmean
    hdoutliers_fmeasure[kk, i] <- hdoutput$fmeasure
    hdoutliers_time[ll, ] <- tt

    # KDEOS
    tt <- system.time(kdeos_scores <- DDoutlier::KDEOS(X)) # using default parameters
    roc_obj <- roc(labs, kdeos_scores, direction="<")
    kdeos_roc[kk, i] <- roc_obj$auc
    kdeos_time[ll, ] <- tt

    # RDOS
    tt <- system.time(rdos_scores <- DDoutlier::RDOS(X)) # using default parameters
    roc_obj <- roc(labs, rdos_scores, direction="<")
    rdos_roc[kk, i] <- roc_obj$auc
    rdos_time[ll, ] <- tt
  }
}

outnew <- persisting_outliers(X)
g2 <- autoplot(outnew) +
  geom_vline(xintercept = outnew$lookoutbw, linetype = "dashed")

# F-MEASURE
str_mean <- colMeans(stray_fmeasure)
lookout_mean <- colMeans(lookout_fmeasure)
hdoutliers_mean <- colMeans(hdoutliers_fmeasure)

dfl1 <- tibble(
  Iteration = seq(20),
  stray = str_mean,
  lookout = lookout_mean,
  HDoutliers = hdoutliers_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Fmeasure')

# GEOMETRIC MEAN OF SENSITIVITY AND SPECIFICITY
str_mean <- colMeans(stray_gmean)
lookout_mean <- colMeans(lookout_gmean)
hdoutliers_mean <- colMeans(hdoutliers_gmean)

dfl2 <- tibble(
  Iteration = seq(20),
  stray = str_mean,
  lookout = lookout_mean,
  HDoutliers = hdoutliers_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Gmean')

# AUC FOR KDEOS, RDOS AND LOOKOUT
kdeos_mean <- colMeans(kdeos_roc)
lookout_mean <-  colMeans(lookout_roc)
rdos_mean <-  colMeans(rdos_roc)

dfl3 <- tibble(
  Iteration = seq(20),
  kdeos = kdeos_mean,
  lookout = lookout_mean,
  rdos = rdos_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'AUC')

dfl <- bind_rows(dfl1, dfl2, dfl3)

g3 <- ggplot(dfl, aes(x=Iteration, y=value)) + geom_line(aes(color=Algorithm), size=1) + ylab("Performance") + facet_wrap(~Metric) + scale_x_continuous(breaks=2*(1:10)) + theme_bw()

# TIME TAKEN
dftime <- tibble(
  Run = seq(200),
  stray = stray_time[ ,3],
  lookout = lookout_time[ ,3],
  HDoutliers = hdoutliers_time[ ,3],
  kdeos = kdeos_time[ ,3],
  rdos = rdos_time[ ,3]
) %>%
  pivot_longer(-Run, names_to = "Algorithm")

g4 <- ggplot(dftime, aes(Algorithm, value)) + geom_boxplot(outlier.shape = 20)  + ylab("Time (mins)") + coord_flip() + theme_bw()

g3 / (g4 | g2)


## ----zerooutliers, fig.cap="", out.width="80%", fig.align="center", message=FALSE----------------------------------

output <- matrix(0, nrow=3, ncol=6)
colnames(output) <- c("lookout_mean", "lookout_sd", "stray_mean", "stray_sd", "hdoutliers_mean", "hdoutliers_sd")

# ---------------------------------------------------------------------
# TASK 1 - EXAMPLE 1 - NO OUTLIERS
# ---------------------------------------------------------------------
library(lookout)
library(HDoutliers)
library(stray)
values <- rep(0, 10)
pp <- 10

hdoutliers_specificity <- stray_specificity <- lookout_specificity <-  rep(0, 10)

for(kk in 1:pp){
  x2 <- rnorm(405)
  x3 <- rnorm(405)
  x4 <- rnorm(405)
  x5 <- rnorm(405)
  x6 <- rnorm(405)
  x1 <- rnorm(405)

  X <- cbind(x1,x2,x3,x4,x5,x6)
  labs <- rep(0,405)

  # STRAY
  strayout <- stray::find_HDoutliers(X, knnsearchtype = "kd_tree", alpha=0.05)
  straylabs <- rep(0, 405)
  straylabs[strayout$outliers] <- 1
  strayoutput <- diff_metrics_2(labs, straylabs)
  stray_specificity[kk] <- strayoutput$specificity

  # LOOKOUT
  lookoutobj <- lookout(X, alpha=0.05, unitize = TRUE)
  lookoutlabs <- rep(0, 405)
  lookoutlabs[lookoutobj$outliers[ ,1]] <- 1
  lookoutput <- diff_metrics_2(labs, lookoutlabs)
  lookout_specificity[kk] <- lookoutput$specificity

  # HDOUTLIERS
  hdoutobj <- HDoutliers(X, alpha=0.05)
  hdoutlabs <- rep(0, dim(X)[1])
  hdoutlabs[hdoutobj] <- 1
  hdoutput <- diff_metrics_2(labs, hdoutlabs)
  hdoutliers_specificity[kk] <- hdoutput$specificity

}
output[1, ] <- c(mean(lookout_specificity), sd(lookout_specificity), mean(stray_specificity), sd(stray_specificity), mean(hdoutliers_specificity), sd(hdoutliers_specificity))

# ---------------------------------------------------------------------
# TASK 2 - EXAMPLE 2 - NO OUTLIERS
# ---------------------------------------------------------------------
hdoutliers_specificity <- stray_specificity <- lookout_specificity <-  rep(0, 10)

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
    x2 = y
  )
  labs <- rep(0,805)

  # STRAY
  strayout <- stray::find_HDoutliers(X, knnsearchtype = "kd_tree", alpha=0.05)
  straylabs <- rep(0, 805)
  straylabs[strayout$outliers] <- 1
  strayoutput <- diff_metrics_2(labs, straylabs)
  stray_specificity[kk] <- strayoutput$specificity

  # LOOKOUT
  lookoutobj <- lookout(X, alpha=0.05, unitize = TRUE)
  lookoutlabs <- rep(0, 805)
  lookoutlabs[lookoutobj$outliers[ ,1]] <- 1
  lookoutput <- diff_metrics_2(labs, lookoutlabs)
  lookout_specificity[kk] <- lookoutput$specificity

  # HDOUTLIERS
  hdoutobj <- HDoutliers(X, alpha=0.05)
  hdoutlabs <- rep(0, dim(X)[1])
  hdoutlabs[hdoutobj] <- 1
  hdoutput <- diff_metrics_2(labs, hdoutlabs)
  hdoutliers_specificity[kk] <- hdoutput$specificity

}
output[2, ] <- c(mean(lookout_specificity), sd(lookout_specificity), mean(stray_specificity), sd(stray_specificity), mean(hdoutliers_specificity), sd(hdoutliers_specificity))

# ---------------------------------------------------------------------
# TASK 3 - EXAMPLE 3 - NO OUTLIERS
# ---------------------------------------------------------------------

hdoutliers_specificity <- stray_specificity <- lookout_specificity <-  rep(0, 10)

labs <- rep(0, 500)
for(kk in 1:pp){
  X <- runif(500)
  for(j in 1:19){
    X <- cbind(X, runif(500))
  }
  colnames(X) <- paste("x", 1:20, sep="")
  # STRAY
  strayout <- stray::find_HDoutliers(X, knnsearchtype = "kd_tree", alpha=0.05)
  straylabs <- rep(0, 500)
  straylabs[strayout$outliers] <- 1
  strayoutput <- diff_metrics_2(labs, straylabs)
  stray_specificity[kk] <- strayoutput$specificity

  # LOOKOUT
  lookoutobj <- lookout(X, alpha=0.05, unitize = TRUE)
  lookoutlabs <- rep(0, 500)
  lookoutlabs[lookoutobj$outliers[ ,1]] <- 1
  lookoutput <- diff_metrics_2(labs, lookoutlabs)
  lookout_specificity[kk] <- lookoutput$specificity

  # HDOUTLIERS
  hdoutobj <- HDoutliers(X, alpha=0.05)
  hdoutlabs <- rep(0, dim(X)[1])
  hdoutlabs[hdoutobj] <- 1
  hdoutput <- diff_metrics_2(labs, hdoutlabs)
  hdoutliers_specificity[kk] <- hdoutput$specificity

}
output[3, ] <- c(mean(lookout_specificity), sd(lookout_specificity), mean(stray_specificity), sd(stray_specificity), mean(hdoutliers_specificity), sd(hdoutliers_specificity))

output <- round(as.data.frame(output), 4)

headcols <- data.frame(c1 = c("", "lookout", "stray", "HDoutliers"), c2=c(1, 2, 2, 2))

output %>%
  mutate(experiment = seq(3)) %>% relocate(experiment, .before = lookout_mean ) %>%
  knitr::kable(
    col.names = c("Experiment", "mean","sd", "mean", "sd", "mean", "sd"),
    booktabs = TRUE,
    caption = "Specificity for lookout, HDoutliers and stray algorithms when no outliers are present.") %>% kableExtra::add_header_above(x, header=headcols) %>%  kableExtra::row_spec(1:dim(output)[1], color = "blue"
  ) %>%
  kableExtra::kable_styling(font_size=10)


## ----timeseriesdat, echo=FALSE, include=FALSE----------------------------------------------------------------------
lookout_ts <- function(x, alpha = 0.1){
  u <- c(0,diff(diff(x)))
  out <- lookout(u, alpha = alpha, unitize = FALSE, bw = NULL, gpd = NULL)

  outliers <- out$outliers[ ,1]

  if(length(outliers) > 1){
   oo <- c()
   clust <- cumsum(c(1, diff(outliers) > 1))
   len <- max(clust)
   for(kk in 1:len){
     inds <- outliers[which(clust==kk)]
     oo <- c(oo, inds[which.min(out$outlier_probability[inds])] )
   }
   inds <- which(out$outliers[ ,1] %in% oo)
   out$outliers <- out$outliers[inds, ]
  }
  out
}

BT_outliers <- function(X, alpha=0.05, k=1){
  out <- c()
  for(i in 1:k){
    df1 <- diff(X)
    df2 <- -diff(X)
    df3 <- abs(df2)
    ordstatx <- sort(df3, decreasing = TRUE)
    ordx <- order(df3, decreasing = TRUE)

    BT_weights <- c(1, 0.531, 0.362, 0.280, 0.230, 0.193, 0.169, 0.147, 0.132, 0.123, 0.113, 0.104, 0.096, 0.088, 0.083, 0.080, 0.075, 0.071, 0.067, 0.065, 0.062, 0.060, 0.058, 0.056, 0.053, 0.052, 0.050, 0.048, 0.047, 0.046, 0.044, 0.042, 0.042, 0.040, 0.040, 0.039, 0.037, 0.037, 0.036, 0.035, 0.035, 0.034, 0.033, 0.032, 0.032, 0.031, 0.031, 0.031, 0.030, 0.029, 0.028, 0.028, 0.027, 0.027, 0.027, 0.026, 0.026, 0.025, 0.025, 0.025, 0.024, 0.024, 0.023, 0.023, 0.023, 0.023, 0.022, 0.022, 0.022, 0.022, 0.021, 0.021, 0.020, 0.020, 0.020, 0.020, 0.020, 0.020, 0.019, 0.019, 0.019, 0.019, 0.019, 0.018, 0.018, 0.018, 0.018, 0.018, 0.018, 0.018, 0.017, 0.017, 0.017, 0.017, 0.016, 0.016, 0.016, 0.016, 0.016, 0.016, 0.016, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.014, 0.014, 0.014, 0.014, 0.014, 0.014, 0.014, 0.013, 0.013)

    ll <- 3*k/alpha
    y <- -1*diff(ordstatx)[1:120]/BT_weights

    set1 <- 1:(3*k)
    cond <- which.max(y[1:(3*k/alpha)]) %in% set1
    if(!cond){
      return(out)
    }else{
      out <- c(out, ordx[which.max(y[1:(3*k/alpha)])] )
      X[out] <- X[out] - df2[out]

    }
  }
  return(out)
}

BT_gmean <- BT_fmeasure <- lookout_gmean  <- lookout_fmeasure <- matrix(0, nrow = 10, ncol = 5)
for(ii in 1:5){
  ar_val = 0.4 + ii*0.1
  for(jj in 1:10){
    ts_sim <- arima.sim(list(order = c(1,1,0), ar = ar_val), n = 200)
    ts_sim[50] <- ts_sim[50]  + 10
    labs <- rep(0, 200)
    labs[50] <- 1

    btout <- BT_outliers(ts_sim, k=3, alpha=0.1)
    loutobj <- lookout_ts(ts_sim, alpha=0.1)

    btlabs <- rep(0, 200)
    btlabs[btout] <- 1

    looklabs <- rep(0, 200)
    looklabs[loutobj$outliers[ ,1]] <- 1

    dm_bt <- diff_metrics(labs, btlabs)
    dm_lo <- diff_metrics(labs, looklabs)

    BT_gmean[jj, ii] <- dm_bt$gmean
    BT_fmeasure[jj, ii] <- dm_bt$fmeasure
    lookout_gmean[jj, ii] <- dm_lo$gmean
    lookout_fmeasure[jj, ii] <- dm_lo$fmeasure
  }
}


## ----timeseriesdatviz, dependson="timeseriesdat", fig.cap="", out.width="100%", message=FALSE, echo=FALSE----------
look_gm_mean <- apply(lookout_gmean, 2, mean)
look_gm_sd <- apply(lookout_gmean, 2, sd)
look_fm_mean <- apply(lookout_fmeasure, 2, mean)
look_fm_sd <- apply(lookout_fmeasure, 2, sd)

BT_gm_mean <- apply(BT_gmean, 2, mean)
BT_gm_sd <- apply(BT_gmean, 2, sd)
BT_fm_mean <- apply(BT_fmeasure, 2, mean)
BT_fm_sd <- apply(BT_fmeasure, 2, sd)

output <- tibble(ar = 0.1*(5:9),   lookout_gmean_m = look_gm_mean, lookout_gmean_sd = look_gm_sd,   BT_gmean_m = BT_gm_mean, BT_gmean_sd =  BT_gm_sd, look_fmeasure_m = look_fm_mean, look_fmeasure_sd = look_fm_sd, BT_fmeasure_m = BT_fm_mean, BT_fmeasure_sd = BT_fm_sd  )
output <- round(output, 3)

headcols <- data.frame(c1 = c("", "Gmean", "FMeasure"), c2=c(1, 4, 4))
headcols2 <- data.frame(c1 = c("", "lookout", "BT", "lookout", "BT"), c2=c(1, 2, 2, 2, 2))
# output %>%
#   knitr::kable(
#     col.names = c("AR", "lookout mean", "lookout sd", "BT mean", "BT sd", "lookout mean", "lookout sd", "BT mean", "BT sd"),
#     booktabs = TRUE,
#     caption = "Time series outliers performance comparison") %>% kableExtra::add_header_above(x, header=headcols) %>%  kableExtra::row_spec(1:dim(output)[1], color = "blue")

output %>%
  knitr::kable(
    col.names = c("AR", "mean", "sd", "mean", "sd", "mean", "sd", "mean", "sd"),
    booktabs = TRUE,
    caption = "Time series outliers performance comparison") %>% kableExtra::add_header_above(x, header=headcols2) %>% kableExtra::add_header_above(x, header=headcols) %>%  kableExtra::row_spec(1:dim(output)[1], color = "blue"
  ) %>%
  kableExtra::kable_styling(font_size=10)


## ----ODDSdatasetswork, message=FALSE, echo=FALSE, results='hide', warning=FALSE, fig.show='hide', include=FALSE----
knitr::knit_hooks$set(
  evaluate.inline = function (code, envir = knit_global()) {
    v = try(eval(xfun::parse_only(code), envir = envir))
    knitr::knit_print(v, inline = TRUE, options = knitr::opts_chunk$get())
  },
  inline = function(x) {
  if (any(class(x) == "try-error")) {
    as.vector(x)
  } else x
})

# The 12 ODDS datasets are downloaded and available at https://github.com/sevvandi/Paper-lookout/tree/master/lookout/Data_Input
folder <- here::here("Data_Input/")

output <- data.frame(look_gmean = numeric(), stray_gmean = numeric(), hdout_gmean = numeric(), look_fmeas = numeric(), stray_fmeas = numeric(),  hdout_fmeas = numeric(), look_roc = numeric(), kdeos_roc = numeric(), rdos_roc = numeric())

file_list <- list.files(folder)

for(i in 1:length(file_list)){ 
  dat <- readMat(paste(folder, file_list[i], sep=""))

  # lookout
  lookobj <- lookout::lookout(dat$X, alpha=0.1)
  look_scores <- lookobj$outlier_scores
  lookoutlabs <- rep(0, dim(dat$X)[1])
  lookoutlabs[lookobj$outliers[ ,1]] <- 1
  lookoutput <- diff_metrics(dat$y[ ,1], lookoutlabs)
  lookrocobj <- roc(dat$y[ ,1], look_scores, direction="<" )
  nn <- dim(dat$X)[1]

  # KDEOS
  kdeos_scores <- tryCatch(DDoutlier::KDEOS(dat$X), error = function(e){
    print(sys.calls())
    kdeos_scores <- rep(NA, nn)
    return(kdeos_scores)})
  kdeosroc_obj <- tryCatch(roc(dat$y[ ,1], kdeos_scores, direction="<"), error = function(e){
    print(sys.calls())
    kdeosroc_obj <- list()
    kdeosroc_obj$auc <- NA
    return(kdeosroc_obj)})
  kdeos_roc <- kdeosroc_obj$auc

  # RDOS
  rdos_scores <- tryCatch(DDoutlier::RDOS(dat$X), error = function(e){
    print(sys.calls())
    rdos_scores <- rep(NA, nn)
    return(rdos_scores)})
  rdosroc_obj <- tryCatch(roc(dat$y[ ,1], rdos_scores, direction="<"), error = function(e){
    print(sys.calls())
    rdosroc_obj <- list()
    rdosroc_obj$auc <- NA
    return(rdosroc_obj)})
  rdos_roc <- rdosroc_obj$auc

  # stray
  strayout <- stray::find_HDoutliers(dat$X, knnsearchtype = "kd_tree", alpha=0.1)
  straylabs <- rep(0, dim(dat$X)[1])
  straylabs[strayout$outliers] <- 1
  strayoutput <- diff_metrics(dat$y[ ,1], straylabs)

  # HDOUTLIERS
  hdoutobj <- HDoutliers(dat$X, alpha=0.1)
  hdoutlabs <- rep(0, dim(dat$X)[1])
  hdoutlabs[hdoutobj] <- 1
  hdoutput <- diff_metrics(dat$y[ ,1], hdoutlabs)

  output[i, ] <- c(lookoutput$gmean, strayoutput$gmean, hdoutput$gmean, lookoutput$fmeasure, strayoutput$fmeasure, hdoutput$fmeasure, lookrocobj$auc, kdeos_roc, rdos_roc)

}


## ----ODDResults, dependson="ODDSdatasetswork",  fig.cap="", out.width="100%", message=FALSE, echo=FALSE------------
output <- cbind.data.frame(file_list, output)
output[ ,2:dim(output)[2]] <- round(output[ ,2:dim(output)[2]], 2)
output[ ,1] <- substring(output[ ,1], 1, nchar(output[ ,1])-4)

headcols <- data.frame(c1 = c("", "Gmean", "FMeasure", "AUC"), c2=c(1, 3, 3, 3))
options(knitr.kable.NA = '')
output %>%
  knitr::kable(
    col.names = c("Filename", "lookout", "stray", "HDoutliers", "lookout", "stray", "HDoutliers", "lookout", "KDEOS", "RDOS"),
    booktabs = TRUE,
    caption = "Performance evaluation of 12 datasets in the ODDS repository."
  ) %>%
  kableExtra::add_header_above(header=headcols) %>%
  kableExtra::row_spec(1:dim(output)[1], color = "blue") %>%
  kableExtra::kable_styling(font_size=10)


## ----lvplots, fig.cap="Letter value plots of performance differences between 1. lookout and HDoutliers, 2. lookout and stray using Gmean and Fmeasure.", fig.height=4, message=FALSE, out.width="80%", fig.align='center'----

# The following file is at https://github.com/sevvandi/Paper-lookout/tree/master/lookout/data_repo_output
dat <- readr::read_csv(here::here("data_repo_output/Collated_EX2_Take1_1_alpha_point05.csv"))

# Differences - gmean
sensitivity <- select(dat, contains("sensitivity"))
specificity <- select(dat, contains("specificity"))
gmean <- sqrt(sensitivity * specificity) %>%
  rename_all(function(x){stringr::str_extract(x, "[a-z]*")}) %>%
  as_tibble() %>%
  bind_cols(dat) %>%
  filter(lookout > 0 | hdoutliers > 0 | stray > 0) %>%
  mutate(
    look_hd = lookout - hdoutliers,
    look_stray = lookout - stray,
  ) %>%
  pivot_longer(look_hd:look_stray, names_to = "Algorithm", values_to = "Gmean")
g1 <- ggplot(gmean, aes(x = Algorithm, y = Gmean)) +
  lvplot::geom_lv(aes(fill = ..LV..),
                  width.method = "area", width = 0.3, show.legend=FALSE) +
  lvplot::scale_fill_lv() +
  labs(x = "Algorithm", y = "Gmean")

# Precision and recall
true_pos <- select(dat, contains("true_pos"))
false_pos <- select(dat, contains("false_pos"))
false_neg <- select(dat, contains("false_neg"))
precision <- as_tibble(true_pos / (true_pos + false_pos)) %>%
  rename_all(function(x){stringr::str_extract(x, "[a-z]*")}) %>%
  replace_na(list(lookout = 0, hdoutliers = 0, stray = 0))
recall <- as_tibble(true_pos / (true_pos + false_neg)) %>%
  rename_all(function(x){stringr::str_extract(x, "[a-z]*")})
fmeas <- as_tibble(2 * precision * recall / (precision + recall)) %>%
  replace_na(list(lookout = 0, hdoutliers = 0, stray = 0)) %>%
  bind_cols(dat) %>%
  filter(lookout > 0 | hdoutliers > 0 | stray > 0) %>%
  mutate(
    look_hd = lookout - hdoutliers,
    look_stray = lookout - stray,
  ) %>%
  pivot_longer(look_hd:look_stray, names_to = "Algorithm", values_to = "Fmeasure")

g2 <- ggplot(fmeas, aes(x = Algorithm, y = Fmeasure)) +
  lvplot::geom_lv(aes(fill = ..LV..), width.method = "area", width = 0.2) +
  lvplot::scale_fill_lv() +
  labs(x = "Algorithm", y = "F measure")

#g1 | g2


## ----table, dependson='lvplots', message=FALSE---------------------------------------------------------------------
stats <- bind_rows(
    gmean %>%
      select(Algorithm, Gmean) %>%
      rename(value = Gmean) %>%
      mutate(Metric = "Gmean"),
    fmeas %>%
      select(Algorithm, Fmeasure) %>%
      rename(value = Fmeasure) %>%
      mutate(Metric = "Fmeasure"),
  ) %>%
  group_by(Algorithm, Metric) %>%
  summarise(
    median = median(value),
    mean = mean(value),
    sd = sd(value),
    n = n()
  ) %>%
  mutate(
    lo = mean - 1.96 * sd/sqrt(n),
    hi = mean + 1.96 * sd/sqrt(n)
  )
tab <- stats %>%
  mutate(
    CI = paste0("(",sprintf("%.4f",lo),",",sprintf("%.4f",hi),")"),
    Mean = sprintf("%.4f", mean),
    Median = sprintf("%.4f", median)
  ) %>%
  pivot_longer(c(Median, Mean, CI), names_to = "Statistic") %>%
  select(Algorithm, Metric, Statistic, value) %>%
  pivot_wider(names_from=Algorithm, values_from = value) %>%
  arrange(desc(Metric), desc(Statistic)) %>%
  mutate(
    Statistic = recode(Statistic, CI = "95% Confidence Interval")
  )
tab %>%
  select(-Metric) %>%
  knitr::kable(
    col.names = c("Statistic","lookout - HDoutliers", "lookout - stray"),
    booktabs = TRUE,
    caption = "Summary statistics for comparing lookout with the HDoutliers and stray algorithms."
  ) %>%
  kableExtra::pack_rows(index = table(tab$Metric)) %>%
  kableExtra::kable_styling(font_size=10)

gmean <- stats %>%
  filter(Metric == "Gmean") %>%
  arrange(Algorithm) %>%
  pull(median)
fmeas <- stats %>%
  filter(Metric == "Fmeasure") %>%
  arrange(Algorithm) %>%
  pull(median)

