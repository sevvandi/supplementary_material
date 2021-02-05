# ---------------------------------------------------------------------
# TASK 1 - ANNULUS, TDA DIAMETER AND KNN DISTANCES
# TASK 2 - TWO CIRCLES, NORMAL DISTRIBUTION
# TASK 3 - OUTLIER PERSISTENCE
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# TASK 1 - ANNULUS, TDA DIAMETER AND KNN DISTANCES
# ---------------------------------------------------------------------

library(ggplot2)
library(RANN)
library(TDAstats)

set.seed(1)
oo <- 10
outer_radius <- 1
inner_radius <- 0.7
n <- 1000
rho <- sqrt(abs(rnorm(n,mean=5,sd=1)))
theta <- runif(n, 0, 2*pi)
x <- rho * cos(theta)
y <- rho * sin(theta)
X <- cbind(x,y)
opoints <- cbind(rnorm(oo, mean=0, sd=0.2), rnorm(oo, mean=0, sd=0.2))
X <- rbind(X, opoints)

X <- as.data.frame(X)
g1 <- ggplot(X, aes(x,y)) + geom_point() + theme_bw()
# g1

phom <- calculate_homology(X, dim=0)
g2 <- plot_barcode(phom)
# g2
deathtimes <- phom[ ,3]

df1 <- cbind.data.frame("TDA", deathtimes)
colnames(df1) <- c("Type", "Distance")

distances <- as.matrix(dist(X))
dist_vec <- as.vector(distances)
df2 <- cbind.data.frame("ALL", dist_vec)
colnames(df2) <- colnames(df1)
df <- rbind.data.frame(df1, df2)

# KNN distances
nn_obj <- RANN::nn2(X, k=11)
len <- dim(nn_obj$nn.dists)[1]
for(kk in c(2, 6, 11)){
  dist_df <- cbind.data.frame(rep(paste("NN_", (kk-1), sep=""),len) ,nn_obj$nn.dists[ ,kk])
  colnames(dist_df) <- c("NN", "Distance")
  if(kk==2){
    dist_df_all <- dist_df
  }else{
    dist_df_all <- rbind.data.frame(dist_df_all, dist_df)
  }
}
colnames(dist_df_all) <- colnames(df1)
df <- rbind.data.frame(df1, dist_df_all)

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}
g3 <- ggplot(df, aes(x=Type, y=Distance, fill=Type)) + geom_violin(trim=FALSE) + stat_summary(fun.data=data_summary) +scale_fill_brewer(palette="Dark2")+ xlab("Distance Type") +  theme_bw()


ll <- dim(phom)[1]
phom2 <- phom[(ll-20):ll, ]
g4 <- plot_barcode(phom2) + geom_vline(xintercept =0.436, lty=2)
# g4

gridExtra::grid.arrange(
  g1, g2, g3, g4,
  ncol = 2, nrow = 2,
  layout_matrix = rbind(c(1,2), c(3,4)))


# ---------------------------------------------------------------------
# TASK 2 - TWO CIRCLES, NORMAL DISTRIBUTION
# ---------------------------------------------------------------------
library(ggtda)
library(ggforce)
X <- matrix(c(0, 0, 1, 0), ncol=2, byrow = TRUE)
colnames(X) <- c("x", "y")
X <- as.data.frame(X)
prox <- 1
g5 <- ggplot(X, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = prox/2, fill = "aquamarine3") +
  geom_point() + geom_segment(x=0, y=0, xend=1, yend=0, arrow= arrow(length = unit(0.03, "npc"))) + geom_text(x=0.5, y=0.05, label="1")
# g5

x <- seq(-2, 2, length=1000)
y <- dnorm(x, mean=0, sd=1)
Y <- cbind.data.frame(x,y)
colnames(X) <- c("x0", "y0")
g6 <- ggplot(Y, aes(x,y))+ geom_line() + geom_point(x=0, y=0) + geom_point(x=1, y=0) + geom_segment(x=0, y=0, xend=1, yend=0) + coord_fixed(ratio=4) + geom_vline(xintercept = 0, lty=2) +  geom_segment(x=1, y=-0.15, xend=1, yend=0.23, lty=2) + geom_ellipse(aes(x0=0,y0=0,a=0.5, b=0.125, angle=0), inherit.aes = FALSE)+  geom_ellipse(aes(x0=1,y0=0,a=0.5, b=0.125, angle=0), inherit.aes = FALSE) +theme_bw()
# g6


x <- seq(-2, 2, length=1000)
y <- sqrt(4-x^2)
Y <- cbind.data.frame(x,y)
g7 <- ggplot(Y, aes(x,y))+ geom_line() + geom_point(x=0, y=0) + geom_point(x=1, y=0) + geom_segment(x=0, y=0, xend=1, yend=0) + coord_fixed(ratio=1) + geom_vline(xintercept = 0, lty=2) +  geom_segment(x=1, y=-0.5, xend=1, yend=1.7, lty=2) + geom_circle(aes(x0 = 0, y0 = 0, r = 0.5), inherit.aes = FALSE) + geom_circle(aes(x0 = 1, y0 = 0, r = 0.5), inherit.aes = FALSE) + theme_bw()


gridExtra::grid.arrange(
  g5, g6, g7,
  ncol = 2, nrow = 2,
  layout_matrix = rbind(c(1,1), c(2,3)))


# ---------------------------------------------------------------------
# TASK 3 - OUTLIER PERSISTENCE
# ---------------------------------------------------------------------
set.seed(1)
oo <- 5
outer_radius <- 1
inner_radius <- 0.7
n <- 1000
rho <- sqrt(abs(rnorm(n,mean=5,sd=1)))
theta <- runif(n, 0, 2*pi)
x <- rho * cos(theta)
y <- rho * sin(theta)
X <- cbind(x,y)
opoints <- cbind(rnorm(oo, mean=0, sd=0.2), rnorm(oo, mean=0, sd=0.2))
X <- rbind(X, opoints)

inds <- c(1001,1002,1003,1004,1005)
oplabs <- cbind.data.frame(X[inds, ], inds)
colnames(oplabs) <- c("x", "y", "lab")
rightones <- c(1,3,5)
leftones <- c(2,4)
oplabs[rightones, 1] <- oplabs[rightones, 1] + 0.5
oplabs[leftones, 1] <- oplabs[leftones, 1] - 0.5

upones <- c(2,3, 6,7,8)
downones <- c(1,4)
oplabs[upones, 2] <- oplabs[upones, 2] + 0.2
oplabs[downones, 2] <- oplabs[downones, 2] - 0.2

X <- as.data.frame(X)
g1 <- ggplot(X, aes(x,y)) + geom_point() + geom_text(data=oplabs, aes(x, y, label=lab))  + theme_bw()
# g1


lookobj <- lookout(X)
outnew <- persisting_outliers(X, num_steps = 20)
g2 <- autoplot(outnew, alpha=0.05) + geom_vline(xintercept = lookobj$bandwidth, linetype="dashed")
# g2

gridExtra::grid.arrange(g1, g2,
  ncol = 2, nrow = 1)


autoplot(outnew) + geom_vline(xintercept = lookobj$bandwidth, linetype="dashed")


