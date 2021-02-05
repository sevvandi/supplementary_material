# ---------------------------------------------------------------------
# TASK 0 - COLOR PALETTE
# TASK 1 - EXAMPLE 1
# TASK 2 - EXAMPLE 2
# TASK 3 - EXAMPLE 3
# TASK 4 - EXAMPLE 4
# TASK 5 - EXAMPLE 5
# ---------------------------------------------------------------------

library(ggplot2)
library(tidyr)
library(lookout)

# ---------------------------------------------------------------------
# TASK 0 - COLOR PALETTE
# ---------------------------------------------------------------------
col_pal1 <- c("white", "#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026")

col_pal2 <- col_pal1 <- c("grey", "#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026")

# ---------------------------------------------------------------------
# TASK 1 - EXAMPLE 1
# ---------------------------------------------------------------------
set.seed(1)
x1 <- cbind.data.frame(rnorm(500), rnorm(500))
x2 <- cbind.data.frame(rnorm(5, mean=10, sd=0.2), rnorm(5, mean=10, sd=0.2))
colnames(x1) <- colnames(x2) <- c("x", "y")
X <- rbind.data.frame(x1, x2)

outnew <- persisting_outliers(X, unitize=FALSE)
g2 <- autoplot(outnew)  + geom_vline(xintercept = outnew$lookoutbw, linetype="dashed") + theme(legend.position = "none")
g2

lookobj <- lookout(X, unitize=FALSE, alpha=0.1)
strength <- (0.1 - lookobj$outlier_probability)/0.01
strength[strength < 0] <- 0
X2 <- cbind.data.frame(X, strength)
g1 <- ggplot(X2, aes(x,y)) + geom_point(aes(color = strength)) + scale_colour_gradientn(colours=col_pal2) + theme_bw() + theme(legend.position = "none")
g1


# ---------------------------------------------------------------------
# TASK 2 - EXAMPLE 2
# ---------------------------------------------------------------------
set.seed(1)
x1 <- cbind.data.frame(rnorm(500, mean=-10), rnorm(500))
x2 <- cbind.data.frame(rnorm(500, mean= 10), rnorm(500))
x3 <- cbind.data.frame(rnorm(5, sd=0.2), rnorm(5, sd=0.2))
colnames(x1) <- colnames(x2) <- colnames(x3) <- c("x", "y")
X <- rbind.data.frame(x1, x2, x3)

outnew <- persisting_outliers(X, unitize=FALSE)
g4 <- autoplot(outnew) + geom_vline(xintercept = outnew$lookoutbw, linetype="dashed") + ylab("")  + theme(legend.position = "none")
g4

lookobj <- lookout(X, unitize=FALSE, alpha=0.1)
strength <- (0.1 - lookobj$outlier_probability)/0.01
strength[strength < 0] <- 0
X2 <- cbind.data.frame(X, strength)
g3 <- ggplot(X2, aes(x,y)) + geom_point(aes(color = strength)) + scale_colour_gradientn(colours=col_pal2)  + theme_bw() + ylab("")  + theme(legend.position = "none")
g3


# ---------------------------------------------------------------------
# TASK 3 - EXAMPLE 3
# ---------------------------------------------------------------------
set.seed(1)
x1 <- cbind.data.frame(rnorm(500), rnorm(500))
x2 <- cbind.data.frame(rnorm(100, sd=0.7), rnorm(100, mean=8, sd=0.7))
x3 <- cbind.data.frame(rnorm(100, mean=8 , sd=0.7), rnorm(100, sd=0.7))
x4 <- cbind.data.frame(rnorm(3, mean=6, sd=0.2), rnorm(3, mean=6, sd=0.2))
colnames(x1) <- colnames(x2) <- colnames(x3) <- colnames(x4) <- c("x", "y")
X <- rbind.data.frame(x1, x2, x3, x4)

outnew <- persisting_outliers(X, unitize=FALSE)
g6 <- autoplot(outnew) + ylab("")  + geom_vline(xintercept = outnew$lookoutbw, linetype="dashed") + theme(legend.position = "none")
g6

lookobj <- lookout(X, unitize=FALSE, alpha=0.1)
strength <- (0.1 - lookobj$outlier_probability)/0.01
strength[strength < 0] <- 0
X2 <- cbind.data.frame(X, strength)
g5 <- ggplot(X2, aes(x,y)) + geom_point(aes(color = strength)) + ylab("")  + scale_colour_gradientn(colours=col_pal2) + theme_bw() + theme(legend.position = "none")
g5

# ---------------------------------------------------------------------
# TASK 4 - EXAMPLE 4
# ---------------------------------------------------------------------
set.seed(456)
x1 <- cbind.data.frame(rnorm(500), rnorm(500))
x2 <- cbind.data.frame(rnorm(100, sd=0.7), rnorm(100, mean=8, sd=0.7))
x3 <- cbind.data.frame(rnorm(100, mean=8 , sd=0.7), rnorm(100, sd=0.7))
x4 <- rbind.data.frame(c(5,5), c(8,7.5), c(12,4))
colnames(x1) <- colnames(x2) <- colnames(x3) <- colnames(x4) <- c("x", "y")
X <- rbind.data.frame(x1, x2, x3, x4)

outnew <- persisting_outliers(X, unitize=FALSE)
g8 <- autoplot(outnew) + geom_vline(xintercept = outnew$lookoutbw, linetype="dashed") + ylab("")  + theme(legend.position = "none")
g8

lookobj <- lookout(X, unitize=FALSE, alpha=0.1)
strength <- (0.1 - lookobj$outlier_probability)/0.01
strength[strength < 0] <- 0
X2 <- cbind.data.frame(X, strength)
g7 <- ggplot(X2, aes(x,y)) + geom_point(aes(color = strength)) + ylab("")  + scale_colour_gradientn(colours=col_pal2) + theme_bw()  + theme(legend.position = "none")
g7


# ---------------------------------------------------------------------
# TASK 5 - EXAMPLE 5
# ---------------------------------------------------------------------
set.seed(1)
x1 <- rnorm(1000, sd=0.2)
x2 <- x1^2 + rnorm(1000, sd=0.01)
x <- cbind.data.frame(x1, x2)
x3 <- rbind.data.frame(c(0,0.3), c(-0.2, 0.4), c(0.4, 0.5) )
colnames(x) <- colnames(x3) <- c("x", "y")
X <- rbind.data.frame(x, x3)

outnew <- persisting_outliers(X, unitize=FALSE)
g10 <- autoplot(outnew) + geom_vline(xintercept = outnew$lookoutbw, linetype="dashed") + ylab("")
g10

lookobj <- lookout(X, unitize=FALSE, alpha=0.1)
strength <- (0.1 - lookobj$outlier_probability)/0.01
strength[strength < 0] <- 0
X2 <- cbind.data.frame(X, strength)
g9 <- ggplot(X2, aes(x,y)) + geom_point(aes(color = strength)) + ylab("")  + scale_colour_gradientn(colours=col_pal2) + theme_bw()
g9


gridExtra::grid.arrange(
  g1, g3, g5, g7, g9, g2, g4, g6, g8, g10,
  ncol = 5, nrow = 2,
  layout_matrix = rbind(c(1,2, 3, 4, 5), c(6, 7, 8, 9, 10)), widths=c(1,1,1,1,1.8))

