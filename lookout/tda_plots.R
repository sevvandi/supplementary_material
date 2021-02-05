# ---------------------------------------------------------------------
# TASK 1 - EXAMPLE 1
# TASK 2 - EXAMPLE 2
# TASK 3 - EXAMPLE 3
# ---------------------------------------------------------------------
library(ggplot2)
library(ggtda)
library(gridExtra)
library(TDAstats)
# library(cowplot)
# ---------------------------------------------------------------------
# TASK 1 - EXAMPLE 1
# ---------------------------------------------------------------------
p <- c(0,0,0)
q <- c(0.2, 0.2)
r <- c(0, 0.2)
s <- c(0.1, 0)
t <- c(0.75, 0.75)
d <- rbind.data.frame(p,q,r,s,t)
rownames(d) <- c("p","q", "r", "s", "t")
colnames(d) <-c("x", "y")

# compute the persistent homology
ph <- as.data.frame(TDAstats::calculate_homology(as.matrix(d), dim = 1))
print(head(ph, n = 12))
ph <- transform(ph, dim = as.factor(dimension))
prox <- 0.5


p1 <- ggplot(d, aes(x = x, y = y, label=rownames(d))) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = prox/2, fill = "aquamarine3") +
  geom_point() + geom_text(nudge_x=0.08, nudge_y=0.08) +
  stat_vietoris0() +
  stat_vietoris1(diameter = prox, alpha = .25) +
  stat_vietoris2(diameter = prox, fill = "darkgoldenrod")
# 300, 300 - saving width and height

set.seed(1)
x1 <- rnorm(8)
x2 <- rnorm(8)
X <- cbind.data.frame(x2, x1)

# compute the persistent homology
ph <- as.data.frame(TDAstats::calculate_homology(as.matrix(X), dim = 1))
print(head(ph, n = 12))
ph <- transform(ph, dim = as.factor(dimension))

colnames(X) <- c("x", "y")

prox <- 4/3
p2 <- ggplot(X, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = prox/2, fill = "aquamarine3") +
  geom_point() +
  stat_vietoris0() +
  stat_vietoris1(diameter = prox, alpha = .25) +
  stat_vietoris2(diameter = prox, fill = "darkgoldenrod")
# 650, 453 - saving width and height

gridExtra::grid.arrange(
  p1, p2,
  layout_matrix = matrix(c(1, 2), nrow = 1)
)


# ---------------------------------------------------------------------
# TASK 3 - EXAMPLE 2
# ---------------------------------------------------------------------

set.seed(1)
outer_radius <- 1
inner_radius <- 0.7
n <- 50
rho <- sqrt(runif(n,inner_radius^2,outer_radius^2))
theta <- runif(n, 0, 2*pi)
x <- rho * cos(theta)
y <- rho * sin(theta)
plot(x, y, pch=20)
X <- cbind.data.frame(x,y)


# compute the persistent homology
ph1 <- TDAstats::calculate_homology(as.matrix(X), dim = 1)
# plot topological barcode
bcd <- plot_barcode(ph1)
bcd
# plot persistence diagram
pst <- plot_persist(ph1)
pst + xlab("Birth") + ylab("Death")

# plot_grid(bcd, pst, align = "v", nrow = 1, rel_heights = c(0.5, 2))
gridExtra::grid.arrange(
  bcd, pst,
  ncol = 2, nrow = 1)

# ---------------------------------------------------------------------
# TASK 3 - EXAMPLE 3
# ---------------------------------------------------------------------


set.seed(1)
outer_radius <- 1
inner_radius <- 0.7
n <- 50
rho <- sqrt(runif(n,inner_radius^2,outer_radius^2))
theta <- runif(n, 0, 2*pi)
x <- rho * cos(theta)
y <- rho * sin(theta)
X <- cbind.data.frame(x,y)
ph <- TDAstats::calculate_homology(as.matrix(X), dim = 1)
ph <- as.data.frame(ph)

print(head(ph, n = 12))
ph <- transform(ph, dim = as.factor(dimension))

colnames(X) <- c("x", "y")

prox <- 0.1
p3 <- ggplot(X, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = prox/2, fill = "aquamarine3") +
  geom_point() +
  stat_vietoris0() +
  stat_vietoris1(diameter = prox, alpha = .25) +
  stat_vietoris2(diameter = prox, fill = "darkgoldenrod")

prox <- 0.3
p4 <- ggplot(X, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = prox/2, fill = "aquamarine3") +
  geom_point() +
  stat_vietoris0() +
  stat_vietoris1(diameter = prox, alpha = .25) +
  stat_vietoris2(diameter = prox, fill = "darkgoldenrod")

prox <- 0.8
p5 <- ggplot(X, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = prox/2, fill = "aquamarine3") +
  geom_point() +
  stat_vietoris0() +
  stat_vietoris1(diameter = prox, alpha = .25) +
  stat_vietoris2(diameter = prox, fill = "darkgoldenrod")


prox <- 1.5
p6 <- ggplot(X, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = prox/2, fill = "aquamarine3") +
  geom_point() +
  stat_vietoris0() +
  stat_vietoris1(diameter = prox, alpha = .25) +
  stat_vietoris2(diameter = prox, fill = "darkgoldenrod")

# 650, 453 - saving width and height

gridExtra::grid.arrange(
  p3, p4, p5, p6,
  ncol = 2, nrow = 2,
  layout_matrix = rbind(c(1,2), c(3,4)))

