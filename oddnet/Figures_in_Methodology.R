# ------------------------------------------------------------------------------
# TASK    1 : EXAMPLES IN METHODOLOGY
# TASK    2 : EXPLAINING WHY TIME SERIES ANOMALIES ARE NOT FEATURE ANOMALIES
# TASK    3 : EXAMPLES OF ERDOS-RENYI, BARABASI AND WATTS-STROGATS GRAPHS
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# TASK    1 : EXAMPLES IN METHODOLOGY
# ------------------------------------------------------------------------------
library(igraph)
library(ggraph)
library(gridExtra)
library(dplyr)
library(tidyr)

# Example 1
set.seed(1)
gr1 <- erdos.renyi.game(n = 50 , p = 0.05)
ecount(gr1)
mean(degree(gr1))

gr2 <- erdos.renyi.game(n = 50 , p = 0.05)
ecount(gr2)
mean(degree(gr2))
gr3 <- erdos.renyi.game(n = 52 , p = 0.2)
ecount(gr3)
mean(degree(gr3))


layout <- create_layout(gr1, layout = 'kk')
g1 <- ggraph(layout) +
  geom_edge_link() +
  geom_node_point(aes(color = "blue")) +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("")
# g1

layout <- create_layout(gr2, layout = 'kk')
g2 <- ggraph(layout) +
  geom_edge_link() +
  geom_node_point(aes(color = "blue")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("")
# g2

layout <- create_layout(gr3, layout = 'kk')
g3 <- ggraph(layout) +
  geom_edge_link() +
  geom_node_point(aes(color = "blue")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("")
# g3

grid.arrange(g1, g2, g3, nrow = 1)

set.seed(123)
df <- data.frame(Edges = double(), Nodes = double())
for(i in 1:19){
  nodes <- sample(50:55, 1)
  gr <- erdos.renyi.game(n = nodes , p = 0.05)
  df[i, ] <- c(ecount(gr), length(degree(gr)) )
}
df[20, ] <-c(ecount(gr3), length(degree(gr3)))

df$label <- c(rep("Normal", 19), "Anomaly")
df$Experiment <- rep("EX1")
ggplot(df, aes(x = Edges, y = Nodes, color = label)) +
  geom_point() +
  theme_bw()


# -------------------------------------------------------------------
# Example 2
set.seed(1)
numnodes <- sample(50:55, 1)
mat1 <- matrix(0, nrow = numnodes, ncol = numnodes)
mat1[sample(numnodes*numnodes, 100)] <- 1
gr1 <- graph_from_adjacency_matrix(mat1)

numnodes <- sample(50:55, 1)
mat2 <- matrix(0, nrow = numnodes, ncol = numnodes)
mat2[sample(numnodes*numnodes, 100)] <- 1
gr2 <- graph_from_adjacency_matrix(mat2)

numnodes <- sample(50:55, 1)
mat3 <- matrix(0, nrow = numnodes, ncol = numnodes)
mat3[1:50, 30] <- 1
mat3[30, 1:51] <- 1
gr3 <- graph_from_adjacency_matrix(mat3)

set.seed(1)
df2 <- data.frame(Edges = double(), Nodes = double())
for(i in 1:19){
  nummodes <- sample(50:55, 1)
  mat <- matrix(0, nrow = nummodes, ncol = nummodes)
  mat[sample(nummodes*nummodes, 100)] <- 1
  gr <- graph_from_adjacency_matrix(mat)
  df2[i, ] <- c(ecount(gr), length(degree(gr)) )
}
df2[20, ] <- c(ecount(gr3), length(degree(gr3)))
df2$label <- c(rep("Normal", 19), "Anomaly")
df2$Experiment <- rep("EX2")

ggplot(df2, aes(x = Edges, y = Nodes, color = label)) +
  geom_point() +
  theme_bw()

df <- bind_rows(df, df2)
ggplot(df, aes(x = Edges, y = Nodes, color = label)) +
  geom_point() +
  facet_wrap(~Experiment) +
  theme_bw()

layout <- create_layout(gr1, layout = 'kk')
g1 <- ggraph(layout) +
  geom_edge_link() +
  geom_node_point(aes(color = "blue")) +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("")
# g1

layout <- create_layout(gr2, layout = 'kk')
g2 <- ggraph(layout) +
  geom_edge_link() +
  geom_node_point(aes(color = "blue")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("")
# g2

layout <- create_layout(gr3, layout = 'kk')
g3 <- ggraph(layout) +
  geom_edge_link() +
  geom_node_point(aes(color = "blue")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("")
# g3

grid.arrange(g1, g2, g3, nrow = 1)



# ------------------------------------------------------------------------------
# TASK    1 : EXPLAINING WHY TIME SERIES ANOMALIES ARE NOT FEATURE ANOMALIES
# ------------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)
library(ggExtra)
library(fable)
library(fabletools)

set.seed(1)
t <- seq(1:100)
x <- t + rnorm(100, sd = 2)
x[60] <- x[60] -20
labs <- rep("Normal", 100)
labs[60] <- "Anomaly"

df <- data.frame(time = t, feature = x, fixed = 1, label = labs)
g1 <- ggplot(df, aes(x = time, y = feature)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Feature Value") +
  xlab("Time")

fillcol <- rep("lightblue", 49 )
fillcol[20] <- "red"
ggMarginal(g1, margins = 'y', type="histogram", fill=fillcol, size=4, yparams = list(bins = 50))

xts <- as.ts(x)
mod <- as_tsibble(xts) %>%
  fabletools::model(arima = fable::ARIMA(value))

dfresiduals <- fabletools::augment(mod)
g2 <- autoplot(dfresiduals, .resid) +
  geom_point() +
  xlab("Time") +
  ylab("Residuals") +
  theme_bw()


g2
fillcol <- rep("lightblue", 49 )
fillcol[1] <- "red"
ggMarginal(g2, margins ='y', type = "histogram", fill=fillcol, size=4, yparams = list(bins = 50))

# ------------------------------------------------------------------------------
# TASK    3 : EXAMPLES OF ERDOS-RENYI, BARABASI AND WATTS-STROGATZ GRAPHS
# ------------------------------------------------------------------------------
library(igraph)
library(ggraph)
library(gridExtra)

set.seed(1)
gr1 <- erdos.renyi.game(100, p.or.m = 0.02)
layout <- create_layout(gr1, layout = 'kk')
g1 <- ggraph(layout) +
  geom_edge_link2() +
  geom_node_point(aes(color = "blue")) +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  ggtitle("Erdos-Renyi")
# g1


gr2 <- igraph::barabasi.game(100, power = 1.2)
layout <- create_layout(gr2, layout = 'kk')
g2 <- ggraph(layout) +
  geom_edge_link2() +
  geom_node_point(aes(color = "blue")) +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  ggtitle("Barabasi-Albert")
#g2


gr3 <- igraph::sample_smallworld(1, 100, 5, 0.02)
layout <- create_layout(gr3, layout = 'kk')
g3 <- ggraph(layout) +
  geom_edge_link2() +
  geom_node_point(aes(color = "blue")) +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  ggtitle("Watts-Strogatz")
# g3

grid.arrange(g1, g2, g3, nrow = 1)
