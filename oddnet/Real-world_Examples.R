# -------------------------------------------------------------------------------
# Example 1: US Senate Dataset
# Example 2: UCI Message Dataset
# Example 3: Canadian Voting Dataset
# Example 4: Physics Citations Dataset
# Example 5: US Presidential Blogposts
# -------------------------------------------------------------------------------


library(fpp3)
library(GGally)
library(lookout)
library(pcaPP)
library(VCERGM)
library(igraph)
library(rlang)
library(tnet)
library(dnr)
library(ggraph)
library(gridExtra)
library(latex2exp)
# -------------------------------------------------------------------------------
# Example 1: US Senate Dataset
# -------------------------------------------------------------------------------
networks = Rollcall$networks
attr = Rollcall$attr

anomalous_1 <- oddnet::anomalous_networks(networks, vert_attr = TRUE, attr_name = "party", attr_mat = attr)
anomalous_1$outliers[ ,1] + 39   # Because the first data point is the 40th Congress
anomalous_1
# > anomalous_1
# Leave-out-out KDE outliers using lookout algorithm
#
# Call: lookout::lookout(X = dfpca[, 1:dd], alpha = alpha)
#
# Outliers Probability
# 1       61 0.008693675


# Plotting
network_40 <- networks[[1]]
gr40 <- igraph::graph_from_adjacency_matrix(network_40)
gr40 <- set_vertex_attr(gr40, "party", value = attr[[1]])
plot(gr40, vertex.color = c("blue", "red")[as.numeric(as.factor(vertex_attr(gr40, "party")))])

layout <- create_layout(gr40, layout = 'igraph', algorithm = 'nicely')
g1 <- ggraph(layout) +
  geom_edge_link() +
  geom_node_point(aes(color = party)) +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("blue", "red")) +
  ggtitle(TeX("$40^{th}$ Congress"))

#g1

tt <- 25
networkt <- networks[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
grt <- set_vertex_attr(grt, "party", value = attr[[tt]])
layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g2 <- ggraph(layout) +
  geom_edge_link() +
  geom_node_point(aes(color = party)) +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("blue", "red")) +
  ggtitle(TeX("$64^{th}$ Congress"))

#g2

tt <- 50
networkt <- networks[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
grt <- set_vertex_attr(grt, "party", value = attr[[tt]])
layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g3 <- ggraph(layout) +
  geom_edge_link() +
  geom_node_point(aes(color = party)) +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("blue", "red")) +
  ggtitle(TeX("$89^{th}$ Congress"))

#g3

network_113 <- networks[[74]]
gr113 <- igraph::graph_from_adjacency_matrix(network_113)
gr113 <- set_vertex_attr(gr113, "party", value = attr[[74]])
plot(gr113, vertex.color = c("blue", "red")[as.numeric(as.factor(vertex_attr(gr113, "party")))])

layout <- create_layout(gr113, layout = 'igraph', algorithm = 'nicely')
#create_layout(gr40, layout = 'kk')
g4 <- ggraph(layout) +
  geom_edge_link() +
  geom_node_point(aes(color = party)) +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("blue", "red")) +
  ggtitle(TeX("$113^{th}$ Congress"))

#g4

gridExtra::grid.arrange(g1, g2, g3, g4, nrow = 2)

anomalous_1$outliers
anom_scores <- (1 - anomalous_1$outlier_probability)*10
Congress <- 40:113
df_anom <- data.frame(Congress = Congress, Scores = anom_scores, Probability = anomalous_1$outlier_probability)

ggplot(df_anom, aes(Congress, Probability)) +
  geom_line() +
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  ylab("Conditional Probability")


# -------------------------------------------------------------------------------
# Example 2: UCI Message Dataset
# -------------------------------------------------------------------------------

data(tnet)
dat <- tnet::OnlineSocialNetwork.n1899.lnet
head(dat)
dat$t <-  as_datetime(dat$t)
dat$date <- as_date(dat$t)
inds <- which(dat$i == dat$j)
length(inds)
dat2 <- dat

dat3 <- dat2[ ,c("i", "j", "date")]
len <-  length(unique(dat3$date))
unique_dates <- unique(dat3$date)
num_networks <- length(unique_dates)


mat_list <- list()
for(i in 1:num_networks){
  nn <- unique_dates[i]
  inds <- which( dat3$date == nn )
  datwin <- dat3[inds, 1:2]
  gr <- graph_from_data_frame(datwin)
  mat_list[[i]] <- as_adjacency_matrix(gr)
}

anomalous_2 <- oddnet::anomalous_networks(mat_list, na_action = 0)
anomalous_2
# > anomalous_2
# Leave-out-out KDE outliers using lookout algorithm
#
# Call: lookout::lookout(X = dfpca[, 1:dd], alpha = alpha)
#
# Outliers  Probability
# 1       39 8.785092e-05
# 2       45 0.000000e+00
# 3       63 4.642616e-02
# 4      164 8.785092e-05

unique_dates[anomalous_2$outliers[ ,1]]

tt <- 6
networkt <- mat_list[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g1 <- ggraph(layout) +
  geom_edge_link(edge_colour = "grey66",
                 arrow = arrow(
                 angle = 10,
                 length = unit(0.1, "inches"),
                 ends = "last",
                 type = "closed"
                   )) +
  geom_node_point(col = "black", show.legend = FALSE) +
  scale_size(range = c(3, 11)) +
  theme_graph() +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  ggtitle(paste("Day", tt))

g1

tt <- 15
networkt <- mat_list[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g2 <- ggraph(layout) +
  geom_edge_link(edge_colour = "grey66",
                 arrow = arrow(
                   angle = 10,
                   length = unit(0.1, "inches"),
                   ends = "last",
                   type = "closed"
                 )) +
  geom_node_point(col = "black", show.legend = FALSE) +
  scale_size(range = c(3, 11)) +
  theme_graph() +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  ggtitle(paste("Day", tt))

g2


tt <- 45
networkt <- mat_list[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g3 <- ggraph(layout) +
  geom_edge_link(edge_colour = "grey66",
                 arrow = arrow(
                   angle = 10,
                   length = unit(0.1, "inches"),
                   ends = "last",
                   type = "closed"
                 )) +
  geom_node_point(col = "black", show.legend = FALSE) +
  scale_size(range = c(3, 11)) +
  theme_graph() +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  ggtitle(paste("Day", tt))

g3


tt <- 75
networkt <- mat_list[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g4 <- ggraph(layout) +
  geom_edge_link(edge_colour = "grey66",
                 arrow = arrow(
                   angle = 10,
                   length = unit(0.1, "inches"),
                   ends = "last",
                   type = "closed"
                 )) +
  geom_node_point(col = "black", show.legend = FALSE) +
  scale_size(range = c(3, 11)) +
  theme_graph() +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  ggtitle(paste("Day", tt))

g4


tt <- 105
networkt <- mat_list[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g5 <- ggraph(layout) +
  geom_edge_link(edge_colour = "grey66",
                 arrow = arrow(
                   angle = 10,
                   length = unit(0.1, "inches"),
                   ends = "last",
                   type = "closed"
                 )) +
  geom_node_point(col = "black", show.legend = FALSE) +
  scale_size(range = c(3, 11)) +
  theme_graph() +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  ggtitle(paste("Day", tt))

g5

tt <- 155
networkt <- mat_list[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g6 <- ggraph(layout) +
  geom_edge_link(edge_colour = "grey66",
                 arrow = arrow(
                   angle = 10,
                   length = unit(0.1, "inches"),
                   ends = "last",
                   type = "closed"
                 )) +
  geom_node_point(col = "black", show.legend = FALSE) +
  scale_size(range = c(3, 11)) +
  theme_graph() +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  ggtitle(paste("Day", tt))

g6

gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, nrow = 2)


df_anom <- data.frame(Time = unique_dates,  Probability = anomalous_2$outlier_probability)
ggplot(df_anom, aes(Time, Probability)) +
  geom_line() +
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  ylab("Conditional Probability")




# -------------------------------------------------------------------------------
# Example 3: Canadian Voting Dataset
# -------------------------------------------------------------------------------

dat <- read.csv("Data_Input/Canadian_Votes_Edgelist.csv", header = FALSE)
unique(dat$V2)
colnames(dat) <- c("Year", "From", "To", "Weight")
years <- unique(dat$Year)
num_years <- length(years)

mat_list <- list()

for(i in 1:num_years){
  yearr <- years[i]
  datwin <- dat[dat$Year == yearr, 2:3]
  gr <- graph_from_data_frame(datwin)
  mat_list[[i]] <- as_adjacency_matrix(gr)
}

anomalous_3 <- anomalous_networks(mat_list, na_action = 0, alpha = 0.05)
anomalous_3


# > anomalous_3
# Leave-out-out KDE outliers using lookout algorithm
#
# Call: lookout::lookout(X = dfpca[, 1:dd], alpha = alpha)
#
# Outliers Probability
# 1        6           0
# 2       10           0

tt <- 1
yearr <- tt +  2005
networkt <- mat_list[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g1 <- ggraph(layout) +
  geom_edge_link(edge_colour = "grey66") +
  geom_node_point(col = "black", show.legend = FALSE) +
  scale_size(range = c(3, 11)) +
  theme_graph() +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  ggtitle(yearr)
#g1

tt <- 3
yearr <- tt +  2005
networkt <- mat_list[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g2 <- ggraph(layout) +
  geom_edge_link(edge_colour = "grey66") +
  geom_node_point(col = "black", show.legend = FALSE) +
  scale_size(range = c(3, 11)) +
  theme_graph() +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  ggtitle(yearr)
# g2


tt <- 6
yearr <- tt +  2005
networkt <- mat_list[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g3 <- ggraph(layout) +
  geom_edge_link(edge_colour = "grey66") +
  geom_node_point(col = "black", show.legend = FALSE) +
  scale_size(range = c(3, 11)) +
  theme_graph() +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  ggtitle(yearr)
# g3


tt <- 10
yearr <- tt +  2005
networkt <- mat_list[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g4 <- ggraph(layout) +
  geom_edge_link(edge_colour = "grey66") +
  geom_node_point(col = "black", show.legend = FALSE) +
  scale_size(range = c(3, 11)) +
  theme_graph() +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  ggtitle(yearr)
# g4


tt <- 12
yearr <- tt +  2005
networkt <- mat_list[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g5 <- ggraph(layout) +
  geom_edge_link(edge_colour = "grey66") +
  geom_node_point(col = "black", show.legend = FALSE) +
  scale_size(range = c(3, 11)) +
  theme_graph() +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  ggtitle(yearr)
# g5


tt <- 14
yearr <- tt +  2005
networkt <- mat_list[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g6 <- ggraph(layout) +
  geom_edge_link(edge_colour = "grey66") +
  geom_node_point(col = "black", show.legend = FALSE) +
  scale_size(range = c(3, 11)) +
  theme_graph() +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  ggtitle(yearr)
# g6


gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, nrow = 2)

years <- 2006:2019
df_anom <- data.frame(Time = years,  Probability = anomalous_3$outlier_probability)
ggplot(df_anom, aes(Time, Probability)) +
  geom_line() +
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  ylab("Conditional Probability")



# -------------------------------------------------------------------------------
# Example 4: US Presidential Blogposts
# -------------------------------------------------------------------------------
data(rdNets)
party_name <- c(rep("Democrat", 34), rep("Republican", 13))
party_name[23] <- "common"
networks <- rdNets

num_networks <- length(networks)
mat_list <- list()
attr <- list()

k <- 1
# Networks 345 and 346 are empty.
for(i in 1:num_networks){
  if(class(networks[[i]]) == 'network'){
    mat_list[[k]] <- as.matrix.network.adjacency(networks[[i]])
    attr[[k]] <- party_name
    k <- k +1
  }
}

anomalous_5 <- oddnet::anomalous_networks(mat_list, vert_attr = TRUE, attr_name = "party", attr_mat = attr)
anomalous_5

# Leave-out-out KDE outliers using lookout algorithm
#
# Call: lookout::lookout(X = dfpca[, 1:dd], alpha = alpha)
#
# Outliers Probability
# 1       37  0.04101475
# 2      246  0.02977197

dates1 <- seq(as.Date("2004-07-22"), as.Date("2004-11-19"), by = 1, each = 4 )
dates <- rep(dates1, each = 4 )
dates
num_networks

dates <- dates[ -c(345,346)]  # The networks corresponding to these two entries are empty

tt <- 10
day <- dates[tt]
networkt <- mat_list[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
grt <- igraph::set_vertex_attr(grt, "party", value = attr[[tt]])

layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g1 <- ggraph(layout) +
  geom_edge_link() +
  geom_node_point(aes(color = party), size = 2) +
  geom_edge_link(edge_colour = "grey66") +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("yellow", "blue", "red")) +
  ggtitle(day)

#g1

tt <- 100
day <- dates[tt]
networkt <- mat_list[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
grt <- set_vertex_attr(grt, "party", value = attr[[tt]])

layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g2 <- ggraph(layout) +
  geom_edge_link() +
  geom_node_point(aes(color = party), size = 2) +
  geom_edge_link(edge_colour = "grey66") +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("yellow", "blue", "red")) +
  ggtitle(day)

#g2


tt <- 150
day <- dates[tt]
networkt <- mat_list[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
grt <- set_vertex_attr(grt, "party", value = attr[[tt]])

layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g3 <- ggraph(layout) +
  geom_edge_link() +
  geom_node_point(aes(color = party), size = 2) +
  geom_edge_link(edge_colour = "grey66") +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("yellow", "blue", "red")) +
  ggtitle(day)

#g3


tt <- 246
day <- dates[tt]
networkt <- mat_list[[tt]]
grt <- igraph::graph_from_adjacency_matrix(networkt)
grt <- set_vertex_attr(grt, "party", value = attr[[tt]])

layout <- create_layout(grt, layout = 'igraph', algorithm = 'nicely')
g4 <- ggraph(layout) +
  geom_edge_link() +
  geom_node_point(aes(color = party), size = 2) +
  geom_edge_link(edge_colour = "grey66") +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank() ) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("yellow", "blue", "red")) +
  ggtitle(day)

#g4


gridExtra::grid.arrange(g1, g2, g3, g4, nrow = 2)

df_anom <- data.frame(Time = dates,  Probability = anomalous_5$outlier_probability)
ggplot(df_anom, aes(Time, Probability)) +
  geom_line() +
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  ylab("Conditional Probability")

dates[anomalous_5$outliers[ ,1]]
dates[which(df_anom$Probability < 0.1)]


