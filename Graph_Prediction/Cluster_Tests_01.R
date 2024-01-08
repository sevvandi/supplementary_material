# This test is to check different parameter settings of generate_graph function
# Parameters del_edge , new_nodes , edge_increase
# del_edge is the proportion of edges deleted from each graph to next
# new_nodes is the proportion of new nodes from each graph to next
# edge_increase is the proportion of edges increased from one graph to next

library(igraph)
library(dplyr)
library(fable)

source(file="Functions_For_Cluster.R")


sequ <- seq(0.1, 0.9, by = 0.1)
values <- matrix(0, nrow = 729, ncol = 3)
ll <- 1
for(i in 1:9){
  for(j in 1:9){
    for(k in 1:9){
      values[ll, ] <- c(i, j, k)/10
      ll <- ll +1
    }
  }
}

args=(commandArgs(TRUE))
print(args)
row_num <- as.numeric(args)
print(row_num)

if(length(args)==0){
  print("No arguments supplied.")
  ##supply default values
  break;
  row_index <- 10
}else{
  row_index <- values[row_num, ]
}
del_edge_val <- row_index[3]
new_nodes_val <- row_index[1]
edge_increase_val <- row_index[2]


set.seed(2023)
graphlist <- list()
graphlist[[1]] <- gr <-  sample_pa(5, directed = FALSE)
for(i in 2:35){
  gr <-  generate_graph(gr, del_edge = del_edge_val, new_nodes = new_nodes_val, edge_increase = edge_increase_val )
  graphlist[[i]] <- gr
}


for(ll in 1:5){
  tt <- 19 + ll
  for(h in 1:10){
    time_step <- h
    grpred <- predict_graph(graphlist[1:tt],
                            conf_level2 = 55,
                            weights_opt = 4,
                            h = time_step)
    eval1 <- eval_metrics2(graphlist[[(tt+time_step)]], grpred$graph_mean)
    evaldf <- eval1 %>%
      as.data.frame() %>%
      mutate(pred_nodes = vcount(grpred$graph_mean),
             actual_nodes = vcount(graphlist[[(tt+time_step)]]),
             pred_edges = ecount(grpred$graph_mean),
             actual_edges = ecount(graphlist[[(tt+time_step)]]),
             time_step = time_step,
             graphs_in_training = tt
      ) %>%
      relocate(actual_nodes, pred_nodes, actual_edges, pred_edges)
    if((ll == 1) & (h == 1)){
      evaldf_all <- evaldf
    }else{
      evaldf_all <- bind_rows(evaldf_all, evaldf)
    }

    eval0 <- eval_metrics(graphlist[[(tt+time_step)]], grpred$graph_mean)
    evaldf0 <- eval0 %>%
      as.data.frame() %>%
      mutate(pred_nodes = vcount(grpred$graph_mean),
             actual_nodes = vcount(graphlist[[(tt+time_step)]]),
             pred_edges = ecount(grpred$graph_mean),
             actual_edges = ecount(graphlist[[(tt+time_step)]]),
             time_step = time_step,
             graphs_in_training = tt
      ) %>%
      relocate(actual_nodes, pred_nodes, actual_edges, pred_edges)
    if((ll == 1) & (h == 1)){
      evaldf_all0 <- evaldf0
    }else{
      evaldf_all0 <- bind_rows(evaldf_all0, evaldf0)
    }

  }

}

# Reordered adjacency matrix
filename <- paste("Data_Output/Tests_01/Evaluation_Metrics_2_del_edge_", del_edge_val, "_new_nodes_", new_nodes_val, "_edge_increase_", edge_increase_val, "_row_num_", args[1], ".csv", sep="" )
write.csv(evaldf_all, filename, row.names = FALSE)

# Original adjacency matrix
filename <- paste("Data_Output/Tests_01/Evaluation_Metrics_1_del_edge_", del_edge_val, "_new_nodes_", new_nodes_val, "_edge_increase_", edge_increase_val, "_row_num_", args[1], ".csv", sep="" )
write.csv(evaldf_all0, filename, row.names = FALSE)

