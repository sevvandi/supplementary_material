## Hierarchical time series for network

library(plyr)
library(ggplot2)


## reading network data, one hour

load("adjacency_matrix_list_1hr.RData")
load("adjacency_matrix_list_5min.RData")


load("adjacency_matrix_list_1hr_broadcast_fixed.RData")
load("adjacency_matrix_list_5min_broadcast_fixed.RData")

load("adjacency_matrix_list_6hr_broadcast_fixed.Rdata")

data_one_hour <- adjacency_list
data_five_mins <- adjacency_list_5min

data_one_hour_bc <- adjacency_list
data_five_min_bc <- adjacency_list_5min
data_six_hour_bc <- adjacency_list_6hr


from_mat_all_ts <- to_mat_all_ts<-  NULL
#from_vec_ts <- to_vec_ts <- NULL

for (j in 1:dim(data_six_hour_bc[[1]])[1]) {
  for (i in 1:length(data_six_hour_bc)) {
     from_mat_all_ts <- rbind(from_mat_all_ts, cbind(t(as.matrix(data_six_hour_bc[[i]])[j,]), t=i, node=j))
     to_mat_all_ts <- rbind(to_mat_all_ts, cbind(t(as.matrix(data_six_hour_bc[[i]])[,j]), t=i, node=j))
     }
}

saveRDS(from_mat_all_ts, "from_mat_all_ts.rds")
saveRDS(to_mat_all_ts, "to_mat_all_ts.rds")


from_vec_total_ts <- to_vec_total_ts <- NULL
from_i_mat_middle<- to_j_mat_middle <- NULL
for (j in 1:dim(data_six_hour_bc[[1]])[1]) {
    from_ij_mat_bottom <- from_mat_all_ts %>% as_tibble() %>% filter(node==j) %>% select(-"t",-"node")
    from_i_vec <- rowSums(from_ij_mat_bottom)
    from_i_mat_middle <- rbind(from_i_mat_middle,cbind(t(from_i_vec),node=j))

    to_ij_mat_bottom <- to_mat_all_ts %>% as_tibble() %>% filter(node==j) %>% select(-"t",-"node")
    to_j_vec <- rowSums(to_ij_mat_bottom)
    to_j_mat_middle <- rbind(to_j_mat_middle,cbind(t(to_j_vec),node=j))

    #to_mat <- to_mat_all_ts %>% as_tibble() %>% filter(node==j) %>% select(-"t",-"node")
    #to_vec <- colSums(to_mat)
    #to_vec_total_ts <- rbind(to_vec_total_ts,cbind(t(to_vec),node=j))
  }
saveRDS(from_mat_all_ts, "from_mat_all_ts.rds")
saveRDS(to_mat_all_ts, "to_mat_all_ts.rds")


## building the hierarchies
library(hts)
library(fable)
library(fpp3)


from_mat_hts <- hts(t(from_i_mat_middle[,-32]))
from_mat_hts %>% aggts(levels=0) %>%
  autoplot(facet=TRUE) +
  xlab("Time(6_hourly)") + ylab("From") + ggtitle("Network outputs(from)")

to_mat_hts <-  hts(t(to_j_mat_middle[,-32]))
to_mat_hts %>% aggts(levels=0) %>%
  autoplot(facet=TRUE) +
  xlab("Time(6_hourly)") + ylab("To") + ggtitle("Network inputs(to)")

## the hts below is the difference between from and to matrices, just to explore.(does it amke sense?)
from_to_diff_hts <-  hts(t(from_i_mat_middle[,-32]) - t(to_j_mat_middle[,-32]))
from_to_diff_hts %>% aggts(levels=0) %>%
  autoplot(facet=TRUE) +
  xlab("Time(6_hourly)") + ylab("To") + ggtitle("Network inputs(from-to difference)")


## Total from and Total to seems to be the same, however, from one node to another node is not symmetric.
## 6 hourly data
View(from_mat_hts$bts)
View(to_mat_hts$bts)

# difference between from and to on a specific node
plot.ts(from_mat_hts$bts[,1] - to_mat_hts$bts[,1])
plot.ts(from_mat_hts$bts[,10] - to_mat_hts$bts[,10])
plot.ts(from_mat_hts$bts[,100] - to_mat_hts$bts[,100])
plot.ts(from_mat_hts$bts[,200] - to_mat_hts$bts[,200])


## predicting from matrices after building the long vector.
from_vec_total_tsible <- from_i_mat_middle[,-dim(from_i_mat_middle)[2]] %>%
  t() %>% as_tibble() %>%
  pivot_longer(names_to = "nodes", values_to = "Signals", cols=contains("V")) %>%
  mutate(time=rep(1:dim(from_i_mat_middle[,-dim(from_i_mat_middle)[2]])[2], each=dim(from_i_mat_middle)[1]))


# fit_bench <- from_vec_total_tsible %>%
#   as_tsibble(index = time, key = nodes) %>%
#   model(ets_bench= ETS(Signals),
#         croston_bench = CROSTON(Signals, opt_crit = c("mae"),type = c("sbj")),
#         nnetar_bench = NNETAR(Signals))



#in the model below when I use mint_shrink for the reconciliation method, it returns error due to
# not having positivide difinite matrix.
# fit_reconciled <- fit_bench %>%
#   reconcile(
#    #ets_td = top_down(ets_bench),
#    ets_bu = bottom_up(ets_bench),
#    #ets_shr = min_trace(ets_bench, method = "mint_shrink"),
#    ets_str = min_trace(ets_bench, method = "wls_struct"),
#
#    #croston_td = top_down(croston_bench),
#    croston_bu = bottom_up(croston_bench),
#    #croston_shr = min_trace(croston_bench, method = "mint_shrink"),
#    croston_str = min_trace(croston_bench, method = "wls_struct"),
#
#    #nnetar_td = top_down(nnetar_bench),
#    nnetar_bu = bottom_up(nnetar_bench),
#    #nnetar_shr = min_trace(nnetar_bench, method = "mint_shrink"),
#    nnetar_str = min_trace(nnetar_bench, method = "wls_struct")
#
#   )


## aggregated series from nodes
from_vec_total_tsible_nodes <- from_vec_total_tsible %>%
  as_tsibble(index = time, key = nodes) %>%
  aggregate_key(nodes, Signals = sum(Signals))

from_fc_bench <- from_vec_total_tsible_nodes %>%
  model(ets_bench= ETS(Signals),
        #croston_bench = CROSTON(Signals, opt_crit = c("mae"),type = c("sbj")),
        nnetar_bench = NNETAR(Signals)) %>%
  reconcile(ets_bu = bottom_up(ets_bench),
            ets_td = top_down(ets_bench),
            ets_str = min_trace(ets_bench, method = "wls_struct"),

            nnetar_bu = bottom_up(nnetar_bench),
            nnetar_td = top_down(nnetar_bench),
            nnetar_str = min_trace(nnetar_bench, method = "wls_struct")) %>%
  forecast(h=1)

from_fc_bench <- from_fc_bench %>%
  mutate(across(where(is.numeric), ~if_else(.<0, 0, .)))

from_fc_bench %>%
  filter(is_aggregated(nodes)) %>%
  #filter(.model=="ets_td") %>%
  autoplot(from_vec_total_tsible_nodes %>% filter(is_aggregated(nodes))) +
  labs(y = "Signals")# +
  #facet_wrap(vars(nodes), scales = "free_y")

from_acc <- from_fc_bench %>%
  #filter(.model=="nnetar_str") %>%
  accuracy(data = from_vec_total_tsible_nodes,
           measures = list(mase = MASE,
                           ss = skill_score(CRPS)))

## predicting to matrices after building the long vector.
to_vec_total_tsible <- to_j_mat_middle[,-dim(to_j_mat_middle)[2]] %>%
  t() %>% as_tibble() %>%
  pivot_longer(names_to = "nodes", values_to = "Signals", cols=contains("V")) %>%
  mutate(time=rep(1:dim(to_j_mat_middle[,-dim(to_j_mat_middle)[2]])[2], each=dim(to_j_mat_middle)[1]))


to_vec_total_tsible_nodes <- to_vec_total_tsible %>%
  as_tsibble(index = time, key = nodes) %>%
  aggregate_key(nodes, Signals = sum(Signals))

to_fit_bench <- to_vec_total_tsible_nodes %>%
  filter(time<28) %>%
  model(ets_bench= ETS(Signals),
        croston_bench = CROSTON(Signals, opt_crit = c("mae"),type = c("sbj")),
        nnetar_bench = NNETAR(Signals)) %>%
  reconcile(ets_bu = bottom_up(ets_bench),
            ets_td = top_down(ets_bench),
            ets_str = min_trace(ets_bench, method = "wls_struct"),

            croston_bu = bottom_up(croston_bench),
            croston_td = top_down(croston_bench),
            croston_str = min_trace(croston_bench, method = "wls_struct"),

            nnetar_bu = bottom_up(nnetar_bench),
            nnetar_td = top_down(nnetar_bench),
            nnetar_str = min_trace(nnetar_bench, method = "wls_struct"))

to_fc_bench <- to_fit_bench %>% forecast(h=4)

to_fc_bench <- to_fc_bench %>%
  mutate(across(where(is.numeric), ~if_else(.<0, 0, .)))

to_acc <- to_fc_bench %>%
  #filter(.model=="nnetar_str") %>%
  accuracy(data = to_vec_total_tsible_nodes,
           measures = list(mase = MASE,
                           ss = skill_score(CRPS)))
  #) %>%
  #group_by(.model) %>%
  #summarise(mase = mean(mase), sspc = mean(ss) * 100)

save.image("first_analysis.Rdata")
