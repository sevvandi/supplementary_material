# --------------------------------------------------------------------
# RUNNING LOOKOUT ON DIFFERENT MODELS - GEOMETRIC METHOD   ***********
# TASK  1 :  TRY lookout ON THE RESIDUAL DATA - ETS
# TASK  1 :  TRY lookout ON THE RESIDUAL DATA - TSLM
# TASK  2 :  TRY lookout ON LIGHTGBM MODEL DATA
# TASK  3 :  TRY lookout ON ZERO-INFLATED MODEL DATA
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# TASK  1 :  TRY lookout ON THE RESIDUAL DATA - ETS
# --------------------------------------------------------------------

library(plyr)
library(ggplot2)
library(tidyverse)

library(hts)
library(fable)
library(fpp3)
library(lookout)

fore_res <- readRDS("Data_Output/Mahdi/fore_res.rds")
head(fore_res)

hist(fore_res$fore_res)
min(fore_res$fore_res, na.rm = TRUE)

fore_res_ets <- fore_res %>%
  filter(.model == 'ets_str', nodes != '<aggregated>')
head(fore_res_ets)

# ----------------------------------------------------------
#  Anomaly Window = 1 WEEK, the same as forecasting window
tmax <- max(fore_res_ets$time)
tmin <- min(fore_res_ets$time)
st <- tmin
win <- 24*7
en <- st + win
out_all <- c()
anomaly_stats <- c()

while(en < tmax){
  res_df <- fore_res_ets %>%
    filter(time >= st & time < en )
  res_t <- res_df %>%
    pull(fore_res)

  num_unique_nodes <- dim(res_df[ ,1] %>% distinct())[1]

  inds <- which(abs(res_t) > 10)
  if(length(inds) > 0){
    res_sub <- res_t[inds]
    lookobj <- lookout::lookout(res_sub, alpha = 0.1, fast = FALSE)
    if(NROW(lookobj$outliers) > 0 ){
      out_df <- as_tibble(res_df[inds[lookobj$outliers[ ,1]], ] ) %>%
        mutate(end_time = en)
      out_all <- out_all %>% bind_rows(out_df)

      # Anomaly stats
      num_all_anomalies <- NROW(lookobj$outliers)
      num_unique_anomalies <- dim(res_df[inds[lookobj$outliers[ ,1]], 1] %>% distinct())[1]
    }else{
      # Anomaly stats
      num_unique_anomalies <- num_all_anomalies <- 0
    }
    anom_stats <- data.frame(dim(res_df)[1], num_unique_nodes, num_all_anomalies, num_unique_anomalies)
    anomaly_stats <- anomaly_stats %>% bind_rows(anom_stats)
  }

  st <- st + win
  en <- en + win
}
colnames(anomaly_stats) <- c("Number_of_ARP_Calls", "Number_Unique_Nodes", "Number_Anomaly_Calls", "Number_Unique_Anomalies")
saveRDS(out_all, file = "Data_Output/To_Make_Public/ETS_STR_All_outliers_1_week_moving_window_alpha_point1.rds")
write_csv(anomaly_stats, "Data_Output/To_Make_Public/ETS_STR_Anomaly_stats_1_week_moving_window.csv")


# --------------------------------------------------------------------
# TASK  2 :  TRY lookout ON THE RESIDUAL DATA - YEARLY DATA - TSLM
# --------------------------------------------------------------------

library(plyr)
library(ggplot2)
library(tidyverse)

library(hts)
library(fable)
library(fpp3)
library(lookout)

fore_res <- readRDS("Data_Output/Mahdi/fore_tslm_res.RDS")

head(fore_res)
table(fore_res$.model)

hist(fore_res$fore_res)
min(fore_res$fore_res, na.rm = TRUE)

fore_res_tslm <- fore_res %>%
  filter(.model == 'tslm_str', nodes != '<aggregated>')
head(fore_res_tslm)
unique(fore_res_tslm$nodes)

# ----------------------------------------------------------
#  Anomaly Window = 1 WEEK, the same as forecasting window
tmax <- max(fore_res_tslm$time)
tmin <- min(fore_res_tslm$time)
st <- tmin
win <- 24*7
en <- st + win
out_all <- c()
anomaly_stats <- c()

while(en < tmax){
  res_df <- fore_res_tslm %>%
    filter(time >= st & time < en )
  res_t <- res_df %>%
    pull(fore_res)

  num_unique_nodes <- dim(res_df[ ,1] %>% distinct())[1]

  inds <- which(abs(res_t) > 10)
  if(length(inds) > 0){
    res_sub <- res_t[inds]
    lookobj <- lookout::lookout(res_sub, alpha = 0.1, fast = TRUE)
    if(NROW(lookobj$outliers) > 0 ){
      out_df <- as_tibble(res_df[inds[lookobj$outliers[ ,1]], ] ) %>%
        mutate(end_time = en)
      out_all <- out_all %>% bind_rows(out_df)

      # Anomaly stats
      num_all_anomalies <- NROW(lookobj$outliers)
      num_unique_anomalies <- dim(res_df[inds[lookobj$outliers[ ,1]], 1] %>% distinct())[1]
    }else{
      # Anomaly stats
      num_unique_anomalies <- num_all_anomalies <- 0
    }
    anom_stats <- data.frame(dim(res_df)[1], num_unique_nodes, num_all_anomalies, num_unique_anomalies)
    anomaly_stats <- anomaly_stats %>% bind_rows(anom_stats)
  }

  st <- st + win
  en <- en + win
}
colnames(anomaly_stats) <- c("Number_of_ARP_Calls", "Number_Unique_Nodes", "Number_Anomaly_Calls", "Number_Unique_Anomalies")
saveRDS(out_all, file = "Data_Output/To_Make_Public/TSLM_STR_All_outliers_1_week_moving_window_alpha_point1.rds")
write_csv(anomaly_stats, "Data_Output/To_Make_Public/TSLM_STR_Anomaly_stats_1_week_moving_window.csv")

# --------------------------------------------------------------------
# TASK  3 :  TRY lookout ON LIGHTGBM DATA
# --------------------------------------------------------------------

fore_res <- readRDS("Data_Output/Mahdi_Big/fore_new_res2.RDS")
head(fore_res)
unique(fore_res$hf_model)

fore_res_lgbm <- fore_res %>%
  filter(hf_model == 'Mint-Shr',fore_model =="Lightgbm", node != 0)  # '<aggregated>' = 0 for this set
head(fore_res_lgbm)
table(fore_res_lgbm$fore_model)

# ----------------------------------------------------------
#  Anomaly Window = 1 WEEK, the same as forecasting window
tmax <- max(fore_res_lgbm$time)
tmin <- min(fore_res_lgbm$time)
st <- tmin
win <- 24*7
en <- st + win
out_all <- c()
anomaly_stats <- c()

while(en < tmax){
  res_df <- fore_res_lgbm %>%
    filter(time >= st & time < en )
  res_t <- res_df %>%
    pull(fore_res)

  num_unique_nodes <- dim(res_df[ ,2] %>% distinct())[1]

  inds <- which(abs(res_t) > 10)
  if(length(inds) > 0){
    res_sub <- res_t[inds]
    lookobj <- lookout::lookout(res_sub, alpha = 0.1, fast = FALSE, unitize = FALSE)
    if(NROW(lookobj$outliers) > 0 ){
      out_df <- as_tibble(res_df[inds[lookobj$outliers[ ,1]], ] ) %>%
        mutate(end_time = en)
      out_all <- out_all %>% bind_rows(out_df)

      # Anomaly stats
      num_all_anomalies <- NROW(lookobj$outliers)
      num_unique_anomalies <- dim(res_df[inds[lookobj$outliers[ ,1]], 1] %>% distinct())[1]
    }else{
      # Anomaly stats
      num_unique_anomalies <- num_all_anomalies <- 0
    }
    anom_stats <- data.frame(dim(res_df)[1], num_unique_nodes, num_all_anomalies, num_unique_anomalies)
    anomaly_stats <- anomaly_stats %>% bind_rows(anom_stats)
  }

  st <- st + win
  en <- en + win
}
colnames(anomaly_stats) <- c("Number_of_ARP_Calls", "Number_Unique_Nodes", "Number_Anomaly_Calls", "Number_Unique_Anomalies")
saveRDS(out_all, file = "Data_Output/To_Make_Public/Lightgbm_Mint_Shr_All_outliers_1_week_moving_window_alpha_point1.rds")
write_csv(anomaly_stats, "Data_Output/To_Make_Public/Lightgbm_Mint_Shr_Anomaly_stats_1_week_moving_window.csv")


# --------------------------------------------------------------------
# TASK  4 :  TRY lookout ON ZERO-INFLATED MODEL DATA
# --------------------------------------------------------------------

fore_res <- readRDS("Data_Output/Mahdi_Big/fore_new_res2.RDS")
head(fore_res)
unique(fore_res$hf_model)


fore_res_zero <- fore_res %>%
  filter(hf_model == 'Mint-Shr',fore_model =="Zero-Inflated", node != 0)  # '<aggregated>'
head(fore_res_zero)
table(fore_res_zero$hf_model)

# ----------------------------------------------------------
#  Anomaly Window = 1 WEEK, the same as forecasting window
tmax <- max(fore_res_zero$time)
tmin <- min(fore_res_zero$time)
st <- tmin
win <- 24*7
en <- st + win
out_all <- c()
anomaly_stats <- c()

while(en < tmax){
  res_df <- fore_res_zero %>%
    filter(time >= st & time < en )
  res_t <- res_df %>%
    pull(fore_res)

  num_unique_nodes <- dim(res_df[ ,2] %>% distinct())[1]

  inds <- which(abs(res_t) > 10)
  if(length(inds) > 0){
    res_sub <- res_t[inds]
    lookobj <- lookout::lookout(res_sub, alpha = 0.1, fast = FALSE, unitize = FALSE)
    if(NROW(lookobj$outliers) > 0 ){
      out_df <- as_tibble(res_df[inds[lookobj$outliers[ ,1]], ] ) %>%
        mutate(end_time = en)
      out_all <- out_all %>% bind_rows(out_df)

      # Anomaly stats
      num_all_anomalies <- NROW(lookobj$outliers)
      num_unique_anomalies <- dim(res_df[inds[lookobj$outliers[ ,1]], 1] %>% distinct())[1]
    }else{
      # Anomaly stats
      num_unique_anomalies <- num_all_anomalies <- 0
    }
    anom_stats <- data.frame(dim(res_df)[1], num_unique_nodes, num_all_anomalies, num_unique_anomalies)
    anomaly_stats <- anomaly_stats %>% bind_rows(anom_stats)
  }

  st <- st + win
  en <- en + win
}
colnames(anomaly_stats) <- c("Number_of_ARP_Calls", "Number_Unique_Nodes", "Number_Anomaly_Calls", "Number_Unique_Anomalies")
saveRDS(out_all, file = "Data_Output/To_Make_Public/Zeroinflated_Mint_Shr_All_outliers_1_week_moving_window_alpha_point1.rds")
write_csv(anomaly_stats, "Data_Output/To_Make_Public/Zeroinflated_Mint_Shr_Anomaly_stats_1_week_moving_window.csv")



