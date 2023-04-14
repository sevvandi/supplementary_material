# --------------------------------------------------------------------
# TASK  1:   COMPARE WITH AUTOENCODERS
# TASK  2:   PLOT PRECISION, RECALL, FMEASURE AND FALSE POSITIVES PLOT
# TASK  3:   PLOT NUMBER OF ANOMALIES FOR EACH WINDOW
# TASK  4:   REPEATED ANOMALIES
# --------------------------------------------------------------------



# --------------------------------------------------------------------
# TASK  1:   COMPARE WITH AUTOENCODERS
# --------------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(stringr)
library(tidyr)

library(hts)
library(fable)
library(fpp3)
library(lookout)
library(h2o)
library(dplyr)

# Weekly data
dat_with_labs <- readRDS("Data_Output/To_Make_Public/Anomaly_labels_for_nodes_by_hour.rds")

# BU hierarchical method
out_all<- readRDS("Data_Output/To_Make_Public/All_outliers_1_week_moving_window_alpha_point1.rds")
out_all_tslm <- readRDS("Data_Output/To_Make_Public/TSLM_All_outliers_1_week_moving_window_alpha_point1.rds")
out_all_lightgbm <- readRDS("Data_Output/To_Make_Public/Lightgbm_All_outliers_1_week_moving_window_alpha_point1.rds")
out_all_zero <- readRDS("Data_Output/To_Make_Public/Zeroinflated_All_outliers_1_week_moving_window_alpha_point1.rds")

# # Geometrical - Mint Shr hierarchical method
# out_all<- readRDS("Data_Output/To_Make_Public/ETS_STR_All_outliers_1_week_moving_window_alpha_point1.rds")
# # out_all_tslm <- readRDS("Data_Output/To_Make_Public/TSLM_All_outliers_1_week_moving_window_alpha_point1.rds")
# out_all_lightgbm <- readRDS("Data_Output/To_Make_Public/Lightgbm_Mint_Shr_All_outliers_1_week_moving_window_alpha_point1.rds")
# out_all_zero <- readRDS("Data_Output/To_Make_Public/Zeroinflated_Mint_Shr_All_outliers_1_week_moving_window_alpha_point1.rds")


# General data with anomaly labels
dat_with_labs2 <- dat_with_labs %>%
  filter(hour >=1527) %>%
  mutate(week = ceiling((hour-1527)/(24*7))) %>%
  mutate(week = ifelse(week == 0, 1, week))

dat_with_labs3 <- dat_with_labs2 %>%
  select(node, hour, label)

# ETS anomalies
out_all2 <- out_all %>%
  mutate(week = ceiling((time - 1527)/(24*7))) %>%
  mutate(week = ifelse(week == 0, 1, week)) %>%
  rename(hour = time) %>%
  mutate(node = paste('N',str_pad(nodes, width=3, pad="0"), sep=""))

out_all3 <- out_all2 %>%
  select(node, hour) %>%
  mutate(preds = 1)

out_all4 <- left_join(out_all3, dat_with_labs3, by = c('node', 'hour'))

# TSLM Anomalies
out_all_tslm2 <- out_all_tslm %>%
  mutate(week = ceiling((time - 1527)/(24*7))) %>%
  mutate(week = ifelse(week == 0, 1, week)) %>%
  rename(hour = time) %>%
  mutate(node = paste('N',str_pad(nodes, width=3, pad="0"), sep=""))

out_all_tslm3 <- out_all_tslm2 %>%
  select(node, hour) %>%
  mutate(preds = 1)

# Lightgbm Anomalies
out_all_lightgbm2 <- out_all_lightgbm %>%
  filter(time >= 1527) %>%
  mutate(week = ceiling((time - 1527)/(24*7))) %>%
  mutate(week = ifelse(week == 0, 1, week)) %>%
  rename(hour = time, nodes = node) %>%
  mutate(node = paste('N',str_pad(nodes, width=3, pad="0"), sep=""))

out_all_lightgbm3 <- out_all_lightgbm2 %>%
  select(node, hour) %>%
  mutate(preds = 1)

# Zeroinflated Anomalies
out_all_zero2 <- out_all_zero %>%
  filter(time >= 1527) %>%
  mutate(week = ceiling((time - 1527)/(24*7))) %>%
  mutate(week = ifelse(week == 0, 1, week)) %>%
  rename(hour = time, nodes = node) %>%
  mutate(node = paste('N',str_pad(nodes, width=3, pad="0"), sep=""))

out_all_zero3 <- out_all_zero2 %>%
  select(node, hour) %>%
  mutate(preds = 1)

max_week <- max(dat_with_labs2$week)

# ----------------------------------------
# Separate code for autoencoder
# No need to run as part of the full thing
for(kk in 1:max_week){
  datwin <- dat_with_labs2 %>%
    filter(week == kk) %>%
    ungroup %>%
    select(count)
  autores <- autoencoder_AD2(datwin)

  datwin2 <- dat_with_labs2 %>%
    filter(week == kk) %>%
    ungroup %>%
    select(node, hour, count) %>%
    mutate(autoencoder = 0)
  datwin2[autores, 4] <- 1
  temp <- datwin2 %>%
    group_by(hour) %>%
    summarize(num_out = n())
  if(kk == 1){
    dat_anom_autoencoder <- temp
  }else{
    dat_anom_autoencoder <- bind_rows(dat_anom_autoencoder, temp)
  }

}
write_csv(dat_anom_autoencoder, "Data_Output/A2_TASK_5/Autoencoder_Anomalies.csv")
# ----------------------------------------


kk <- 1
zerolookout_diffmetrics <- lightgbmlookout_diffmetrics<- tslmlookout_diffmetrics <- etslookout_diffmetrics <- autoen_diffmetrics <- matrix(0, ncol = 13, nrow = max_week)
for(kk in 1:max_week){ #
  # Autoencoder
  datwin <- dat_with_labs2 %>%
    filter(week == kk) %>%
    ungroup %>%
    select(count)
  labels <-  dat_with_labs2 %>%
    filter(week == kk) %>%
    ungroup()  %>%
    pull(label)

  autores <- autoencoder_AD2(datwin)
  preds <- rep(0, dim(datwin)[1])
  preds[autores] <- 1
  actual <- labels
  autoen_diffmetrics[kk, ] <- c(kk, unlist(diff_metrics(actual, preds)))

  # ETS + Lookout
  anomalies_hts <- out_all2 %>%
    filter(week == kk) %>%
    mutate(preds = 1) %>%
    select(node, hour, preds)

  datwin_for_ets <- dat_with_labs2 %>%
    filter(week == kk) %>%
    group_by(node, hour) %>%
    left_join(anomalies_hts, by = c('node', 'hour') ) %>%
    replace_na(list(preds= 0))


  if(kk == 1){
    dat_ets_all <- datwin_for_ets
  }else{
    dat_ets_all <- bind_rows(dat_ets_all, datwin_for_ets)
  }

  actual <- datwin_for_ets %>%
    pull(label)
  predicted <- datwin_for_ets %>%
    pull(preds)
  etslookout_diffmetrics[kk, ] <- c(kk, unlist(diff_metrics(actual, predicted)))


  # TSLM + Lookout
  anomalies_tslm <- out_all_tslm2 %>%
    filter(week == kk) %>%
    mutate(preds = 1) %>%
    select(node, hour, preds)

  datwin_for_tslm <- dat_with_labs2 %>%
    filter(week == kk) %>%
    group_by(node, hour) %>%
    left_join(anomalies_tslm, by = c('node', 'hour') ) %>%
    replace_na(list(preds= 0))

  if(kk == 1){
    dat_tslm_all <- datwin_for_tslm
  }else{
    dat_tslm_all <- bind_rows(dat_tslm_all, datwin_for_tslm)
  }

  actual <- datwin_for_tslm %>%
    pull(label)
  predicted <- datwin_for_tslm %>%
    pull(preds)
  tslmlookout_diffmetrics[kk, ] <- c(kk, unlist(diff_metrics(actual, predicted)))


  # Lightgbm + lookout
  anomalies_lightgbm <- out_all_lightgbm2 %>%
    filter(week == kk) %>%
    mutate(preds = 1) %>%
    select(node, hour, preds)

  datwin_for_lightgbm <- dat_with_labs2 %>%
    filter(week == kk) %>%
    group_by(node, hour) %>%
    left_join(anomalies_lightgbm, by = c('node', 'hour') ) %>%
    replace_na(list(preds= 0))

  if(kk == 1){
    dat_lightgbm_all <- datwin_for_lightgbm
  }else{
    dat_lightgbm_all <- bind_rows(dat_lightgbm_all, datwin_for_lightgbm)
  }

  actual <- datwin_for_lightgbm %>%
    pull(label)
  predicted <- datwin_for_lightgbm %>%
    pull(preds)
  lightgbmlookout_diffmetrics[kk, ] <- c(kk, unlist(diff_metrics(actual, predicted)))

  # Zeroinflated + lookout
  anomalies_zero <- out_all_zero2 %>%
    filter(week == kk) %>%
    mutate(preds = 1) %>%
    select(node, hour, preds)

  datwin_for_zero <- dat_with_labs2 %>%
    filter(week == kk) %>%
    group_by(node, hour) %>%
    left_join(anomalies_zero, by = c('node', 'hour') ) %>%
    replace_na(list(preds= 0))

  if(kk == 1){
    dat_zero_all <- datwin_for_zero
  }else{
    dat_zero_all <- bind_rows(dat_zero_all, datwin_for_zero)
  }

  actual <- datwin_for_zero %>%
    pull(label)
  predicted <- datwin_for_zero %>%
    pull(preds)
  zerolookout_diffmetrics[kk, ] <- c(kk, unlist(diff_metrics(actual, predicted)))

}

colnames(zerolookout_diffmetrics)  <- colnames(lightgbmlookout_diffmetrics) <- colnames(tslmlookout_diffmetrics) <- colnames(etslookout_diffmetrics) <- colnames(autoen_diffmetrics) <- c("Week", names(diff_metrics(actual, preds)))

etslookout_diffmetrics2 <- etslookout_diffmetrics %>%
  as.data.frame() %>%
  mutate(fpr = false_pos/(false_pos + true_neg))

autoen_diffmetrics2 <- autoen_diffmetrics %>%
  as.data.frame() %>%
  mutate(fpr = false_pos/(false_pos + true_neg))

tslmlookout_diffmetrics2 <- tslmlookout_diffmetrics %>%
  as.data.frame() %>%
  mutate(fpr = false_pos/(false_pos + true_neg))

lightgbmlookout_diffmetrics2 <- lightgbmlookout_diffmetrics %>%
  as.data.frame() %>%
  mutate(fpr = false_pos/(false_pos + true_neg))

zerolookout_diffmetrics2 <- zerolookout_diffmetrics %>%
  as.data.frame() %>%
  mutate(fpr = false_pos/(false_pos + true_neg))

apply(etslookout_diffmetrics2, 2 ,mean)
apply(tslmlookout_diffmetrics2, 2 ,mean)
apply(autoen_diffmetrics2, 2 ,mean)
apply(lightgbmlookout_diffmetrics2, 2 ,mean)
apply(zerolookout_diffmetrics2, 2 ,mean)

# write_csv(etslookout_diffmetrics2, 'Data_Output/A2_TASK_5/Evaluation_metrics_ets_lookout.csv')
# write_csv(autoen_diffmetrics2, 'Data_Output/A2_TASK_5/Evaluation_metrics_autoencoder.csv')
# write_csv(tslmlookout_diffmetrics2, 'Data_Output/A2_TASK_5/Evaluation_metrics_tslm_lookout.csv')
# write_csv(lightgbmlookout_diffmetrics2, 'Data_Output/A2_TASK_5/Evaluation_metrics_lightgbm_lookout.csv')
# write_csv(zerolookout_diffmetrics2, 'Data_Output/A2_TASK_5/Evaluation_metrics_zero_inflated_lookout.csv')


# --------------------------------------------------------------------
# TASK  2:   PLOT PRECISION, RECALL, FMEASURE AND FALSE POSITIVES PLOT
# --------------------------------------------------------------------

hts_ets <- etslookout_diffmetrics2 %>%
  select(Week, false_pos, precision, recall, fmeasure) %>%
  rename(False_Positives = false_pos, Fmeasure = fmeasure, Precision = precision, Recall = recall) %>%
  pivot_longer(cols = 2:5) %>%
  mutate(Method = 'ets-lookout')

auto_en <- autoen_diffmetrics2 %>%
  select(Week, false_pos, precision, recall, fmeasure) %>%
  rename(False_Positives = false_pos, Fmeasure = fmeasure, Precision = precision, Recall = recall) %>%
  pivot_longer(cols = 2:5) %>%
  mutate(Method = 'autoencoder')

hts_tslm <- tslmlookout_diffmetrics2 %>%
  select(Week, false_pos, precision, recall, fmeasure) %>%
  rename(False_Positives = false_pos, Fmeasure = fmeasure, Precision = precision, Recall = recall) %>%
  pivot_longer(cols = 2:5) %>%
  mutate(Method = 'tslm-lookout')

hts_lightgbm <- lightgbmlookout_diffmetrics2 %>%
  select(Week, false_pos, precision, recall, fmeasure) %>%
  rename(False_Positives = false_pos, Fmeasure = fmeasure, Precision = precision, Recall = recall) %>%
  pivot_longer(cols = 2:5) %>%
  mutate(Method = 'lightgbm-lookout')

hts_zero <- zerolookout_diffmetrics2 %>%
  select(Week, false_pos, precision, recall, fmeasure) %>%
  rename(False_Positives = false_pos, Fmeasure = fmeasure, Precision = precision, Recall = recall) %>%
  pivot_longer(cols = 2:5) %>%
  mutate(Method = 'zero-lookout')

df <- bind_rows(hts_ets, auto_en, hts_tslm, hts_lightgbm, hts_zero)

ggplot(df, aes(x = Method, y = value, color = Method)) +
  geom_boxplot() +
  geom_jitter( size=0.4, alpha=0.9) +
  stat_summary(fun=mean, geom="point", shape=17, size = 3, color = rep(c("indianred3", "khaki4", "forestgreen", "dodgerblue2", "orchid2"),4)  ) +  #
  facet_wrap(~name, scales = 'free', nrow = 1) +
  ylab('Value') +
  xlab("") +
  theme(legend.position="bottom",
        axis.title.x=element_blank(),
        axis.text.x=element_blank())


# FILENAME  comparison_ets-lookout_autoencoder

# ETS
etslookout_diffmetrics2 %>%
  select(Week, false_pos, precision, recall, fmeasure) %>%
  summarize(across(2:5, mean)) %>%
  round(3)
etslookout_diffmetrics2 %>%
  select(Week, false_pos, precision, recall, fmeasure) %>%
  summarize(across(2:5, sd)) %>%
  round(3)


# TSLM
tslmlookout_diffmetrics2 %>%
  select(Week, false_pos, precision, recall, fmeasure) %>%
  summarize(across(2:5, mean)) %>%
  round(3)
tslmlookout_diffmetrics2 %>%
  select(Week, false_pos, precision, recall, fmeasure) %>%
  summarize(across(2:5, sd)) %>%
  round(3)

# Lightgbm
lightgbmlookout_diffmetrics2 %>%
  select(Week, false_pos, precision, recall, fmeasure) %>%
  summarize(across(2:5, mean)) %>%
  round(3)
lightgbmlookout_diffmetrics2 %>%
  select(Week, false_pos, precision, recall, fmeasure) %>%
  summarize(across(2:5, sd)) %>%
  round(3)

# Zero-inflated
zerolookout_diffmetrics2 %>%
  select(Week, false_pos, precision, recall, fmeasure) %>%
  summarize(across(2:5, mean)) %>%
  round(3)
zerolookout_diffmetrics2 %>%
  select(Week, false_pos, precision, recall, fmeasure) %>%
  summarize(across(2:5, sd)) %>%
  round(3)


# Autoencoder
autoen_diffmetrics2 %>%
  select(Week, false_pos, precision, recall, fmeasure) %>%
  summarize(across(2:5, mean)) %>%
  round(3)
autoen_diffmetrics2 %>%
  select(Week, false_pos, precision, recall, fmeasure) %>%
  summarize(across(2:5, sd)) %>%
  round(3)


# --------------------------------------------------------------------
# TASK  3:   PLOT NUMBER OF ANOMALIES FOR EACH WINDOW
# --------------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(stringr)
library(tidyr)

library(hts)
library(fable)
library(fpp3)
library(lookout)
library(h2o)
library(dplyr)

# Weekly data
out_all1<- readRDS("Data_Output/To_Make_Public/All_outliers_1_week_moving_window_alpha_point1.rds")
out_sum <-   out_all1 %>%
  group_by(time) %>%
  summarize(num_out = n()) %>%
  filter(time >= 1527)
tib <- tibble(time = 1527:8231)
out_sum2 <- full_join(out_sum, tib) %>%
  mutate(num_out = replace_na(num_out, 0), method = "ETS")


out_all2 <- readRDS("Data_Output/To_Make_Public/TSLM_All_outliers_1_week_moving_window_alpha_point1.rds")
out_sum <-   out_all2 %>%
  group_by(time) %>%
  summarize(num_out = n()) %>%
  filter(time >= 1527)
tib <- tibble(time = 1527:8231)
out_sum3 <- full_join(out_sum, tib) %>%
  mutate(num_out = replace_na(num_out, 0), method = "TSLM")


out_all3 <- readRDS("Data_Output/To_Make_Public/Lightgbm_All_outliers_1_week_moving_window_alpha_point1.rds")
out_sum <-   out_all3 %>%
  group_by(time) %>%
  summarize(num_out = n()) %>%
  filter(time >= 1527)
tib <- tibble(time = 1527:8231)
out_sum4 <- full_join(out_sum, tib) %>%
  mutate(num_out = replace_na(num_out, 0), method = "LightGBM")


out_all4 <- readRDS("Data_Output/To_Make_Public/Zeroinflated_All_outliers_1_week_moving_window_alpha_point1.rds")
out_sum <-   out_all4 %>%
  group_by(time) %>%
  summarize(num_out = n()) %>%
  filter(time >= 1527)
tib <- tibble(time = 1527:8231)
out_sum5 <- full_join(out_sum, tib) %>%
  mutate(num_out = replace_na(num_out, 0), method = "Zero-inflated")

out_all_autoen <- read_csv("Data_Output/A2_TASK_5/Autoencoder_Anomalies.csv")
out_all_autoen2 <- out_all_autoen %>%
  filter(hour <=8231) %>%
  rename(time = hour) %>%
  filter(time >= 1527)
tib <- tibble(time = 1527:8231)
out_sum_autoen <- full_join(out_all_autoen2, tib) %>%
  mutate(num_out = replace_na(num_out, 0), method = "Autoencoder")

out_sum6 <- bind_rows(out_sum2, out_sum3, out_sum4, out_sum5, out_sum_autoen)

ggplot(out_sum6, aes(time, num_out)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~method, nrow= 1) +
  xlab('Window') +
  ylab('Number of anomalies')


# ETS
quantile(out_sum2$num_out, probs = c(0.50, 0.75, 0.95))
sum(out_sum2$num_out == 0)
sum(out_sum2$num_out != 0)
max(out_sum2$num_out)


# TSLM
quantile(out_sum3$num_out, probs = c(0.50, 0.75, 0.95))
sum(out_sum3$num_out == 0)
sum(out_sum3$num_out != 0)
max(out_sum3$num_out)


# LightGBM
quantile(out_sum4$num_out, probs = c(0.50, 0.75, 0.95))
sum(out_sum4$num_out == 0)
sum(out_sum4$num_out != 0)
max(out_sum4$num_out)


# Zero-Inflated
quantile(out_sum5$num_out, probs = c(0.50, 0.75, 0.95))
sum(out_sum5$num_out == 0)
sum(out_sum5$num_out != 0)
max(out_sum5$num_out)

# Autoencoder
quantile(out_sum_autoen$num_out, probs = c(0.50, 0.75, 0.95))
sum(out_sum_autoen$num_out == 0)
sum(out_sum_autoen$num_out != 0)
max(out_sum_autoen$num_out)

dat <- read_csv("Data_Output/To_Make_Public/All_anomalous_times_with_details.csv")
head(dat)
tib <- tibble(time = 1527:8231)
dat2 <- dat  %>%
  group_by(anomaly_max_hour) %>%
  filter(anomaly_max_hour >= 1527) %>%
  summarize(num_out = n()) %>%
  rename(time = anomaly_max_hour) %>%
  full_join(tib) %>%
  arrange(time) %>%
  mutate(num_out = replace_na(num_out, 0))

dat3 <- dat  %>%
  group_by(anomaly_max_hour, node) %>%
  summarise(num_out = n())
View(dat3)

dat4 <- dat %>%
  select(anomaly_max_hour, node) %>%
  filter(anomaly_max_hour >= 1527) %>%
  distinct() %>%
  group_by(anomaly_max_hour) %>%
  summarize(num_out = n()) %>%
  rename(time = anomaly_max_hour) %>%
  full_join(tib) %>%
  arrange(time) %>%
  mutate(num_out = replace_na(num_out, 0))

quantile(dat4$num_out, probs = c(0.50, 0.75, 0.95))
sum(dat4$num_out == 0)
sum(dat4$num_out != 0)
max(dat4$num_out)

# --------------------------------------------------------------------
# TASK  4:   REPEATED ANOMALIES
# --------------------------------------------------------------------
library(stringr)
library(dplyr)
# ETS data
out_all1<- readRDS("Data_Output/To_Make_Public/All_outliers_1_week_moving_window_alpha_point1.rds")

out_all_ets <- out_all1 %>%
  filter(.model == 'ets_bu') %>%
  mutate(node = paste('N',str_pad(nodes, width=3, pad="0"), sep="")) %>%
  filter(nodes != 5) %>%
  select(nodes, node, time, fore_res) %>%
  mutate(logres = log(abs(fore_res)), node_id = as.integer(paste(nodes)),
         week = ceiling((time-1526.9)/(24*7)))

new_node_id <- 1:length(unique(out_all_ets$node))
node_id_uniq <-sort(unique(out_all_ets$node_id))
df1 <- tibble(node_id = node_id_uniq, new_id = new_node_id)

out_all_ets2 <- out_all_ets %>%
  full_join(df1)

ggplot(out_all_ets, aes(time, logres)) +
  geom_point() +
  facet_wrap(~node)

out_all_ets3 <- out_all_ets2 %>%
  mutate(day = round(time/24)) %>%
  rename(Intensity = logres)

node_des <- sort(unique(out_all_ets3$node))

ggplot(out_all_ets3, aes(day, new_id)) +
  geom_tile(aes(fill = Intensity)) +
  ylab('Node') +
  scale_y_discrete(limits=node_des) +
  xlab("Day") +
  scale_fill_gradient(low = "blue", high = "red")

# ------------------------------------------------------------------------------
# TSLM data
out_all1<- readRDS("Data_Output/To_Make_Public/TSLM_All_outliers_1_week_moving_window_alpha_point1.rds")

out_all_tslm <- out_all1 %>%
  filter(.model == 'tslm_bu') %>%
  mutate(node = paste('N',str_pad(nodes, width=3, pad="0"), sep="")) %>%
  filter(nodes != 5) %>%
  select(nodes, node, time, fore_res) %>%
  mutate(logres = log(abs(fore_res)), node_id = as.integer(paste(nodes)),
         week = ceiling((time-1526.9)/(24*7)))

new_node_id <- 1:length(unique(out_all_tslm$node))
node_id_uniq <- sort(unique(out_all_tslm$node_id))
df1 <- tibble(node_id = node_id_uniq, new_id = new_node_id)

out_all_tslm2 <- out_all_tslm %>%
  full_join(df1)

ggplot(out_all_tslm, aes(time, logres)) +
  geom_point() +
  facet_wrap(~node)

out_all_tslm3 <- out_all_tslm2 %>%
  mutate(day = round(time/24)) %>%
  rename(Intensity = logres)
node_des <- sort(unique(out_all_tslm3$node))


ggplot(out_all_tslm3, aes(day, new_id)) +
  geom_tile(aes(fill = Intensity)) +
  ylab('Node') +
  scale_y_discrete(limits=node_des) +
  xlab("Day") +
  scale_fill_gradient(low = "blue", high = "red")


# ------------------------------------------------------------------------------
# LightGBM data
out_all1<- readRDS("Data_Output/To_Make_Public/Lightgbm_All_outliers_1_week_moving_window_alpha_point1.rds")

out_all_lightgbm <- out_all1 %>%
  filter(hf_model == 'BU' & fore_model == "Lightgbm") %>%
  rename(nodes = node) %>%
  mutate(node = paste('N',str_pad(nodes, width=3, pad="0"), sep="")) %>%
  filter(nodes != 5) %>%
  select(nodes, node, time, fore_res) %>%
  mutate(logres = log(abs(fore_res)), node_id = as.integer(paste(nodes)),
         week = ceiling((time-1526.9)/(24*7)))

new_node_id <- 1:length(unique(out_all_lightgbm$node))
node_id_uniq <- sort(unique(out_all_lightgbm$node_id))
df1 <- tibble(node_id = node_id_uniq, new_id = new_node_id)

out_all_lightgbm2 <- out_all_lightgbm %>%
  full_join(df1)

ggplot(out_all_lightgbm, aes(time, logres)) +
  geom_point() +
  facet_wrap(~node)

out_all_lightgbm3 <- out_all_lightgbm2 %>%
  mutate(day = round(time/24)) %>%
  rename(Intensity = logres)
node_des <- sort(unique(out_all_lightgbm3$node))


ggplot(out_all_lightgbm3, aes(day, new_id)) +
  geom_tile(aes(fill = Intensity)) +
  ylab('Node') +
  scale_y_discrete(limits=node_des) +
  xlab("Day") +
  scale_fill_gradient(low = "blue", high = "red")


# ------------------------------------------------------------------------------
# Zero-inflated data
out_all1<- readRDS("Data_Output/To_Make_Public/Zeroinflated_All_outliers_1_week_moving_window_alpha_point1.rds")

out_all_zero <- out_all1 %>%
  filter(hf_model == 'BU' & fore_model == "Zero-Inflated") %>%
  rename(nodes = node) %>%
  mutate(node = paste('N',str_pad(nodes, width=3, pad="0"), sep="")) %>%
  filter(nodes != 5) %>%
  select(nodes, node, time, fore_res) %>%
  mutate(logres = log(abs(fore_res)), node_id = as.integer(paste(nodes)),
         week = ceiling((time-1526.9)/(24*7)))

new_node_id <- 1:length(unique(out_all_zero$node))
node_id_uniq <- sort(unique(out_all_zero$node_id))
df1 <- tibble(node_id = node_id_uniq, new_id = new_node_id)

out_all_zero2 <- out_all_zero %>%
  full_join(df1)

ggplot(out_all_zero, aes(time, logres)) +
  geom_point() +
  facet_wrap(~node)

out_all_zero3 <- out_all_zero2 %>%
  mutate(day = round(time/24)) %>%
  rename(Intensity = logres)
node_des <- sort(unique(out_all_zero3$node))


ggplot(out_all_zero3, aes(day, new_id)) +
  geom_tile(aes(fill = Intensity)) +
  ylab('Node') +
  scale_y_discrete(limits=node_des) +
  xlab("Day") +
  scale_fill_gradient(low = "blue", high = "red")
