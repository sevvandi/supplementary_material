# Mahdi 126/11/2022
library(rgdal)
library(fabletools)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(hts)
library(fable)
library(fpp3)
library(lightgbm)
library(pscl)


### read data ###

load("mac_analysis_one_year.rdata")
#total_tsible_nodes <- readRDS("total_tsible_nodes.RDS")

total_tsible_nodes_lag <- total_tsible_nodes %>%
  group_by(nodes) %>%
  mutate(lag1= lag(Signals,1),
         lag2= lag(Signals,2),
         lag3= lag(Signals,3),
         lag4= lag(Signals,4),
         lag5= lag(Signals,5),
         lag6= lag(Signals,6),
         lag7= lag(Signals,7),
         lag8= lag(Signals,8),
         lag9= lag(Signals,9),
         lag10= lag(Signals,10),
         lag11= lag(Signals,11),
         lag12= lag(Signals,12),) %>%
  ungroup()
##### Manual functoin for reconciliation #####

error_cal <- function(insample, outsample, forecasts, ppy){

  #insample=Insample[,i];outsample=Forecasts_temp[,i];forecasts=Outsample[,i];ppy=12
  masep<-mean(abs(diff(insample, lag = ppy)))
  outsample <- as.numeric(outsample)
  forecasts <- as.numeric(forecasts)

  #mase <- mean(abs(outsample-forecasts))/masep
  mae <- mean(abs(outsample-forecasts))
  amse <- abs(mean(outsample-forecasts))/masep
  #rmsse <- sqrt(mean(abs(outsample-forecasts)^2)/(masep^2))
  rmse <- sqrt(mean(abs(outsample-forecasts)^2))
  output <- c(mae,amse,rmse) ; names(output) <- c("MAE","AMSE","RMSE")

  return(output)
}

reconcile <- function(frc_base){

  S <- smatrix(structure.n)
  W <- diag(rowSums(S))

  fb <- t(as.matrix(as.data.frame(frc_base)))

  frcst <- S%*%Matrix::solve(t(S)%*% Matrix::solve(W)%*%S)%*%t(S)%*% Matrix::solve(W)%*%fb
  return(frcst)
}



bu <- function(frc_base){

  S <- smatrix(structure.n)
  m <- nrow(S) #total number of series
  K <- length(structure.n$labels)-1 #Numer of levels in the hierarchy
  mK <- length(structure.n$labels[[K+1]]) #Numer of series at the bottom level of the hierarchy
  P <- cbind(matrix(0, nrow = mK,  ncol = m-mK) ,   diag(1, mK))

  fb <- t(as.matrix(as.data.frame(frc_base)))
  frcst <- S%*%P%*%fb
  return(frcst)
}
td <- function(frc_base){

  S <- smatrix(structure.n)
  m <- nrow(S) #total number of series
  K <- length(structure.n$labels)-1 #Numer of levels in the hierarchy
  mK <- length(structure.n$labels[[K+1]]) #Numer of series at the bottom level of the hierarchy

  weigtsTD <- colSums(allts(structure.n))/colSums(allts(structure.n))[1]
  weigtsTD <- matrix(tail(weigtsTD, mK))

  P <- cbind( weigtsTD,  matrix(0, nrow = mK,  ncol = m-1))

  fb <- t(as.matrix(as.data.frame(frc_base)))
  frcst <- S%*%P%*%fb
  return(frcst)
}


### Create hts ####
input <- ts(total_tsible_nodes %>%  filter(!is_aggregated(nodes)) %>% pull(Signals), frequency = 24)
input <- ts(total_tsible_nodes %>%  filter(nodes==1) %>% pull(Signals), frequency = 24)
#TS_id <- seq(363)
#colnames(input) <- TS_id
#data <- hts(input, bnames = c(1:363))
#structure <- window(data, start = c(1998,1))
#m_series <- nrow(smatrix(structure)) #Total number of series at all levels
#b_series <- length(structure$labels[[(length(structure$labels)-1)+1]]) #Number of series at the bottom level
#input <- NULL

data_wide <- total_tsible_nodes %>% pivot_wider(names_from = nodes, values_from = Signals)
colnames(data_wide) <- c("time", "Agg", c(seq(362)))
data <- hts(data_wide[,c(-1,-2)])

structure.n <-  window(data, start = 1)
m_series <- nrow(smatrix(structure.n)) #Total number of series at all levels
b_series <- length(structure.n$labels[[(length(structure.n$labels)-1)+1]]) #Number of series at the bottom level
#input <- NULL

data_wide_covar_1 <- total_tsible_nodes_lag %>% select(lag1) %>% pivot_wider(names_from = nodes, values_from = lag1)
data_wide_covar_2 <- total_tsible_nodes_lag %>% select(lag2) %>% pivot_wider(names_from = nodes, values_from = lag2)
data_wide_covar_3 <- total_tsible_nodes_lag %>% select(lag3) %>% pivot_wider(names_from = nodes, values_from = lag3)
data_wide_covar_4 <- total_tsible_nodes_lag %>% select(lag4) %>% pivot_wider(names_from = nodes, values_from = lag4)
data_wide_covar_5 <- total_tsible_nodes_lag %>% select(lag5) %>% pivot_wider(names_from = nodes, values_from = lag5)
data_wide_covar_6 <- total_tsible_nodes_lag %>% select(lag6) %>% pivot_wider(names_from = nodes, values_from = lag6)


data.covar1 <-  hts(data_wide_covar_1[,c(-1,-2)])
data.covar2 <-  hts(data_wide_covar_2[,c(-1,-2)])
data.covar3 <-  hts(data_wide_covar_3[,c(-1,-2)])
data.covar4 <-  hts(data_wide_covar_4[,c(-1,-2)])
data.covar5 <-  hts(data_wide_covar_5[,c(-1,-2)])
data.covar6 <-  hts(data_wide_covar_6[,c(-1,-2)])


## Lgbm model
params= list(objective = "regression_l1",
             metric = "mae",
             lambda_l1 = 0.1,
             num_leaves =100,
             max_depth = 10,
             learning_rate = 0.1,
             min_data_in_leaf = 150,
             #min_data_in_leaf = k,
             #feature_fraction= 0.7,
             #bagging_fraction= 0.7,
             num_threads = 8,
             boosting = "gbdt",
             verbose =-1)


#Get base forecasts for training and forecast purposes
Forecast_file = Residuals_file <- NULL
data_train_all= data_test_all <- NULL
counter <- 0


for (i in 1) {
  origin <- 1344 #Starting origin
  tslength <- length(data[1]$bts[,1]) #Available observations
  fh = 24*7 #Forecasting horizon considered

  while ((origin+counter+fh) <= tslength+1){

    #Fetch data
    ts_sample <- head(aggts(data), origin+counter+fh)
    train_sample <- head(ts_sample, length(ts_sample[,1])-fh)
    test_sample <- tail(ts_sample, fh)

    ## Include regessors
    ts_sample_c1 <- head(aggts(data.covar1), origin+counter+fh)
    train_sample_c1 <- head(ts_sample_c1, length(ts_sample_c1[,1])-fh)
    test_sample_c1 <- tail(ts_sample_c1, fh)

    ts_sample_c2 <- head(aggts(data.covar2), origin+counter+fh)
    train_sample_c2 <- head(ts_sample_c2, length(ts_sample_c2[,1])-fh)
    test_sample_c2 <- tail(ts_sample_c2, fh)

    ts_sample_c3 <- head(aggts(data.covar3), origin+counter+fh)
    train_sample_c3 <- head(ts_sample_c3, length(ts_sample_c3[,1])-fh)
    test_sample_c3 <- tail(ts_sample_c3, fh)

    ts_sample_c4 <- head(aggts(data.covar4), origin+counter+fh)
    train_sample_c4 <- head(ts_sample_c4, length(ts_sample_c4[,1])-fh)
    test_sample_c4 <- tail(ts_sample_c4, fh)

    ts_sample_c5 <- head(aggts(data.covar5), origin+counter+fh)
    train_sample_c5 <- head(ts_sample_c5, length(ts_sample_c5[,1])-fh)
    test_sample_c5 <- tail(ts_sample_c5, fh)

    ts_sample_c6 <- head(aggts(data.covar6), origin+counter+fh)
    train_sample_c6 <- head(ts_sample_c6, length(ts_sample_c6[,1])-fh)
    test_sample_c6 <- tail(ts_sample_c6, fh)

    #Generate base forecasts
    all_mod_lm = all_mod_zinf = all_mod_lgbm  <- NULL
    mod_lm = mod_zinf = mod_lgbm  <- NULL

    all_for_lm = all_for_zinf = all_for_lgbm  <- NULL
    fore_lm = fore_zinf = fore_lgbm  <- NULL

    all_fit_lm = all_fit_zinf = all_fit_lgbm  <- NULL
    fit_lm = fit_zinf = fit_lgbm  <- NULL

    for (tsid in 1:ncol(train_sample)){
      train_data <- cbind(train_sample[,tsid],train_sample_c1[,tsid],train_sample_c2[,tsid],
                          train_sample_c3[,tsid],train_sample_c4[,tsid],
                          train_sample_c5[,tsid],train_sample_c6[,tsid]);
      train_data <- train_data[!is.na(rowSums(train_data)),]

      colnames(train_data) <- c("Signals","l1","l2","l3","l4","l5","l6");
      test_data <- cbind(test_sample[,tsid],test_sample_c1[,tsid],test_sample_c2[,tsid],
                         test_sample_c3[,tsid],test_sample_c4[,tsid],
                         test_sample_c5[,tsid],test_sample_c6[,tsid]);
      colnames(test_data) <- c("Signals", "l1","l2","l3","l4","l5","l6");

      ##lgbm model
      all_mod_lgbm <- (lightgbm(train_data, label=train_data[,1], params=params))

      fit_lgbm <- rbind(fit_lgbm,cbind(t(train_data[,1] - predict(all_mod_lgbm,train_data))))
      all_fit_lgbm <- rbind(all_fit_lgbm,cbind(t(train_data[,1] - predict(all_mod_lgbm,train_data)), tsid=tsid, counter=counter))

      fore_lgbm <- rbind(fore_lgbm,cbind(t(predict(all_mod_lgbm, test_data))))
      all_for_lgbm <- rbind(all_for_lgbm,cbind(t(predict(all_mod_lgbm, test_data)), tsid=tsid, counter=counter))

      ##linear time series model
      #all_mod_lm <- tslm(ts(train_data[,1])~ train_data[,2:7])
      #all_mod_lm <- ar(ts(train_data[,1]),order=6)

      #fit_lm <- rbind(fit_lm,cbind(t(train_data[,1] - fitted(all_mod_lm))))
      #all_fit_lm <- rbind(all_fit_lm,cbind(t(train_data[,1] - fitted(all_mod_lm)), tsid=tsid, counter=counter))

      #fore_lm <- rbind(fore_lm,cbind(t(predict(all_mod_lm, n.ahead = fh))))
      #all_for_lm <- rbind(all_for_lm,cbind(t(predict(all_mod_lm, n.ahead = fh)$pred), tsid=tsid, counter=counter))



      ## zero-inflated model
      all_mod_zinf <- tryCatch(zeroinfl(Signals ~ . | ., data =data.frame(train_data), dist = "negbin"),
                               error=function(e) NA)
      if (is.na(all_mod_zinf)[1]) {
        all_for_zinf <- rbind(all_for_zinf,cbind(t(rep(0, length(test_data[,1]))), tsid=tsid, counter=counter));
        all_fit_zinf <- rbind(all_fit_zinf,cbind(t(rep(0, length(train_data[,1]))),tsid=tsid, counter=counter));
        #fore_zinf <- rbind(fore_zinf,cbind(t(predict(all_mod_zinf, test_data))))
        #fit_zinf <- rbind(fit_zinf,cbind(t((train_data[,1] - fitted(all_mod_zinf)))))
      }
      else {
        all_for_zinf <- rbind(all_for_zinf,cbind(t(predict(all_mod_zinf, test_data)), tsid=tsid, counter=counter))
        all_fit_zinf <- rbind(all_fit_zinf,cbind(t((train_data[,1] - fitted(all_mod_zinf))),tsid=tsid, counter=counter))
        #fore_zinf <- rbind(fore_zinf,cbind(t(predict(all_mod_zinf, test_data))));
        #fit_zinf <- rbind(fit_zinf,cbind(t((train_data[,tsid] - fitted(all_mod_zinf)))));
      }

      #data_train_all <- rbind(data_train_all, cbind(train_data,counter=counter))
      #data_test_all <- rbind(data_test_all, cbind(test_sample, counter=counter))
    }
    #The first node can not be zero inflated casue it has no zero observation
    train_data1 <- cbind(train_sample[,1],train_sample_c1[,1],train_sample_c2[,1],
                         train_sample_c3[,1],train_sample_c4[,1],
                         train_sample_c5[,1],train_sample_c6[,1]);
    train_data1 <- train_data1[!is.na(rowSums(train_data1)),]
    all_mod_zinf1 <- ar(ts(train_data1[,1], frequency = 24), order.max = 6)
    all_for_zinf1 <- cbind(t(predict(all_mod_zinf1, n.ahead = fh)$pred), tsid=1, counter=0)

    all_for_zinf[1,] <- all_for_zinf1
    #colnames(all_for_lgbm[,1:ncol(train_data)]) <- colnames(test_sample[,1:ncol(train_data)]);

    #test_data <- cbind(test_sample_c1[,tsid],test_sample_c2[,tsid],
    #                 test_sample_c3[,tsid],test_sample_c4[,tsid],
    #                  test_sample_c5[,tsid],test_sample_c6[,tsid]);
    #colnames(test_data) <- c("l1","l2","l3","l4","l5","l6");
    #all_for_lgbm <- (foreach(tsid=1:ncol(train_data), .combine='cbind') %do% as.numeric(predict(all_mod_lgbm[[tsid]], xreg= test_data, h = fh )))
    #all_for_zinf <- foreach(tsid=1:ncol(train_data), .combine='cbind') %do% as.numeric(all_mod_zinf[[tsid]]$forecast)

    #all_fit_zinf <- foreach(tsid=1:ncol(train_data), .combine='cbind') %do% as.numeric((train_data[,1] - fitted(all_mod_zinf[[tsid]])))
    #all_fit_lgbm <- tryCatch(foreach(tsid=1:ncol(train_data), .combine='cbind') %do% as.numeric((train_data[1] - fitted(all_mod_lgbm[[tsid]]))))

    #Save results
    for (model_id in 1:2){
      if (model_id==1){
        allf <- t(all_for_zinf[,-c(169,170)])[,1:363]
        Residuals_temp <- t(all_fit_zinf[,-c(169,170)])[,1:363]
      }else if (model_id==2){
        allf <- t((all_for_lgbm[,-c(169,170)]))[,1:363]
        Residuals_temp <- t(all_fit_lgbm[,-c(169,170)])[,1:363]
        #}else{
        # allf <- data.frame(all_for_theta)
        #Residuals_temp <- all_fit_theta
      }
      colnames(allf) <- paste0("F_", as.character(colnames(test_sample)));
      test_sample_c <- as.data.frame(test_sample)
      colnames(test_sample_c) <- paste0("A_", as.character(colnames(test_sample)))
      #Combine with actual values
      allf <- cbind(allf, test_sample_c)
      #Add info for period
      allf$rep <- counter +1
      allf$fh <- c(1:fh)
      # this is the start of test after the original training data
      allf$period <- c((origin+counter+1) : (origin+counter+fh))
      allf$origin <- origin+counter
      allf$model_id <- model_id
      #Combine with the rest
      Forecast_file <- rbind(Forecast_file, allf)

      Residuals_temp <- as.data.frame(Residuals_temp[1:length(train_data[,1]),])
      colnames(Residuals_temp) <- colnames(test_sample)
      Residuals_temp$rep <- counter +1
      #period is not the length of training data, but smaller because some NA values are removed
      Residuals_temp$period <- c(1:length(train_data[,1]))
      Residuals_temp$origin <- origin+counter
      Residuals_temp$model_id <- model_id
      Residuals_file <- rbind(Residuals_file, Residuals_temp)
    }
    counter <- counter + (24*7)
  }
}

saveRDS(Forecast_file, "Forecast_file_2.RDS")
saveRDS(Residuals_file, "Residual_file_2.RDS")

save.image("lgbm_zinf_2.Rdata")
ts_sample = train_data = test_sample = allf = Residuals_temp <- NULL

#######################################################################
#######################################################################

#######################################################################
#Reconcile
########################################################################

Summary_error<- c()
fore_error_all <- Forecasts_mint_all <-  Forecasts_st_all <-  Forecasts_bu_all <-  Forecasts_td_all <- NULL
#originid= 1344
originid=2184
#ids <- c(unique(Forecast_file$origin))
ids <- c( 1344 ,1512 ,1680 ,1848 ,2016 ,2184 ,2352 ,2520 ,2688 ,2856 ,3024 ,3192 ,3360 ,3528 ,3696 ,3864 ,4032 ,4200
          ,4368 ,4536 ,4704 ,4872 ,5040 ,5208 ,5376 ,5544 ,5712 ,5880 ,6048 ,6216 ,6384 ,6552 ,6720 ,6888 ,7056 ,7224
          ,7392 ,7560 ,7728 ,7896 ,8064 )
model_ids <- c(1,2)
gc()
for (model_id in (model_ids)) {
  for (originid in (ids)){
    #model_type <- "lgbm" ;
    model_type_id <- model_id #Consider a base model

    tempf <- Forecast_file[Forecast_file$origin == originid,]
    tempf <- tempf[tempf$model_id==model_type_id,]
    Outsample <- tempf[,(1:m_series)+m_series]
    Forecasts <- tempf[,1:m_series]
    #replacing zero forecasts with a small number to get Mint working
    #Forecasts[,c(which(colSums(Forecasts)==0))] <- matrix(rnorm(fh*length((which(colSums(Forecasts)==0))), mean = 0,sd=1), nrow =fh ,ncol = length((which(colSums(Forecasts)==0))))
    Forecasts <- Forecasts + rnorm(fh,mean=0,0.1)
    Insample <- aggts(data)[(1:originid),]

    Forecasts_st <- t(reconcile(Forecasts)) #MinT
    Forecasts_bu <- t(bu(Forecasts)) #BU
    Forecasts_td <- t(td(Forecasts)) #TD

    res = Residuals_file[(Residuals_file$model_id==model_type_id)&(Residuals_file$origin==originid),]
    res= res+ rnorm(dim(res)[1],mean=0,0.1)
    #res[,c(which(colSums(res)==0))] <- matrix(rnorm(fh*length((which(colSums(res)==0))), mean = 0,sd=1), nrow =fh ,ncol = length((which(colSums(res)==0))))
    #res[,1:363]=res[,1:363]+0.01
    res$model_id = res$origin = res$period = res$rep <- NULL
    row.names(res) <- NULL
    res <- as.matrix(res) ; forc <- as.matrix(Forecasts)
    #Forecasts_mint <- tryCatch(MinT(forc, get_nodes(structure.n), residual = res,
    #                      covariance = "shr", keep = "all", algorithms = "lu"),
    #                     error=function(e) NA)

    #Forecasts_mint_all <- rbind(Forecasts_mint_all, cbind(Forecasts_mint,origin = originid, model_id=model_type_id))
    Forecasts_st_all <- rbind(Forecasts_st_all,  cbind(Forecasts_st,origin = originid, model_id=model_type_id))
    Forecasts_bu_all <- rbind(Forecasts_bu_all,  cbind(Forecasts_bu,origin = originid, model_id=model_type_id))
    Forecasts_td_all <- rbind(Forecasts_td_all,  cbind(Forecasts_td,origin = originid, model_id=model_type_id))

    ##########################################################################
    for (mid in 1:3){
      if (mid==1){
        Forecasts_temp <- Forecasts_bu
      }else if (mid==2){
        Forecasts_temp <- Forecasts_td
      }else if (mid==3){
        Forecasts_temp <- Forecasts_st
      }#else if (mid==4){
      # Forecasts_temp <- Forecasts_mint
      #}

      fore_error <- error_list <- NULL
      for (i in 1:ncol(Outsample)){
        error_list <- rbind(error_list, error_cal(Insample[,i],Forecasts_temp[,i],Outsample[,i],ppy=24))
        fore_error <- rbind(fore_error, cbind(fore_res=(Outsample)[,i]-(Forecasts_temp)[,i],node=i,mean=Forecasts_temp[,i], Signals=Outsample[,i], h=c(1:168)))
      }
      fore_error_all <- rbind(fore_error_all,cbind(fore_error,origin = originid, model_id=model_type_id, model=mid))

      Errors <- data.frame(error_list) ; colnames(Errors) <- c("MAE","AMSE","RMSE")
      Errors$Tags <- as.character(unlist(colnames(Outsample)))
      for (i in 1:nrow(Errors)){
        Errors$Tags[i] <- substr(Errors$Tags[i],3, nchar(Errors$Tags[i]))
      }
      Errors$Level <- NA
      for (idd in 1:nrow(Errors)){
        for (ldd in 1:length(structure.n$labels)){
          if (Errors$Tags[idd] %in% structure.n$labels[[ldd]]){
            Errors$Level[idd] <- ldd
          }
        }
      }

      Errors$Tags <- NULL
      if (mid==1){
        Errors$mid <- "BU"
      }else if (mid==2){
        Errors$mid <- "TD"
      }else if (mid==3){
        Errors$mid <- "MinT"
      }#else if (mid==4){
      # Errors$mid <- "MinT"
      #}

      Errors$origin <- originid
      Errors$model_type <- model_type_id
      Summary_error <- rbind(Summary_error, Errors)
    }
  }
}


## calculting accuracy
Summary_error_n <- Summary_error %>%
  mutate(nodes=rep(c(1:363),2*41*3)) %>%
  as_tibble() %>%
  mutate(hf_model = case_when(mid ==1 ~"BU",
                              mid ==2 ~"TD",
                              mid ==3 ~"Mint-Shr")) %>%
  mutate(fore_model= case_when(model_type==1 ~"Lightgbm",
                               model_type==2 ~"Zero-Inflated")) %>%
  filter(Level==2) %>%
  mutate(nodes=nodes-1) %>%
  mutate(Type= if_else(nodes %in% c(anomalous_nodes$nodes), "Anomalous", "Non-anomalous"))

Summary_error_n %>% filter(!is.na(AMSE)) %>%
  filter(MAE<10000) %>%
  dplyr::group_by(mid,fore_model,Type) %>%
  dplyr::summarise(mean(MAE)) %>%
  xtable::xtable()
#mutate(Avg_AMSE= mean(AMSE, na.rm=TRUE)) %>% xtab



fore_new_res <- fore_error_all %>% as_tibble() %>%
  mutate(time = origin + h) %>%
  mutate(hf_model = case_when(model==1 ~"BU",
                              model==2 ~"TD",
                              #model==3 ~"ST",
                              model==3 ~"Mint-Shr")) %>%
  mutate(fore_model= case_when(model_id==1 ~"Lightgbm",
                               model_id==2 ~"Zero-Inflated")) %>%
  filter(!is_aggregated(node)) %>%
  mutate(node=node-1) %>%
  mutate(Type= if_else(node %in% c(anomalous_nodes$nodes), "Anomalous", "Non-anomalous")) %>%
  select(-model, -model_id, node,  fore_model, hf_model,time,
         Signals,mean,h, fore_res,origin, Type)

## saving forecasts and residuals
#saveRDS(Summary_error, "Summary_error.RDS")
#saveRDS(Forecast_file, "Forecast_file.RDS")
#saveRDS(Residuals_file, "Residuals_file.RDS")
#replace Inf and small numbers with 0
fore_new_res[fore_new_res<0] <- 1
fore_new_res[fore_new_res$mean=="Inf",3] <- 1
fore_new_res <- fore_new_res %>% mutate(fore_res=Signals-mean)
saveRDS(fore_new_res, "fore_new_res2.RDS")




### looking at anomalous nodes ####

load("2019_Hourly_Time_Series_1hr_Step_Size.RData")

data_one_hour_ts <- ts_2019
anomalies <- read.csv("Anomalous_Nodes_and_Times.csv")

mac_all_ts <-  NULL

for (j in 1:length(data_one_hour_ts[[1]])) {
  mac_all_ts <- rbind(mac_all_ts, cbind((c(data_one_hour_ts[[1]][[j]])), t=c(1:length(data_one_hour_ts[[1]][[j]])), node=j,
                                        earlytime=c(data_one_hour_ts[[3]]),
                                        macadd=c(data_one_hour_ts[[2]][[j]])))
}

colnames(mac_all_ts) <- c("Signals","time","nodes", "earlytime", "MacAdd")

mac_all_ts <- mac_all_ts %>% as_tibble() %>%
  mutate(Signals=as.numeric(Signals),
         nodes=as.numeric(nodes),
         time=as.numeric(time),
         earlytime=as.numeric((earlytime)))

anomal_nodes <- (as.numeric(substr(anomalies$node, start = 2, stop = 5)))
anomalies <- anomalies %>% as_tibble() %>%
  mutate(nodes= as.numeric(anomal_nodes),
         earlytime= as.numeric(time))

mac_all_ts_add <- left_join(anomalies,
                            mac_all_ts,
                            by=c("nodes", "earlytime"))

#mac_all_ts[mac_all_ts$nodes==anomalies$nodes && mac_all_ts$earlytime==anomalies$earlytime,]
# sum of counts is not equal to the filer of anomaly nodes??
sum(anomalies$count)

## not sure why it cannot find an earlytime mathcing our anomalies??
mac_all_ts %>%
  filter(earlytime %in% c(anomalies$earlytime)) %>%
  filter(nodes %in% c(anomalies$nodes))


anomalous_nodes <- mac_all_ts_add %>% distinct(nodes)

autoplot(mac_all_ts_add %>%
           #filter(time>700 & time<800) %>%
           #filter(Signals>1000) %>%
           filter(!is_aggregated(nodes)), level=NULL) +
  labs(y = "Signals") +
  facet_wrap(vars(nodes), scales = "free_y")


