autoencoder_AD2 <- function(X){
  # initialize h2o cluser
  h2o.init()

  train_inds <- sample(1:NROW(X), ceiling(NROW(X)*0.75))
  train <- X[train_inds, ]
  test <- X[-train_inds, ]
  # convert train and test to h2o object
  train_h2o  <- as.h2o(train)
  test_h2o <-  as.h2o(test)

  # build auto encoder model with 3 layers
  model_unsup <- h2o.deeplearning(x = 1:ncol(X)
                                  , training_frame = train_h2o
                                  , model_id = "Test01"
                                  , autoencoder = TRUE
                                  , reproducible = FALSE
                                  , ignore_const_cols = FALSE
                                  , seed = 42
                                  , hidden = c(10, 10) # c(50,10,50,100,100)
                                  , epochs = 100
                                  , activation ="Tanh")
  # now we need to calculate MSE or anomaly score
  anmlt <- h2o.anomaly(model_unsup
                       , train_h2o
                       , per_feature = FALSE) %>% as.data.frame()
  test_anmlt <- h2o.anomaly(model_unsup
                           , test_h2o
                           , per_feature = FALSE) %>% as.data.frame()

  # combine the train and test anomaly scores for visualization
  results <- rbind.data.frame(anmlt,test_anmlt)

  results2 <- results
  results2[train_inds, 1] <- anmlt
  results2[-train_inds, 1] <- test_anmlt

  results2$Reconstruction.MSE

}


svm_AD <- function(X, nu_val = 0.05){
  modsvm <- e1071::svm(X, type='one-classification', nu=nu_val)
  out <- max(1, max(modsvm$decision.values)) - modsvm$decision.values
  out
  # decision values have lower values for outliers. So inverting it.
}



isolation_AD <- function(X){
  h2o.init()
  # convert train and test to h2o object
  train_h2o  <- as.h2o(X)
  # Build an Isolation forest model
  model <- h2o.isolationForest(training_frame = train_h2o)

  # Calculate score
  score <- h2o.predict(model, train_h2o)
  preds <- as.vector(score$predict)

  preds
}
