# ------------------------------------------------------
# AUTHOR: CALEB HOGAN
# ------------------------------------------------------
                              ## META MODEL FOR TURBIDITY LEVELS ##



#Libraries
library(dplyr)
library(tsfeatures)
library(caTools)
library(randomForest)



                                        ## READ IN DATA ##

Turbidity_Data <- read.csv("daily_2013_2014_rain_lev_turb.csv") # Actual Data
Turbidity_Data$index <- 1:nrow(Turbidity_Data)
Predictions_Actual <- read.csv("predictions_actual_turbidity.csv") # Predictions from different models and actual data



                ## FIND TS FEATURES OF 30 OBSERVATIONS BEFORE EACH PREDICTION AND FIND TSFEATURES ##
Tsfeatures_Thirty_Values_before_pred <- matrix(0, nrow=156,ncol = 16)

for (i in 1:156) {
  Tsfeatures_Thirty_Values_before_pred[i,] = unlist(tsfeatures(subset(Turbidity_Data, index >= 170+i & index < i+200)$daily_turbidity))

}
colnames(Tsfeatures_Thirty_Values_before_pred) = names(tsfeatures(subset(Turbidity_Data, index >= 170+i & index < i+200)$daily_turbidity))



                ## FIND THE ERROR FOR EACH MODEL PREDICTION AND DETERMINE THE BEST MODEL ##
Predictions_Actual <- Predictions_Actual %>% mutate(ARIMA_Error = abs(daily_turbidity - arima_predictions),
                                             LSTM_Error = abs(daily_turbidity - lstm_predictions),
                                             GAMS_Error = abs(daily_turbidity - gams_predictions)) #Calculate absolute errors for each model

Predictions_Actual <- Predictions_Actual %>% mutate(Best_Model = case_when(ARIMA_Error < LSTM_Error & ARIMA_Error < GAMS_Error ~ "A",
                                                                           LSTM_Error < ARIMA_Error & LSTM_Error < GAMS_Error ~ "L",
                                                                           GAMS_Error < ARIMA_Error & GAMS_Error < LSTM_Error ~ "G"))



                                ## MERGE TS FEATURES WITH BEST MODEL ##
Features_Best_Model <- bind_cols(Tsfeatures_Thirty_Values_before_pred, Predictions_Actual$Best_Model)
colnames(Features_Best_Model)[17] <- "Best_Model"



                                              ##WRITE CSV##
write.csv(Features_Best_Model,"C:/Users/Caleb/OneDrive/Documents/Water Quality Research/Features Best Model.csv", row.names = FALSE)



                                      ## FITTING A RANDOM FORREST ##
set.seed(100)
train <- 1:110

TrainSet <- Features_Best_Model[train,]
ValidSet <- Features_Best_Model[-train,]
summary(TrainSet)
summary(ValidSet)
TrainSet$Best_Model <- factor(TrainSet$Best_Model)
dim(Features_Best_Model)
#Predicting ARIMA or LSTM
model1 <- randomForest(Best_Model ~ ., data = TrainSet, n_tree = 2000, importance = TRUE)
model1

#Join Predictions to Original Dataset
Predictions <- predict(model1, newdata = ValidSet[,-17], type = "class")

testsetdf <- Predictions_Actual[111:156,]
testsetdf$Predictions <- Predictions


#Add all ARIMA and All LSTM Columns
testsetdf$All_ARIMA <- c(rep("A",count(testsetdf)))
testsetdf$All_LSTM <- c(rep("L",count(testsetdf)))
testsetdf$All_GAMS <- c(rep("G",count(testsetdf)))



                                    ## EVALUATING THE MODEL ##
#Evaluating all ARIMA
count(testsetdf %>% filter(Best_Model == "A"))/count(testsetdf)

#Evaluating all LSTM
count(testsetdf %>% filter(Best_Model == "L"))/count(testsetdf)

#Evaluating all GAMS
count(testsetdf %>% filter(Best_Model == "G"))/count(testsetdf)

#Evaluating Meta Model
count(testsetdf %>%
        filter(Best_Model == Predictions))/count(testsetdf)


testsetdf <- testsetdf %>% mutate(ARIMA_SE = (ARIMA_Error)^2)
testsetdf <- testsetdf %>% mutate(LSTM_SE = (LSTM_Error)^2)
testsetdf <- testsetdf %>% mutate(GAMS_SE = (GAMS_Error)^2)

testsetdf <- testsetdf %>% mutate(MM_predictions = case_when(
  (Best_Model == "A") ~ arima_predictions,
  (Best_Model == "L") ~ lstm_predictions,
  (Best_Model == "G") ~ gams_predictions
))

testsetdf <- testsetdf %>% mutate(MM_Error = case_when(
  (Best_Model == "A") ~ ARIMA_Error,
  (Best_Model == "L") ~ LSTM_Error,
  (Best_Model == "G") ~ GAMS_Error
))

testsetdf <- testsetdf %>% mutate(MM_SE = case_when(
  (Best_Model == "A") ~ ARIMA_SE,
  (Best_Model == "L") ~ LSTM_SE,
  (Best_Model == "G") ~ GAMS_SE
))

#MSE
mean(testsetdf$ARIMA_SE)
mean(testsetdf$LSTM_SE)
mean(testsetdf$GAMS_SE)
mean(testsetdf$MM_SE)

#RMSE
sqrt(mean(testsetdf$ARIMA_SE))
sqrt(mean(testsetdf$LSTM_SE))
sqrt(mean(testsetdf$GAMS_SE))
sqrt(mean(testsetdf$MM_SE))

#MAE
mean(testsetdf$ARIMA_Error)
mean(testsetdf$LSTM_Error)
mean(testsetdf$GAMS_Error)
mean(testsetdf$MM_Error)

#SD
sd(testsetdf$arima_predictions)
sd(testsetdf$lstm_predictions)
sd(testsetdf$gams_predictions)
sd(testsetdf$MM_predictions)

#error range
paste(min(testsetdf$ARIMA_Error), max(testsetdf$ARIMA_Error))
paste(min(testsetdf$LSTM_Error), max(testsetdf$LSTM_Error))
paste(min(testsetdf$GAMS_Error), max(testsetdf$GAMS_Error))
paste(min(testsetdf$MM_Error), max(testsetdf$MM_Error))

#Percentage best model
testsetdf %>%
  group_by(Predictions) %>%
  summarise(Percentage = n()/count(testsetdf))
