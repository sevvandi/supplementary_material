# ------------------------------------------------------
# AUTHOR: BHARGAV RELE
# ------------------------------------------------------

library(readr)
library(keras)
library(caret)
library(tensorflow)
set.seed(123)
library(TSLSTM)
library(xts)

# daily_2013_2014_rain_lev_turb imported as df------------------------------------------------------------------------

df <- read_csv("Desktop/water quality RA/data files/initial/created datasets/daily_2013_2014_rain_lev_turb.csv",
               col_types = cols(date = col_skip(), daily_rain = col_number(),
                                daily_WL = col_number(), daily_turbidity = col_number(),
                                max_temp = col_number(), total_solar_exposure = col_number()))
datetimes <- df$date
ts_daily_full <- xts(df[,-c(1)], order.by=datetimes)

# FINAL MODEL Dynamic Regression - CROSS VALIDATION [1-PERIOD AHEAD FORECAST]------------------------------------

all_LSTM_folds_forecast <- data.frame(forcasted_turbidity=numeric(0),actual_turbidity=numeric(0))
rmse_vector <- c()#gotta do 7*4=28 in testing 111+28=139 until you get to 278 which is 0.8 of full data then actual testing set after 278 has 29 observations
a_start <- 1
a <- 200 #same starting point
p <- 1 #(or do 46 period window until a<=310)

start.time <- Sys.time()

while(a <=356){

  multi_lstm <-
    ts.lstm(
      ts= df[a_start:a,"daily_turbidity"],
      xreg = df[a_start:a,c("daily_WL", "daily_rain", "max_temp", "total_solar_exposure")],
      tsLag=1,
      xregLag = 1,
      LSTMUnits=10,
      Epochs = 50,
      CompLoss = "mse",
      CompMetrics = "mae",
      ActivationFn = "tanh",
      SplitRatio = 0.99,
      ValidationSplit = 0.99
    )

  one_period_forecast <- data.frame(forecasted_turbidity = multi_lstm$TestPredictedValue[1],
                                    actual_turbidity = ts_daily_full[(nrow(multi_lstm$TrainFittedValue)+1), "daily_turbidity"])
  all_LSTM_folds_forecast <- rbind(all_LSTM_folds_forecast,one_period_forecast)

  a_start <- a_start+1
  a <- a+1

}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #1.977426 hours

ts_fcasts_turbidity <- c(rep(NA, 196), all_LSTM_folds_forecast$forecasted_turbidity)
plot(df$daily_turbidity, type="l",
     main="LSTM Cross-validation",
     xlab="Observation No.",
     ylab="Turbidity")
lines(ts_fcasts_turbidity, col = "blue")
legend(220, 270, legend=c("Actual", "Forecasted"),
       col=c("black", "blue"), lty=1, cex=0.8)

dates <- daily_2013_2014_rain_lev_turb$date
lstm_forecasts <- c(rep(NA, 200), all_LSTM_folds_forecast$forecasted_turbidity[-157])
actual_values <- daily_2013_2014_rain_lev_turb$daily_turbidity
lstm_results_df <- data.frame(dates, lstm_forecasts, actual_values)
ggplot(lstm_results_df, aes(x=dates, y=actual_values)) + geom_line(colour="black") +
  geom_line(aes(x=dates, y=lstm_forecasts), colour="blue") +theme_bw() + labs(x="Date", y="Turbidity (NTU)")+
  scale_x_bd(business.dates = dates, labels=date_format('%b%y'))+ theme(axis.title.x = element_blank(),
                                                                        axis.text.x=element_text(size=11),
                                                                        axis.text.y=element_text(size=11))
ggsave(filename="cvLSTM final.png",path = "~/Desktop/water quality RA/z - preprint",
       width = 8, height = 5,
       device='tiff', dpi=150)

