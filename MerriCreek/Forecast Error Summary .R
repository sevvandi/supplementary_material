# ------------------------------------------------------
# AUTHOR: BHARGAV RELE
# ------------------------------------------------------
library(ggplot2)
library(cowplot)
library(reshape2)
library(tidyr)
library(tibble)
# The following code is to save results required from files: "Generalized Additive Models.R", "ARIMA Models.R", and "TS-LSTM Model.R"
# This R file is to save the results for comparison to actual turbidity values and to present RMSE and MAE of forecasts

# Saving predictions for all models vs actual turbidity------------------------------------------------------

arima_predictions <- all_folds_forecast$forecasted_turbidity[1:156]
lstm_predictions <-  all_LSTM_folds_forecast$forecasted_turbidity[1:156]
gams_predictions <-  all_gams_folds_forecast$forecasted_turbidity[1:156]
actual_turbidity <-  daily_2013_2014_rain_lev_turb[201:356, "daily_turbidity"]
predictions_actual_turbidity <- data.frame(arima_predictions,lstm_predictions,gams_predictions,actual_turbidity)

plot(predictions_actual_turbidity$daily_turbidity, type="l")
lines(predictions_actual_turbidity$arima_predictions, col="red")
lines(predictions_actual_turbidity$lstm_predictions, col="blue")
lines(predictions_actual_turbidity$gams_predictions, col="green")

write.table(predictions_actual_turbidity,
            file = "~/Desktop/water quality RA/data files/initial/created datasets/predictions_actual_turbidity.csv",
            sep = ",", row.names = F)

predictions_actual_turbidity <- read_csv("Desktop/water quality RA/data files/initial/created datasets/predictions_actual_turbidity.csv",
                                         col_types = cols(arima_predictions = col_number(), lstm_predictions = col_number(),
                                                          gams_predictions = col_number(), daily_turbidity = col_number()))


# RMSE of forecasts for all models------------------------------------------------------------------------------------

RMSE_arima <- sqrt(mean((predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$arima_predictions)^2))
RMSE_lstm <- sqrt(mean((predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$lstm_predictions)^2))
RMSE_gams <- sqrt(mean((predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$gams_predictions)^2))

# MAE of forecasts for all models------------------------------------------------------------------------------------

MAE_arima <- mean(abs(predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$arima_predictions))
MAE_lstm <- mean(abs(predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$lstm_predictions))
MAE_gams <- mean(abs(predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$gams_predictions))

# Standard Deviation of errors for all models------------------------------------------------------------------------------------

sd_arima <- sd(predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$arima_predictions)
sd_lstm <- sd(predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$lstm_predictions)
sd_gams <- sd(predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$gams_predictions)

# Range of errors for all models------------------------------------------------------------------------------------

min_arima <- min(predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$arima_predictions)
min_lstm <- min(predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$lstm_predictions)
min_gams <- min(predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$gams_predictions)

max_arima <- max(predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$arima_predictions)
max_lstm <- max(predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$lstm_predictions)
max_gams <- max(predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$gams_predictions)

# Summary Results of forecast errors for all models----------------------------------------------------------------------

Models <- c("ARIMA", "LSTM", "GAMs")
RMSE <- c(RMSE_arima, RMSE_lstm, RMSE_gams)
MAE <- c(MAE_arima, MAE_lstm, MAE_gams)
SD <- c(sd_arima, sd_lstm, sd_gams)
Min <- c(min_arima, min_lstm, min_gams)
Max <- c(max_arima, max_lstm, max_gams)
error_summary <- data.frame(Models, RMSE, MAE, SD, Min, Max)

View(error_summary)

# histogram of residuals for all models

arima_errors <- c(predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$arima_predictions)
lstm_errors <- c(predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$lstm_predictions)
gam_errors <- c(predictions_actual_turbidity$daily_turbidity - predictions_actual_turbidity$gams_predictions)
prediction_errors <- data.frame(arima_errors=arima_errors, lstm_errors=lstm_errors, gam_errors=gam_errors)

long_prediction_errors <- prediction_errors%>%rownames_to_column("id")%>%gather(model,values,-id)

models_names <- c("ARIMA Errors", "GAM Errors", "LSTM Errors")
names(models_names) <- c("arima_errors", "gam_errors", "lstm_errors")
dp <- ggplot(long_prediction_errors, aes(x=values)) + geom_density(alpha=.30, size=1.5) + theme_bw() +
  facet_wrap(~model, nrow=3, labeller=labeller(model=models_names))+
  theme(strip.text.x = element_text(size=14),axis.title.y = element_text(size = 13) ,axis.title.x = element_blank()) +
  labs(y="Density")
ggsave(filename="density plot of errors final.png",path = "~/Desktop/water quality RA/z - preprint",
       width = 6, height = 8,
       device='tiff', dpi=100)

# scatterplot predicted vs actual

predictions_actual_turbidity
pa1 <- ggplot(predictions_actual_turbidity, aes(x=daily_turbidity, y=arima_predictions)) + geom_point(colour="red") + theme_bw() + labs(x="", y="ARIMA Predictions") + geom_smooth(method=lm, colour="black")+ theme(axis.title = element_text(size = 15))
pa2 <- ggplot(predictions_actual_turbidity, aes(x=daily_turbidity, y=lstm_predictions)) + geom_point(colour="dark green") + theme_bw() + labs(x="Observed Turbidity (NTU)", y="LSTM Predictions") + geom_smooth(method=lm, colour="black")+ theme(axis.title.x = element_text(size = 13), axis.title.y = element_text(size=15))
pa3 <- ggplot(predictions_actual_turbidity, aes(x=daily_turbidity, y=gams_predictions)) + geom_point(colour="blue") + theme_bw() + labs(x="", y="GAM Predictions") + geom_smooth(method=lm, colour="black")+ theme(axis.title = element_text(size = 15))
plot_grid(pa1, pa2, pa3, ncol=3)
ggsave(filename="scatterplot actual vs predictions final.png",path = "~/Desktop/water quality RA/z - preprint",
       width = 8, height = 7,
       device='tiff', dpi=150)


