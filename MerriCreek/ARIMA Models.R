# ------------------------------------------------------
# AUTHOR: BHARGAV RELE
# ------------------------------------------------------
library(forecast)
library(expsmooth)
library(dynlm)
library(x12)
library(zoo)
library(car)
library(dLagM)
library(AER)
library(Hmisc)
library(ggplot2)
library(readr)
library(tseries)
library(xts)
library(quantmod)
library(readr)
library(lubridate)
library(dplyr)
library(readxl)
library(dhReg)
library(fpp)
library(tsibble)
library(tsibbledata)
library(fable)
library(tidyverse)
library(feasts)
library(cowplot)
library(bdscale)
library(scales)
source("https://gist.githubusercontent.com/ellisp/4002241def4e2b360189e58c3f461b4a/raw/e959562be9e7a4d919a9c454d8b1b70cde904ab0/dualplot.R")

daily_2013_2014_rain_lev_turb <- read_csv("~/Desktop/water quality RA/data files/initial/created datasets/daily_2013_2014_rain_lev_turb.csv")
datetimes <- daily_2013_2014_rain_lev_turb$date
ts_daily_full <- xts(daily_2013_2014_rain_lev_turb[,-c(1)], order.by=datetimes)

# Full dataset visualized

View(daily_2013_2014_rain_lev_turb)
dates <- daily_2013_2014_rain_lev_turb$date
p1 <- ggplot(daily_2013_2014_rain_lev_turb, aes(x = date, y = daily_turbidity)) + geom_line() + labs(x="", y="Turbidity (NTU)")+ theme(axis.title.y = element_text(size = 9)) + scale_x_bd(business.dates = dates, labels=date_format('%b%y'))
p2 <- ggplot(daily_2013_2014_rain_lev_turb, aes(x = date, y = daily_rain)) + geom_line() + labs(x="", y="Rainfall (mm)")+ theme(axis.title.y = element_text(size = 9)) + scale_x_bd(business.dates = dates, labels=date_format('%b%y'))
p3 <- ggplot(daily_2013_2014_rain_lev_turb, aes(x = date, y = daily_WL)) + geom_line() + labs(x="", y="Water Level (m)")+ theme(axis.title.y = element_text(size = 9)) + scale_x_bd(business.dates = dates, labels=date_format('%b%y'))
p4 <- ggplot(daily_2013_2014_rain_lev_turb, aes(x = date, y = max_temp)) + geom_line() + labs(x="", y="Maximum Temperature (\u00B0C)")+ theme(axis.title.y = element_text(size = 9)) + scale_x_bd(business.dates = dates, labels=date_format('%b%y'))
p5 <- ggplot(daily_2013_2014_rain_lev_turb, aes(x = date, y = total_solar_exposure)) + geom_line() + labs(x="Date", y=expression("Total Solar Exposure (mJ/m"^2*")"))+ theme(axis.title.y = element_text(size = 9)) + scale_x_bd(business.dates = dates, labels=date_format('%b%y'))
plot_grid(p1, p2, p3, p4, p5, nrow = 5)

summary(daily_2013_2014_rain_lev_turb %>% select(c(2,3,4,5,6)))


# FINAL MODEL Dynamic Regression - CROSS VALIDATION [1-PERIOD AHEAD FORECAST]------------------------------------

all_folds_forecast <- data.frame(lower_80=numeric(0),lower_95=numeric(0),upper_80=numeric(0),upper_95=numeric(0),
                                 forcasted_turbidity=numeric(0),actual_turbidity=numeric(0), daily_WL=numeric(0),
                                 daily_rain = numeric(0))
rmse_vector <- c()#gotta do 7*4=28 in testing 111+28=139 until you get to 278 which is 0.8 of full data then actual testing set after 278 has 29 observations
a_start <- 1
a <- 200 #same starting point
p <- 1 #(or do 46 period window until a<=310)

start.time <- Sys.time()

while(a <356){

  fit.5 <- auto.arima(ts_daily_full[a_start:a, "daily_turbidity"], xreg = ts_daily_full[a_start:a, c("daily_WL", "daily_rain")])
  fcasts <- forecast::forecast(fit.5, h=p, xreg = ts_daily_full[(a+1):(a+p), c("daily_WL", "daily_rain")])

  one_period_forecast <- data.frame(lower_80=fcasts$lower[,1][1:p],
                                    lower_95=fcasts$lower[,2][1:p],
                                    upper_80=fcasts$upper[,1][1:p],
                                    upper_95=fcasts$upper[,2][1:p],
                                    forecasted_turbidity = fcasts$mean[1:p],
                                    actual_turbidity = ts_daily_full[(a+1):(a+p), "daily_turbidity"],
                                    daily_WL = ts_daily_full[(a+1):(a+p), c("daily_WL")],
                                    daily_rain = ts_daily_full[(a+1):(a+p), c("daily_rain")])
  all_folds_forecast <- rbind(all_folds_forecast,one_period_forecast)

  a_start <- a_start+p
  a <- a+p

}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken # 47.7781 secs

checkresiduals(fit.5) #last fold diagnositcs

dates <- daily_2013_2014_rain_lev_turb$date
arima_forecasts <- c(rep(NA, 200), all_folds_forecast$forecasted_turbidity)
upper_bound <- c(rep(NA, 200), all_folds_forecast$upper_95)
lower_bound <- c(rep(NA, 200), all_folds_forecast$lower_95)
actual_values <- daily_2013_2014_rain_lev_turb$daily_turbidity
arima_results_df <- data.frame(dates, arima_forecasts, upper_bound, lower_bound, actual_values)
ggplot(arima_results_df, aes(x=dates, y=actual_values)) +
  geom_line(colour="black") +
  geom_line(aes(x=dates, y=arima_forecasts), colour="blue") +
  geom_ribbon(aes(x=dates, ymax=upper_bound, ymin=lower_bound), fill="grey",alpha=0.6) +
  theme_bw() + labs(x="Date", y="Turbidity (NTU)") +
  scale_x_bd(business.dates = dates, labels=date_format('%b%y')) +
  theme(axis.title.x = element_blank(), axis.text.x=element_text(size=11), axis.text.y=element_text(size=11))
ggsave(filename="cvARIMA final.png",path = "~/Desktop/water quality RA/z - preprint",
       width = 8, height = 5,
       device='tiff', dpi=150)

exceed_upper_bound_ARIMA <- all_folds_forecast %>% filter(all_folds_forecast$daily_turbidity > all_folds_forecast$upper_95)
exceed_lower_bound_ARIMA <- all_folds_forecast %>% filter(all_folds_forecast$daily_turbidity < all_folds_forecast$lower_95)
nrow(exceed_upper_bound_ARIMA)/nrow(all_folds_forecast) #2.56%
nrow(exceed_lower_bound_ARIMA)/nrow(all_folds_forecast) #2.56%
(nrow(exceed_upper_bound_ARIMA)+nrow(exceed_lower_bound_ARIMA))/nrow(all_folds_forecast)# 5.12%

# Preliminary Analysis - Individual models outside of Cross validation procedure [46-period ahead forecasts]-----------------------------------------------

## daily_2013_train and daily_2013 test------------------------------------------------------------------

daily_2013_test <- read_csv("~/Desktop/water quality RA/data files/initial/created datasets/daily_2013_test.csv",
                            col_types = cols(daily_rain = col_number(),
                                             daily_WL = col_number(), daily_turbidity = col_number(),
                                             date = col_date(format = "%Y-%m-%d")))

daily_2013_train <- read_csv("~/Desktop/water quality RA/data files/initial/created datasets/daily_2013_train.csv",
                             col_types = cols(daily_rain = col_number(),
                                              daily_WL = col_number(), daily_turbidity = col_number(),
                                              date = col_date(format = "%Y-%m-%d")))

### Standardizing daily_2013_train and daily_2013 test---------------------------------------------------------------------

daily_2013_test$daily_rain <- ((daily_2013_test$daily_rain -
                                  mean(daily_2013_train$daily_rain,na.rm=T))/
                                 sd(daily_2013_train$daily_rain,na.rm=T))
daily_2013_test$daily_WL <- ((daily_2013_test$daily_WL -
                                mean(daily_2013_train$daily_WL,na.rm=T))/
                               sd(daily_2013_train$daily_WL,na.rm=T))
daily_2013_test$daily_turbidity <- ((daily_2013_test$daily_turbidity -
                                       mean(daily_2013_train$daily_turbidity,na.rm=T))/
                                      sd(daily_2013_train$daily_turbidity,na.rm=T))
daily_2013_test$max_temp <- ((daily_2013_test$max_temp -
                                mean(daily_2013_train$max_temp,na.rm=T))/
                               sd(daily_2013_train$max_temp,na.rm=T))
daily_2013_test$total_solar_exposure <- ((daily_2013_test$total_solar_exposure -
                                            mean(daily_2013_train$total_solar_exposure,na.rm=T))/
                                           sd(daily_2013_train$total_solar_exposure,na.rm=T))

daily_2013_train$daily_rain <- ((daily_2013_train$daily_rain -
                                   mean(daily_2013_train$daily_rain,na.rm=T))/
                                  sd(daily_2013_train$daily_rain,na.rm=T))
daily_2013_train$daily_WL <- ((daily_2013_train$daily_WL -
                                 mean(daily_2013_train$daily_WL,na.rm=T))/
                                sd(daily_2013_train$daily_WL,na.rm=T))
daily_2013_train$daily_turbidity <- ((daily_2013_train$daily_turbidity -
                                        mean(daily_2013_train$daily_turbidity,na.rm=T))/
                                       sd(daily_2013_train$daily_turbidity,na.rm=T))
daily_2013_train$max_temp <- ((daily_2013_train$max_temp -
                                 mean(daily_2013_train$max_temp,na.rm=T))/
                                sd(daily_2013_train$max_temp,na.rm=T))
daily_2013_train$total_solar_exposure <- ((daily_2013_train$total_solar_exposure -
                                             mean(daily_2013_train$total_solar_exposure,na.rm=T))/
                                            sd(daily_2013_train$total_solar_exposure,na.rm=T))

datetimes_train <- daily_2013_train$date
datetimes_test <- daily_2013_test$date
ts_daily_train <- xts(daily_2013_train[,-c(1)], order.by = datetimes_train)
ts_daily_test <- xts(daily_2013_test[,-c(1)], order.by=datetimes_test)

### Visualizing Training set-------------------------------------------------------------------------------------

plot(ts_daily_train[,1],main = "Somerton - Epping Rainfall" , xlab = "1st Jan 2013 - 1st Dec 2013", ylab = "daily rainfall (mm)")
plot(ts_daily_train[,2],main = "GT Water Level" , xlab = "1st Jan 2013 - 1st Dec 2013", ylab = "daily water level (lt)")
plot(ts_daily_train[,3],main = "GT Turbidity" , xlab = "1st Jan 2013 - 1st Dec 2013", ylab = "daily turbidity (NTU)")
plot(ts_daily_train[,4],main = "Melb. Airport Maximum Temperature" , xlab = "1st Jan 2013 - 1st Dec 2013", ylab = "daily temperature (*C)")
plot(ts_daily_train[,5],main = "Melb. Airport Total Solar Exposure" , xlab = "1st Jan 2013 - 1st Dec 2013", ylab = "daily total solar exposure (*C)")

autoplot(ts_daily_train[,c(1:5)] , facets = TRUE) +
  xlab("date") + ylab("standardized values") +
  ggtitle("Daily Measurements 2013")

plot(as.data.frame(ts_daily_train), pch=20 , cex=0.45 , col="red")

ts_daily_train_turbidity <- xts(daily_2013_train$daily_turbidity, order.by = datetimes_train)
ts_daily_test_turbidity <- xts(daily_2013_test$daily_turbidity, order.by = datetimes_test)

### Test for stationarity-------------------------------------------------------------------------------------

acf(ts_daily_train)
pacf(ts_daily_train)
plot(ts_daily_train)

length(ts_daily_train[,1])
k = trunc(12*((length(ts_daily_train[,1])/100)^(1/4)))
print(k)
adf.test(ts_daily_train$daily_rain, k=k) # stationary, p<alpha=5%, alt hypothesis (stat) accepted
adf.test(ts_daily_train$daily_WL, k=k) # stationary, p<alpha=5%, alt hypothesis (stat) accepted
adf.test(ts_daily_train$daily_turbidity, k=k) # non-stationary, p>alpha=5%, alt hypothesis (stat) rejected, confirmed by acf
adf.test(ts_daily_train$max_temp, k=k) # non-stationary, p>alpha=5%, alt hypothesis (stat) rejected, confirmed by acf
adf.test(ts_daily_train$total_solar_exposure, k=k) # non-stationary, p>alpha=5%, alt hypothesis (stat) rejected, confirmed by acf

## Dynamic Regression Model with auto.arima on ts_daily_train------------------------------------------------------------------------------------------------------------------------------------

fit.1 <- auto.arima(ts_daily_train[,"daily_turbidity"],xreg = ts_daily_train[,c("daily_rain")])
fit.1 # AICc=155.96
checkresiduals(fit.1) # LJ = white noise, spikes, non sig AC, skewed slightly

fit.2 <- auto.arima(ts_daily_train[,"daily_turbidity"],xreg = ts_daily_train[,c("daily_rain","max_temp")])
fit.2 # AICc=156.67
checkresiduals(fit.2) # LJ = white noise, spikes, non sig AC, skewed slightly

fit.3 <- auto.arima(ts_daily_train[,"daily_turbidity"],xreg = ts_daily_train[,c("daily_rain","total_solar_exposure")])
fit.3 # AICc=157.98
checkresiduals(fit.3) # LJ = white noise, spikes, non sig AC, skewed slightly

fit.4 <- auto.arima(ts_daily_train[,"daily_turbidity"],xreg = ts_daily_train[,c("daily_WL","daily_rain","total_solar_exposure")])
fit.4 # AICc=138.18
checkresiduals(fit.4) # LJ = white noise, spikes, no sig AC, skewed slightly

fit.5 <- auto.arima(ts_daily_train[,"daily_turbidity"],xreg = ts_daily_train[,c("daily_WL", "daily_rain")])
fit.5 # AICc=122.39 BEST
checkresiduals(fit.5) # LJ = white noise, spikes, no sig AC, skewed (but better)

fit.6 <- auto.arima(ts_daily_train[,"daily_turbidity"],xreg = ts_daily_train[,c("daily_WL", "daily_rain", "max_temp")])
fit.6 # AICc=123.48
checkresiduals(fit.6)# LJ = white noise, spikes, no sig AC, skewed (but better)

fit.7 <- auto.arima(ts_daily_train[,"daily_turbidity"],xreg = ts_daily_train[,c("daily_WL", "daily_rain", "max_temp", "total_solar_exposure")])
fit.7 # AICc=125.58
checkresiduals(fit.7)# LJ = white noise, spikes, no sig AC, skewed slightly

ts_daily_test_ts <- ts(daily_2013_test[,"daily_turbidity"], start=311)

fcast <- forecast::forecast(fit.5, h=46, xreg = ts_daily_test[1:46,c("daily_WL", "daily_rain")])
autoplot(fcast) +
  autolayer(ts_daily_test_ts)+
  xlab("Observation no.") + ylab("standardized Turbidity")
fcast # BEST FORECASTS IS FOR BEST MODEL FIT.5

fcast.1 <- forecast::forecast(fit.4, h=46, xreg = ts_daily_test[1:46,c("daily_WL","daily_rain","total_solar_exposure")])
autoplot(fcast.1) +
  autolayer(ts_daily_test_ts)+
  xlab("Observation no.") + ylab("standardized Turbidity")
fcast.1 #effect of adding solar exposure brings the forecasts down a bit on avg

fcast.2 <- forecast::forecast(fit.6, h=46, xreg = ts_daily_test[1:46,c("daily_WL","daily_rain","max_temp")])
autoplot(fcast.2) +
  autolayer(ts_daily_test_ts)+
  xlab("Observation no.") + ylab("standardized Turbidity")
fcast.2 #effect of adding max_temp shoots the forecasts up a bit on avg

fcast.3 <- forecast::forecast(fit.7, h=46, xreg = ts_daily_test[1:46,c("daily_WL","daily_rain","max_temp","total_solar_exposure")])
autoplot(fcast.3) +
  autolayer(ts_daily_test_ts)+
  xlab("Observation no.") + ylab("standardized Turbidity")
fcast.3 #effect of adding max_temp and total_solar_exposure shoots the forecasts up a bit on avg

## Dynamic Harmonic Regression Model with fable and auto.arima on tsibble_train-------------------------------------------------------------------------------------------------

tsibble_train <- daily_2013_train %>% as_tsibble()
tsibble_test <- daily_2013_test %>% as_tsibble()

tsibble_train <- tsibble_train %>% fill_gaps(daily_turbidity = mean(daily_turbidity, na.rm=T))
tsibble_test <- tsibble_test %>% fill_gaps(daily_turbidity = mean(daily_turbidity, na.rm=T))

fit.h <- model(tsibble_train,
               `K = 1` = ARIMA(daily_turbidity ~ fourier(K=1) + PDQ(0,0,0)),
               `K = 2` = ARIMA(daily_turbidity ~ fourier(K=2) + PDQ(0,0,0)),
               `K = 3` = ARIMA(daily_turbidity ~ fourier(K=3) + PDQ(0,0,0)))
glance(fit.h)

best.fit.h <- model(tsibble_train, ARIMA(daily_turbidity ~ fourier(K=1) + PDQ(0,0,0)+ daily_WL + daily_rain))
report(best.fit.h) # AICc = 137.89
best.fit.h %>% gg_tsresiduals()# no sig AC, skewed slightly

new_data_test <- new_data(tsibble_train, 59)%>%
  mutate(daily_WL = tsibble_test$daily_WL,
         daily_rain = tsibble_test$daily_rain)

fcast.s <- best.fit.h %>% forecast::forecast(new_data = new_data_test)
fcast.s %>% autoplot(tsibble_train)

## TBATS Model for ts_daily_train-----------------------------------------------------------------------------------------------------------------------------

ts_daily_train_ts <- ts(daily_2013_train[,"daily_turbidity"], start=c(2013,1), frequency=7)
ts_daily_train_ts %>% tbats() %>% forecast::forecast() %>% autoplot() + xlab("Date") + ylab("daily_turbidity")



















