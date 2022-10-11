# ------------------------------------------------------
# AUTHOR: BHARGAV RELE
# ------------------------------------------------------
library(tidyverse)
library(mgcv)
library(mgcViz)
library(tsibble)
library(feasts)
library(lubridate)
library(forecast)
library(modelr)
library(reshape2)
library(gam)
library(ggplot2)
library(cowplot)
library(bdscale)
library(scales)

# FINAL MODEL GAM - CROSS VALIDATION [1-PERIOD AHEAD FORECAST]--------------------------------------------

daily_2013_2014_rain_lev_turb <- read_csv("Desktop/water quality RA/data files/initial/created datasets/daily_2013_2014_rain_lev_turb.csv",
                                          col_types = cols(daily_rain = col_number(),
                                                           daily_WL = col_number(), daily_turbidity = col_number(),
                                                           max_temp = col_number(), total_solar_exposure = col_number(),
                                                           date = col_date(format = "%Y-%m-%d")))

daily_2013_2014_lagged <- daily_2013_2014_rain_lev_turb
daily_2013_2014_lagged$lag_1 <- lag(daily_2013_2014_rain_lev_turb$daily_turbidity, 1)
daily_2013_2014_lagged$lag_2 <- lag(daily_2013_2014_rain_lev_turb$daily_turbidity, 2)


all_gams_folds_forecast <- data.frame(lower_95=numeric(0),upper_95=numeric(0),forcasted_turbidity=numeric(0),daily_turbidity=numeric(0))
a_start <- 1
a <- 200
p <- 4

start.time <- Sys.time()

while(a <356){

  model1 <- mgcv::gam(daily_turbidity ~ s(daily_rain,k=4) + s(daily_WL,k=4) + s(max_temp,k=4) + s(total_solar_exposure) + lag_1 + lag_2,
                      data = daily_2013_2014_lagged[a_start:a,])

  fcasts <- predict(model1,  newdata = daily_2013_2014_lagged[(a+1):(a+p),], se.fit=TRUE)
  upr <- fcasts$fit + (2 * fcasts$se.fit)
  lwr <- fcasts$fit - (2 * fcasts$se.fit)

  one_period_forecast <- data.frame(lower_95=lwr[1],
                                    upper_95=upr[1],
                                    forecasted_turbidity = fcasts$fit[1],
                                    daily_turbidity=daily_2013_2014_lagged[(a+1), "daily_turbidity"])
  all_gams_folds_forecast <- rbind(all_gams_folds_forecast,one_period_forecast)

  a_start <- a_start+1 #change to p
  a <- a+1 #change to p
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken # 7.419253 secs

#last fold diagnostics
hist(model1$residuals, xlab="model residuals", main = "histogram of model residuals")
gam.check(model1, cex=4)
plot(model1$residuals, type="l", ylab="model residuals", xlab="observation no.", main="line plot for model residuals")

ts_fcasts_turbidity <- c(rep(NA, 200), all_gams_folds_forecast)
plot(daily_2013_2014_rain_lev_turb$daily_turbidity, type="l",
     main="GAM Cross-validation",
     xlab="Observation No.",
     ylab="Turbidity")
lines(ts_fcasts_turbidity, col = "blue")
legend(220, 270, legend=c("Actual", "Forecasted"),
       col=c("black", "blue"), lty=1, cex=0.8)

dates <- daily_2013_2014_rain_lev_turb$date
gam_forecasts <- c(rep(NA, 200), all_gams_folds_forecast$forecasted_turbidity)
upper_bound <- c(rep(NA, 200), all_gams_folds_forecast$upper_95)
lower_bound <- c(rep(NA, 200), all_gams_folds_forecast$lower_95)
actual_values <- daily_2013_2014_rain_lev_turb$daily_turbidity
gam_results_df <- data.frame(dates, gam_forecasts, upper_bound, lower_bound, actual_values)
ggplot(gam_results_df, aes(x=dates, y=actual_values)) +
  geom_line(colour="black") +
  geom_ribbon(aes(x=dates, ymax=upper_bound, ymin=lower_bound), fill="grey",alpha=0.6) +
  geom_line(aes(x=dates, y=gam_forecasts), colour="blue") +
  theme_bw() + labs(x="Date", y="Turbidity (NTU)")+
  scale_x_bd(business.dates = dates, labels=date_format('%b%y')) + theme(axis.title.x = element_blank(),
                                                                         axis.text.x=element_text(size=11),
                                                                         axis.text.y=element_text(size=11))
ggsave(filename="cvGAMs final.png",path = "~/Desktop/water quality RA/z - preprint",
       width = 8, height = 5, units="in",
       device='tiff', dpi=150)

exceed_upper_bound_GAMs <- all_gams_folds_forecast %>% filter(all_gams_folds_forecast$daily_turbidity > all_gams_folds_forecast$upper_95)
exceed_lower_bound_GAMs <- all_gams_folds_forecast %>% filter(all_gams_folds_forecast$daily_turbidity < all_gams_folds_forecast$lower_95)
nrow(exceed_upper_bound_GAMs)/nrow(all_gams_folds_forecast) #21.15%
nrow(exceed_lower_bound_GAMs)/nrow(all_gams_folds_forecast) #16.03%
(nrow(exceed_upper_bound_GAMs)+nrow(exceed_lower_bound_GAMs))/nrow(all_gams_folds_forecast)# 37.18%


# Preliminary analysis to find best model  ---------------------------------------------------------

## Training and testing set ------------------------------------------------------------------------

daily_2013_test <- read_csv("Desktop/water quality RA/data files/initial/created datasets/daily_2013_test.csv",
                            col_types = cols(daily_rain = col_number(),
                                             daily_WL = col_number(), daily_turbidity = col_number(),
                                             max_temp = col_number(), total_solar_exposure = col_number(),
                                             date = col_date(format = "%Y-%m-%d")))

daily_2013_train <- read_csv("Desktop/water quality RA/data files/initial/created datasets/daily_2013_train.csv",
                             col_types = cols(daily_rain = col_number(),
                                              daily_WL = col_number(), daily_turbidity = col_number(),
                                              max_temp = col_number(), total_solar_exposure = col_number(),
                                              date = col_date(format = "%Y-%m-%d")))

### Standardizing daily_2013_train and daily_2013 test------------------------------------------------

daily_2013_2014_rain_lev_turb$daily_rain <- ((daily_2013_2014_rain_lev_turb$daily_rain -
                                                mean(daily_2013_train$daily_rain,na.rm=T))/
                                               sd(daily_2013_train$daily_rain,na.rm=T))
daily_2013_2014_rain_lev_turb$daily_WL <- ((daily_2013_2014_rain_lev_turb$daily_WL -
                                              mean(daily_2013_train$daily_WL,na.rm=T))/
                                             sd(daily_2013_train$daily_WL,na.rm=T))
daily_2013_2014_rain_lev_turb$daily_turbidity <- ((daily_2013_2014_rain_lev_turb$daily_turbidity -
                                                     mean(daily_2013_train$daily_turbidity,na.rm=T))/
                                                    sd(daily_2013_train$daily_turbidity,na.rm=T))
daily_2013_2014_rain_lev_turb$max_temp <- ((daily_2013_2014_rain_lev_turb$max_temp -
                                              mean(daily_2013_train$max_temp,na.rm=T))/
                                             sd(daily_2013_train$max_temp,na.rm=T))
daily_2013_2014_rain_lev_turb$total_solar_exposure <- ((daily_2013_2014_rain_lev_turb$total_solar_exposure -
                                                          mean(daily_2013_train$total_solar_exposure,na.rm=T))/
                                                         sd(daily_2013_train$total_solar_exposure,na.rm=T))

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

## Stepwise variable selection ------------------------------------------------------------------------------------

### Base model ---------------------------------------------------------------------------------------------------------

model_gam <- gam::gam(daily_turbidity ~ daily_rain + daily_WL + max_temp + total_solar_exposure, data = daily_2013_train)

### Finding best model ---------------------------------------------------------------------------------------------------------

step_model_gam<-gam::step.Gam(model_gam, scope=list("daily_rain"=~1+daily_rain+s(daily_rain,4)+s(daily_rain,6)+s(daily_rain,12),
                                                    "daily_WL"=~1+daily_WL+s(daily_WL,4)+s(daily_WL,5)+s(daily_WL,6)+s(daily_WL,12),
                                                    "max_temp"=~1+max_temp+s(max_temp,4)+s(max_temp,5)+s(max_temp,6)+s(max_temp,12),
                                                    "total_solar_exposure"=~1+total_solar_exposure+s(total_solar_exposure,4)+s(total_solar_exposure,6)+s(total_solar_exposure,12)))

## Best model -----------------------------------------------------------------------------------------------------------------

best_model_gam<-gam::gam(daily_turbidity ~ s(daily_rain, 4) + s(daily_WL, 4) + s(max_temp, 4) + s(total_solar_exposure, 12),
                         data = daily_2013_train)

### Diagnostics ----------------------------------------------------------------------------------------------------------------

summary(best_model_gam)
best_model_gam$aic
hist(best_model_gam$residuals, xlab="model residuals", main = "histogram of model residuals")
gam.check(best_model_gam, cex=4)
plot(best_model_gam$residuals, type="l", ylab="model residuals", xlab="observation no.", main="line plot for model residuals")

## Predictions over testing set ----------------------------------------------------------------------------------------------

p  <- predict(best_model_gam,  newdata = daily_2013_test)
ts_fcasts_turbidity <- c(rep(NA, 310), p)
plot(daily_2013_2014_rain_lev_turb$daily_turbidity, type="l",
     main="GAM training and testing standardised",
     xlab="Observation No.",
     ylab="Turbidity")
lines(ts_fcasts_turbidity, col = "blue")

## Introducing turbidity lags 1-lag and 2-lags ---------------------------------------------------------------------

daily_2013_2014_lagged <- daily_2013_2014_rain_lev_turb

daily_2013_2014_lagged$lag_1 <- lag(daily_2013_2014_rain_lev_turb$daily_turbidity, 1)
daily_2013_2014_lagged$lag_2 <- lag(daily_2013_2014_rain_lev_turb$daily_turbidity, 2)

daily_2013_train_lagged <- daily_2013_2014_lagged[1:310,]
daily_2013_test_lagged <- daily_2013_2014_lagged[311:356,]

## Best model using gams with lag_1 and lag_2 ---------------------------------------------------------------------

model1 <- gam::gam(daily_turbidity ~ s(daily_rain, 4) + s(daily_WL, 4) + s(max_temp, 4) + s(total_solar_exposure, 12) + lag_1 + lag_2,
                   data = daily_2013_train_lagged, se.fit=TRUE)

summary(model1)
model1$aic
hist(model1$residuals, xlab="model residuals", main = "histogram of model residuals")
gam.check(model1, cex=4)
plot(model1$residuals, type="l", ylab="model residuals", xlab="observation no.", main="line plot for model residuals")
plot.gam(model1 )

## Predictions using gams ---------------------------------------------------------------------

p <- predict(model1,  newdata = daily_2013_test_lagged)
ts_fcasts_turbidity <- c(rep(NA, 310), p)
plot(daily_2013_2014_rain_lev_turb$daily_turbidity, type="l",
     main="GAM training and testing standardised",
     xlab="Observation No.",
     ylab="Turbidity")
lines(ts_fcasts_turbidity, col = "blue")

## Best model using mgcv with lag_1 and lag_2 ---------------------------------------------------------------------

model2 <- mgcv::gam(daily_turbidity ~ s(daily_rain, k=4) + s(daily_WL, k=4) + s(max_temp, k=4)+ s(total_solar_exposure, k=12) + lag_1 + lag_2,
                    data = daily_2013_train_lagged)

summary(model2)
model2$aic


model3 <- mgcv::gam(daily_turbidity ~ s(daily_rain, k=4) + s(daily_WL, k=4) + s(max_temp, k=4)+s(total_solar_exposure) + lag_1 + lag_2,
                    data = daily_2013_train_lagged)
summary(model3)
model3$aic

## Predictions using mgcv  ---------------------------------------------------------------------

p <- predict(model3,  newdata = daily_2013_test_lagged, se.fit=TRUE)
upr <- p + (2 * p$se.fit)
lwr <- p - (2 * p$se.fit)
plot(p,shade=TRUE)

