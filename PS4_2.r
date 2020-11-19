#The parts of Time Series was inspired by TaoTaoSun and Li Yuan
#I learned the code from Li
#This part,I haven't grasp it.

library(dplyr)
library(lubridate)
library(forecast)


#read as tibble
BaoAn_data_tbl <- as_tibble(read.csv(file = "2281305.csv", header = T))

#clean  data
BaoAn_data_tbl <- BaoAn_data_tbl %>%
  select(DATE,TMP) %>%
  mutate(
    BaoAn_T_value = as.numeric(substr(TMP,1,5)),
    BaoAn_T_flag = as.logical(as.numeric(substr(TMP,7,7)))
  ) %>% 
  
  filter(BaoAn_T_value!= 9999 & BaoAn_T_flag==TRUE) %>% 
  mutate(BaoAn_T_value2 = BaoAn_T_value * 0.1,
         Obs_Time = as.Date(DATE))

#show the clean data
plot(BaoAn_data_tbl$Obs_Time, BaoAn_data_tbl$BaoAn_T_value2, 
     lwd=0.5, type="l", col="red")
#examine the min and max
min(BaoAn_data_tbl$BaoAn_T_value2, na.rm=T) 
max(BaoAn_data_tbl$BaoAn_T_value2, na.rm=T) 

#get monthly data
BaoAn_data_tbl <- BaoAn_data_tbl %>%
  select(Obs_Time,BaoAn_T_value2) %>%
  group_by(Obs_Time) %>%
  summarise(BaoAn_T_value_monthly = mean(BaoAn_T_value2)) 


# Quick plot
plot(BaoAn_data_tbl$Obs_Time,BaoAn_data_tbl$BaoAn_T_value_monthly, 
     type="l",xlab="Date",ylab="BaoAn_T_value")

#-------------------------------------------------
#2.1 time series from 2010 Jan. to 2020 Aug.
monthly_averaged_temp_ts <- ts(BaoAn_data_tbl$BaoAn_T_value_monthly, 
                               start=c(2010,1), end=c(2020,8),frequency=12)
# Check structure
str(monthly_averaged_temp_ts)
# Plot time series
plot(monthly_averaged_temp_ts, type="l")

#-------------------------------------------------
#2.2 Decompose the time series into trend, seasonality, 
#and error parts. 
#-------------------------------------------------
time_series_components <- decompose(monthly_averaged_temp_ts)
plot(time_series_components)

# Plot hist
hist(time_series_components$random, prob=TRUE)
# Add pdf
curve(dnorm(x, mean=mean(time_series_components$random,
                         na.rm=T),sd=sd(time_series_components$random,na.rm=T)),
      add=TRUE, col="red")


#2.3 Fit an ARIMA(p,d,q) model to the time series. 
trModel <- lm(monthly_averaged_temp_ts ~ c(1:length(monthly_averaged_temp_ts)))
plot(resid(trModel), type="l")

#Check acf and pacf
acf(resid(trModel))
pacf(resid(trModel))

#Automated forecasting using an ARIMA model
auto.arima(monthly_averaged_temp_ts,trace=T)

#Select a better model between different order
arima1 <- arima(monthly_averaged_temp_ts,order=c(1,0,0),seasonal=list(order=c(1,0,0),period=12),method="ML")
arima1
arima2 <- arima(monthly_averaged_temp_ts,order=c(1,0,0),seasonal=list(order=c(2,1,0),period=12),method="ML")
arima2


#2.4 Predict monthly-averaged temperatures in 2020 Sep. and Oct
model <- arima2
months_forecast  <- 2
months_in_plot <- 20
forecast_2months <- forecast(model, months_forecast)

# Plot predictions along with real values
plot(forecast(model, months_forecast), include = months_in_plot, 
     xlab="Time", 
     ylab="Temperature",type="o",lwd=1,
     col = "red") 