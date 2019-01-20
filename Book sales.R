# From
# https://data2decision.net/2018/12/24/predicting-the-demise-of-retail-bookstores-a-time-series-forecasting/

# Required packages 
library(fpp2)
library(forecast)
library(readxl)
library(ggplot2)
library(seasonal)
library(dplyr)

# data import 
df = read_excel("BookSales.xls", skip=6)

# keep only the `Value` column  
df = df[, c(2)]
# convert the values into a time series object  
series = ts(df, start = 1992, frequency =12)
options(repr.plot.width = 6, repr.plot.height = 3)   
# plot the series   
autoplot(series)+   xlab(" ") + ylab("Retail sales (million US$)") + ggtitle(" Figure 1: Bookstores sales series")+  theme(plot.title = element_text(size=8))

options(repr.plot.width = 6, repr.plot.height = 3)

# Aggregate annual sales
annual_sales=aggregate(series, nf=1, FUN=sum) # nf=1 > annual; nf=4 > quarterly; nf=12 > monthly 
autoplot(annual_sales)

# Seasonal sub-series plot  
options(repr.plot.width = 10, repr.plot.height = 3) 

series_season = window(series, start=c(1992,1), end=c(2018,10)) 

ggsubseriesplot(series_season) + ylab(" ") + ggtitle("Figure 2: Seasonal sub-series plot (horizontal bars represent monthly mean)")+ ylab("Retail sales (million US$)")+ theme(plot.title = element_text(size=10))

options(repr.plot.width = 6, repr.plot.height = 3)
# remove seasonality (monthly variation) to see yearly changes
series_ma = ma(series, 12)
autoplot(series_ma) + 
    xlab("Time") + ylab("Retail sales (million US$)")+
    ggtitle("Figure 3: The series after removing seasonality" )+
    theme(plot.title = element_text(size=8))

options(repr.plot.width = 6, repr.plot.height = 3)
# zooming in to the recent trend
series_downtime = window(series_ma, start=c(2007,1), end=c(2018,10))
autoplot(series_downtime) + 
    xlab("Time") + ylab("Retail sales (million US$)")+
    ggtitle(" Figure 4: Bookstores sales recent trend")+
    theme(plot.title = element_text(size=8))

# decomposition
options(repr.plot.width = 6, repr.plot.height = 3)
autoplot(decompose(series)) + ggtitle("Figure 6: Decomposition of the series")+
    theme(plot.title = element_text(size=8))

# predictor series
predictor_series =  window(series, start=c(2007,1), end=c(2018,10))

# model
forecast_hw=hw(predictor_series, seasonal="multiplicative", h=260)


options(repr.plot.width = 10, repr.plot.height = 3)
# ploting the predictions
autoplot(series) +
    autolayer(predictor_series, size = 2, series = "Predictor 2007-2018 series")+ 
    autolayer(series, series = " 1992-2018 series") +
    autolayer(series_ma, series = "1992-2018 MA series")+
    autolayer(forecast_hw, series="Holt-Winter forecast")+
    xlab("Time") + ylab("Retail sales (million US$)")+
    ggtitle("Figure 7: HW Exponential Smoothing")+
    theme(plot.title = element_text(size=8))

# Fiting & prediction with ARIMA
fit.arima = auto.arima(series, seasonal=TRUE, stepwise = FALSE, approximation = FALSE) 
forecast_arima = forecast(fit.arima, h=160)


options(repr.plot.width = 10, repr.plot.height = 3)
# ploting ARIMA predictions
autoplot(series, series=" 1992-2018 series")+
    autolayer(series_ma, series=" MA of Input series")+
    autolayer(forecast_arima, series=" ARIMA Forecast")+
    ggtitle(" Figure 9: ARIMA forecasting")+
    theme(plot.title = element_text(size=8))

# Fiting & prediction with ARIMA on 2007-2018 series
fit.arima2 = auto.arima(predictor_series, seasonal=TRUE, stepwise = FALSE, approximation = FALSE) 
forecast_arima2 = forecast(fit.arima2, h=160)


options(repr.plot.width = 10, repr.plot.height = 3)
# ploting ARIMA predictions
autoplot(series, series=" 1992-2018 series")+
    autolayer(predictor_series, series=" Predictor series")+
    autolayer(forecast_arima2, series=" ARIMA Forecast")+
    ggtitle(" Figure 10: ARIMA forecasting on 2007-2018 period")+
    theme(plot.title = element_text(size=8))
