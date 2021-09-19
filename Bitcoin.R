rm(list=ls())
library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)
library(corrplot)
library(tidyverse)
library(janitor)
library(here)
library(skimr)
library(ggpubr)
library(tidyquant)
library(timetk)
library(tibbletime)

# The symbols vector holds our tickers. 
symbols <- c("BTC-USD")

# The prices object will hold our raw price data throughout this book.
prices <- 
  getSymbols(symbols, src = 'yahoo', from = "2014-01-01", 
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>%
  `colnames<-`(symbols)
head(prices)
# to get prices at month end
prices_monthly <- to.monthly(prices, indexAt = "last", OHLC = FALSE)
head(prices_monthly)

# caculate monthly return
asset_returns_xts <- na.omit(Return.calculate(prices_monthly, method = "log"))
head(asset_returns_xts)

BTCreturn <- fortify(asset_returns_xts)
BTCtimeseries <- ts(BTCreturn$`BTC-USD`, frequency=12, start=c(2014,10))
BTCtimeseries

plot.ts(BTCtimeseries)

BTCtimeseriescomponents <- decompose(BTCtimeseries)

BTCtimeseriescomponents$seasonal # get the estimated values of the seasonal component
plot(BTCtimeseriescomponents)

print(BTCtimeseriescomponents$seasonal)
