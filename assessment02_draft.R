rm(list=ls())
library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(ggplot2)

coin_portfolio=c("BTC-USD","ETH-USD","ADA-USD",
                 "DOGE-USD","SHIB-USD")
stockindex_portfolio=c("^DJI","^IXIC","^GSPC")
stock_portfolio=c("TSLA","GOOG","AAPL","NVDA","AMD","TSM")

data <- getSymbols(c(coin_portfolio,
                     stockindex_portfolio,
                     stock_portfolio),
                   src='yahoo',
                   #from=dyear,
                   #to=d,
                   autoassign=FALSE)



#chartSeries(na.omit(merge(`BTC-USD`/100,TSLA, join="left")))

#chartSeries(na.omit(merge(TSLA,DJI/100, join="left")))

# brief topic 1
cor(na.omit(merge(`BTC-USD`,TSLA,join="left")))

# topic 2
# data transformation
dataBTC = data.frame(date=index(`BTC-USD`), coredata(`BTC-USD`))
dataTSLA = data.frame(date=index(TSLA), coredata(TSLA))
dataDJI = data.frame(date=index(DJI), coredata(DJI))

mdBTCTSLA = na.omit(merge(dataBTC,dataTSLA,join="left"))
lmBTCTSLA = lm(BTC.USD.Close~TSLA.Close,data=mdBTCTSLA)
summary(lmBTCTSLA)

# plot data
ggplot(mdBTCTSLA, aes(BTC.USD.Close, TSLA.Close)) +
  geom_point(color='red') +
  geom_smooth(method='lm')

# topic 3 - within 1 year
mdBTCTSLA = na.omit(merge(dataBTC,dataTSLA,join="left"))
mdBTCTSLA21 <- mdBTCTSLA[mdBTCTSLA$date > "2021-01-01",]

lmBTCTSLA21 = lm(BTC.USD.Close~TSLA.Close,data=mdBTCTSLA21)
summary(lmBTCTSLA21)

ggplot(mdBTCTSLA21, aes(BTC.USD.Close, TSLA.Close)) +
  geom_point(color='red') +
  geom_smooth(method='lm')


# ggfortify
library(ggfortify)
fit <- lm(BTC.USD.Close~TSLA.Close,data=mdBTCTSLA21)
autoplot(fit)
