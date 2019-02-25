#Stock Prophet
#install.packages("prophet")
library(prophet)
library(reshape)

setwd("~/Github/Tools/Stock/")
df <- read.csv('stock-price/GOOG.csv')

df<-df[,1:4]
df.melt<-melt(df)
df.melt<-df.melt[,-2]
colnames(df.melt)<-c("ds","y")

##Starting Prophet
m <- prophet(df.melt,daily.seasonality = TRUE)

future <- make_future_dataframe(m, periods = 365)
tail(future)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])


plot(m, forecast)

prophet_plot_components(m, forecast)
