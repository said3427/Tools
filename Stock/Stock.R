#Stock Prophet
#install.packages("prophet")
#install.packages("crypto")

library(prophet)
library(reshape)
library(crypto)
library(blastula)

#setwd("~/Github/Tools/Stock/")
cryptocoin<- crypto_history("xlm")
#cryptocoin<- crypto_history("btc")
#df <- read.csv('stock-price/GOOG.csv')

df<-cryptocoin[,c(4,6:9)]
df.melt<-melt(df,id.vars = "date")
df.melt<-df.melt[,-2]
colnames(df.melt)<-c("ds","y")

##Starting Prophet
m <- prophet(df.melt,daily.seasonality = TRUE)

#Crea un vector de fechas futuras
future <- make_future_dataframe(m, periods = 7)

#Predicción de la crypto currency
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])


#plot(m, forecast)

prophet_plot_components(m, forecast,)

#plot(m, forecast) + add_changepoints_to_plot(m)

