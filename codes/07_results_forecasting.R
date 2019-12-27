#Results and accuracy

library(stats)
library(fGarch)
library(rugarch)
library(forecast)
library(quantmod)
library(PerformanceAnalytics)
library(xts)

#plotting selected arma-garch model
fGarch::plot(chosen_arma_garch, which = 'all')
fGarch::plot(chosen_arma_garch, which = 1)

#out-of-sample data from quantmod
out_of_sample_prices <- quantmod::getSymbols(Symbols = "PETR4.SA", src = "yahoo", from = '2019-10-01')
out_of_sample_return <- na.omit(PerformanceAnalytics::Return.calculate(PETR4.SA$PETR4.SA.Close, method = "log"))

#plotting out-of-sample return set
plot.xts(out_of_sample_return, main = "Dados fora da amostra - PETR4")

#forecasting 3 months out of sample
arima_garch_forecast <- rugarch::ugarchforecast(chosen_arma_garch, n.ahead = 63)
print(arima_garch_forecast)
fGarch::plot(arima_garch_forecast, which = 1)
fGarch::plot(arima_garch_forecast, which = 3)

#plotting forecast against out-of-sample data
mean_arma_garch <- arima_garch_forecast@forecast$seriesFor
upper_bound_garch <- arima_garch_forecast@forecast$seriesFor + arima_garch_forecast@forecast$sigmaFor
lower_bound_garch <- arima_garch_forecast@forecast$seriesFor - arima_garch_forecast@forecast$sigmaFor

ylim <- c(min(out_of_sample_return), max(out_of_sample_return))
plot.ts(out_of_sample_return , col = "blue", ylim = ylim, main = '63 períodos fora da amostra')
lines(mean_arma_garch,col = "green", ylim = ylim, lwd  = 2)
lines(upper_bound_garch, col = "red", ylim = ylim, lwd = 2)
lines(lower_bound_garch, col = "red", ylim = ylim, lwd = 2)

#accuracy
accuracy_test_set <- forecast::accuracy(as.ts(arima_garch_forecast@forecast$seriesFor), out_of_sample_return)
theils_u <- DescTools::TheilU(out_of_sample_return, 
                              arima_garch_forecast@forecast$seriesFor[0:length(out_of_sample_return),], type = 2)


#cross-validation
forecast_function <- function(x, h){forecast(Arima(x, order = c(0,0,1)), h = h)}
cross_val <- tsCV(petr4_returns, forecast_function, h = 1)
RMSE_cross_val <- sqrt(mean(cross_val^2, na.rm = T))
