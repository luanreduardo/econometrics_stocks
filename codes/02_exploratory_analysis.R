#exploratory analysis

library(xts)
library(fUnitRoots)
library(stats)

#reading data
petr4_xts <- readRDS('data/petr4.rds')
petr4_returns <- readRDS('data/p4_returns.rds')

#plotting the timeseries of prices and returns
plot.xts(petr4_xts$PETR4, main = "Preços PETR4")
plot.xts(petr4_returns, main = "Retornos PETR4")

#testing stationarity, H0 points to random walk
#no constant, no trend
unit_root_nc <- fUnitRoots::adfTest(petr4_returns, lags = 2, type = c("nc")) 
#constant, no trend
unit_root_c <- fUnitRoots::adfTest(petr4_returns, lags = 2, type = c("c"))
#constant, trend
unit_root_ct <- fUnitRoots::adfTest(petr4_returns, lags = 2, type = c("ct"))

#auto-correlation and partial ac functions
acf_petr4_ret <- stats::acf(petr4_returns, na.action = na.pass, plot = FALSE, lag.max = 10)
pacf_petr4_ret <- stats::pacf(petr4_returns, na.action = na.pass, plot = FALSE, lag.max = 10)

par(mfrow = c(2,1))
plot(acf_petr4_ret, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)", adj = 0.5, line = 1)
plot(pacf_petr4_ret, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)", adj = 0.5, line = 1)
par(mfrow = c(1,1))