#packages
library(readxl)
library(xts)
library(zoo)
library(PerformanceAnalytics)

#loading data into a dataframe
closing_prices <- readxl::read_xlsx('data/acoesBR.xlsx', na = "-", )

#creating the index
date_string <- as.Date(closing_prices$Data)

#creating the time series
petr4_xts <- xts::xts(closing_prices[, 393], order.by = date_string)
colnames(petr4_xts) <- "PETR4"

#excluding NAs
petr4_xts <- na.omit(petr4_xts)

#sub-setting to 1996 forward
petr4_xts <- petr4_xts['1996/']

#calculating log returns
petr4_returns <- PerformanceAnalytics::Return.calculate(petr4_xts, method = "log")
petr4_returns <- na.omit(petr4_returns)

saveRDS(petr4_xts, file = "data/petr4.rds")
saveRDS(petr4_returns, file = "data/p4_returns.rds")
