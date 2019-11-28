#packages
install.packages("readxl", "xts", "zoo")
library(readxl)
library(xts)
library(zoo)

#loading data into a dataframe
closing_prices <- readxl::read_xlsx('data/acoesBR.xlsx', na = "-", )

colnames(closing_prices) <- closing_prices[3, ]
closing_prices <- closing_prices[-(1:3), ]

#creating the index
date_string <- as.integer(closing_prices$Data)
index <- as.Date(date_string, origin = "1899-12-30")

#creating the time series
petro4_xts <- xts(closing_prices[, 393], order.by = index)
colnames(petro4_xts) <- "PETRO4"

#excluding NAs
petro4_xts <- na.omit(petro4_xts)

saveRDS(petro4_xts, file = "data/petro4.rds")
