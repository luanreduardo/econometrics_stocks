#This code uses ARIMA and GARCH models to make predictions on Petrobras
#stocks returns (PETR4)

#cleaning and adjusting the data
source("codes/01_adjusting_data.R")

#exploratory analysis
source("codes/02_exploratory_analysis.R")

#ARIMA modelling
source("codes/03_arima_modelling")

#residuals analysis
source("codes/04_residuals_analysis.R")

