#This code uses ARIMA and GARCH models to make predictions on Petrobras
#stocks returns (PETR4)

#cleaning and adjusting the data
source("codes/01_adjusting_data.R")

#exploratory analysis
source("codes/02_exploratory_analysis.R")

#ARIMA modelling
source("codes/03_arima_modelling.R")

#residuals analysis
source("codes/04_residuals_analysis.R")

#GARCH modelling
source("codes/05_arma_garch_modelling.R")

#ARMA-GARCH residual analysis
source("codes/06_arma_garch_residual_analysis.R")

#results and accuracy
source("codes/07_results_forecasting.R")
