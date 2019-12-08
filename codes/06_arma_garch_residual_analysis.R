# ARMA-GARCH residual analysis

library(fGarch)
library(stats)

# squared residuals acf
acf_residuals_square_arma_garch <- acf(fGarch::residuals(arma_garch_model[[4]], standardize = TRUE)^2, 
                                       na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals_square_arma_garch, main = "", ylab = "", xlab = "Defasagem")
title("FAC do quadrado dos resíduos do ARMA(0,1)-GARCH(1,1)", adj = 0.5, line = 1)

#Ljung-Box
box_test <- stats::Box.test(fGarch::residuals(arma_garch_model[[4]], standardize = TRUE),
                            lag = 63, type = "Ljung-Box") #check lags
print(box_test)