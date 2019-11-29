#residuals analysis

library(stats)
library(tseries)

#autocorrelation function for the residuals
acf_residuals <- stats::acf(cond_average$residuals, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals, main = "", ylab = "", xlab = "Defasagem")
title("FAC dos resíduos do ARMA(1,2)", adj = 0.5, line = 1)

#Ljung-Box
box_test <- stats::Box.test(cond_average$residuals, lag = 2, type = "Ljung-Box")
print(box_test)

#autocorrelation function for the squared residuals
acf_residuals_square <- acf(cond_average$residuals^2,
                            na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals_square, main = "", ylab = "", xlab = "Defasagem")
title("FAC do quadrado dos resíduos do ARMA(1,2)", adj = 0.5, line = 1)

#tests for the distribution of the resilduals, H0: follows a normal dist.
#shapiro_test <- stats::shapiro.test(na.remove(cond_average$residuals))
jarque_bera <- tseries::jarque.bera.test(na.remove(cond_average$residuals))
print(jarque_bera)
