#residuals analysis

library(stats)
library(tseries)
library(vrtest) #for Ljung-Box lag number choose

#autocorrelation function for the residuals
acf_residuals <- stats::acf(cond_average$residuals, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals, main = "", ylab = "", xlab = "Defasagem")
title("FAC dos resíduos do ARMA(0,1)", adj = 0.5, line = 1)

#Ljung-Box
box_test <- stats::Box.test(cond_average$residuals, lag = 63, type = "Ljung-Box") #check lags
print(box_test)

#autocorrelation function, and partial acf, for the squared residuals
acf_residuals_square <- acf(cond_average$residuals^2,
                            na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals_square, main = "", ylab = "", xlab = "Defasagem")
title("FAC do quadrado dos resíduos do ARMA(0,1)", adj = 0.5, line = 1)
pacf_residuals_square <- stats::pacf(cond_average$residuals^2,
                                     plot = FALSE, na.action = na.pass, max.lag = 25)
plot(pacf_residuals_square, main = "", ylab = "", xlab = "Defasagem")
title("FACP do quadrado dos resíduos do ARMA(0,1)", adj = 0.5, line = 1)

#tests for the distribution of the resilduals, H0: follows a normal dist.
#shapiro_test <- stats::shapiro.test(na.remove(cond_average$residuals))
jarque_bera <- tseries::jarque.bera.test(na.remove(cond_average$residuals))
print(jarque_bera)
