#Results and predictions

library(stats)
library(fGarch)
library(forecast)

#plotting selected arma-garch model
plot(arma_garch_model[[4]], which = 3)

#accuracy

#forecasting
forecast <- fGarch::predict(arma_garch_model[[4]], n.ahead = 63)
print(forecast)
plot(forecast, which = 3)
