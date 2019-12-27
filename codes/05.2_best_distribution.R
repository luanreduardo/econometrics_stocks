#Residual Distribution Best Fit Test

library(rugarch)


#NORM
garch_spec_norm <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(0,1)), distribution.model = "norm")
garch_fit_norm <- ugarchfit(spec = garch_spec_norm, data = petr4_returns)
print(garch_fit_norm)

#SNORM
garch_spec_snorm <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(0,1)), distribution.model = "snorm")
garch_fit_snorm <- ugarchfit(spec = garch_spec_snorm, data = petr4_returns)
print(garch_fit_snorm)

#STD
garch_spec_std <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(0,1)), distribution.model = "std")
garch_fit_std <- ugarchfit(spec = garch_spec_std, data = petr4_returns)
print(garch_fit_std)

#SSTD
garch_spec_sstd <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(0,1)), distribution.model = "sstd")
garch_fit_sstd <- ugarchfit(spec = garch_spec_sstd, data = petr4_returns)
print(garch_fit_sstd)



bic_values <- c(-4.6062, -4.6061, -4.6431, -4.6418)
names_dist <- c('norm', 'snorm', 'std', 'sstd')
bic_values <- as.data.frame(bic_values, row.names = names_dist)
print(bic_values)
