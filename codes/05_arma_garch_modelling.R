#Garch modelling

library(fGarch)
library(xts)
library(stats)
library(rugarch)

#all possible m, n for garch modelling
pars_arma_garch <- expand.grid(m = 1:5, n = 0:5)

#list for results
arma_garch_model <- list()

#arma specification defined on previous codes
arma_set <- "~arma(0,1)"

# probability distribution for the residuals (norm, std, snorm, sstd)
arma_residuals_dist <- "sstd"

#skew and assimetry definitions
include.skew = FALSE
include.shape = TRUE

#parameters estimation through maximum likelihood
for (i in 1:nrow(pars_arma_garch)) {
  arma_garch_model[[i]] <- fGarch::garchFit(as.formula(paste0(arma_set,"+","garch(",pars_arma_garch[i,1],",",pars_arma_garch[i,2], ")")),
                                             data = petr4_returns, trace = FALSE, cond.dist = arma_residuals_dist,
                                             include.skew = include.skew, include.shape = include.shape) 
}

# function maximum value
log_likelihood_arma_garch <- list()
for (i in 1:length(arma_garch_model)) {
  log_likelihood_arma_garch[[i]] <- arma_garch_model[[i]]@fit$llh
}

# AIC
aicarma_garch <- list()
for (i in 1:length(arma_garch_model)) {
  aicarma_garch[[i]] <- arma_garch_model[[i]]@fit$ics[1]
}

# BIC
bicarma_garch <- list()
for (i in 1:length(arma_garch_model)) {
  bicarma_garch[[i]] <- arma_garch_model[[i]]@fit$ics[2]
}

# number of estimated parameters
arma_garch_param_number <- list()
for (i in 1:length(arma_garch_model)) {
  arma_garch_param_number[[i]] <- length(arma_garch_model[[i]]@fit$coef)
}

# table of results
spec <- paste0(arma_set,"-","garch",pars_arma_garch$m,pars_arma_garch$n)
sample_size <- rep(length(petr4_returns), length(arma_garch_model))
results_arma_garch <- data.frame(spec, ln_likelihood = unlist(log_likelihood_arma_garch),
                                 param_number = unlist(arma_garch_param_number),
                                 sample_size, aic = unlist(aicarma_garch), bic = unlist(bicarma_garch),
                                 stringsAsFactors = FALSE, row.names = NULL)
print(results_arma_garch)

#using rugarch modelling package for it provides greater possibilities of analysis
ugarch_version_spec <- rugarch::ugarchspec(variance.model = list(garchOrder = c(1,1)), 
                                  mean.model = list(armaOrder = c(0,1)), distribution.model = "sstd")
chosen_arma_garch <- rugarch::ugarchfit(spec = ugarch_version_spec, data = petr4_returns)
print(chosen_arma_garch)
