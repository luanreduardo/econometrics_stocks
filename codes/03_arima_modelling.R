#ARIMA modelling

library(stats)


#loading data
petr4_returns <- readRDS('data/p4_returns.rds')

#possibilities equal to or smaller than the last lag of ACF/PACF
feasible_set <- expand.grid(ar = 0:1, diff = 0, ma = 0:1)

#defining a list for the results
model <- list()

#estimating parameters through maximum likelihoood
for (i in 1:nrow(feasible_set)) {
  model[[i]] <- stats::arima(petr4_returns, order = unlist(feasible_set[i, 1:3]), method = "ML")
}

#greater value through log of ML
log_likelihood <- list()
for (i in 1:length(model)) {
  log_likelihood[[i]] <- model[[i]]$loglik
}

#AIC
aicarma <- list()
for (i in 1:length(model)) {
  aicarma[[i]] <- stats::AIC(model[[i]])
}

#BIC
bicarma <- list()
for (i in 1:length(model)) {
  bicarma[[i]] <- stats::BIC(model[[i]])
}

#number of parameters
qt_parameters <- list()
for (i in 1:length(model)) {
  qt_parameters[[i]] <- length(model[[i]]$coef) + 1 # + error's variance
}

#table of results
spec <- paste0("ARIMA",feasible_set$ar, feasible_set$diff, feasible_set$ma)
size <- rep(length(petr4_returns), length(model))
results <- data.frame(spec, log_likelihood = unlist(log_likelihood),
                             qt_parameters = unlist(qt_parameters),
                             size, AIC = unlist(aicarma), 
                             BIC = unlist(bicarma), stringsAsFactors = FALSE)
print(results)

#chosen arima model
cond_average <- arima(petr4_returns, order = c(0,0,1), method = "ML")
