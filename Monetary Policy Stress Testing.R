library(urca)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(quantmod)
data<-read.csv(file.choose())
head(data)
class(data)
y<-ts(data$'GDP', start=c(1997.1), frequency=12)
pi<-ts(data$'CPI', start=c(1997.1), frequency=12)
r<-ts(data$'Base.Rate', start=c(1997.1), frequency=12)
fx<-ts(data$'UK.US.ER', start=c(1997.1), frequency=12)
y_diff<-diff(y)
pi_diff<-diff(pi)
r_diff<-diff(r)
fx_diff<-diff(fx)
data_diff<-data.frame(GDP=y_diff, CPI=pi_diff, IR=r_diff, ER=fx_diff)
y_diff_ts<-ts(data_diff$'GDP', start=c(1997.2), frequency=12)
pi_diff_ts<-ts(data_diff$'CPI', start=c(1997.2), frequency=12)
r_diff_ts<-ts(data_diff$'IR', start=c(1997.2), frequency=12)
fx_diff_ts<-ts(data_diff$'ER', start=c(1997.2), frequency=12)
model_var_data<-cbind(y_diff_ts,pi_diff_ts, r_diff_ts, fx_diff_ts )
model_var_data
info <- VARselect(model_var_data, lag.max = 12, type = "const")
var1 <- VAR(model_var_data, lag.max = 3, ic = "AIC")
summary(var1)
var1
plot(var1)
residuals <- residuals(var1)
str(residuals)
resid_1 <- residuals[, "y_diff_ts"]
resid_2 <- residuals[, "pi_diff_ts"]
resid_3 <- residuals[, "r_diff_ts"]
resid_4 <- residuals[, "fx_diff_ts"]

# Plot the cross-correlograms
par(mfcol = c(1, 2))
ccf(resid_1, resid_2, lag.max = 4, type = "correlation", plot = TRUE)
ccf(resid_1, resid_3, lag.max = 4, type = "correlation", plot = TRUE)
ccf(resid_1, resid_4, lag.max = 4, type = "correlation", plot = TRUE)
ccf(resid_2, resid_3, lag.max = 4, type = "correlation", plot = TRUE)
ccf(resid_2, resid_4, lag.max = 4, type = "correlation", plot = TRUE)
ccf(resid_3, resid_4, lag.max = 4, type = "correlation", plot = TRUE)
var.pred <- predict(var1, n.ahead=10, ci=0.95)
var.pred
VAR_fevd <- fevd(var1, n.ahead=10)
plot(VAR_fevd)
var.irf <- irf(var1, ci = 0.95)
plot(var.irf)
amat <- diag(4)
model_summary <- summary(var1)
model_summary$covres
t(chol(model_summary$covres))
amat[2,1]<-NA
amat
amat[3,1]<-NA
amat[4,1]<-NA
amat
amat[3,2]<-NA
amat[4,2]<-NA
amat[4,3]<-NA
amat
svar1 <- SVAR(var1, estmethod="direct", Amat=amat)
irf.svar <- irf(svar1)
plot(irf.svar)

