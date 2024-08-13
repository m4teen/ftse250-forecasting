library(tseries)
library(vars)
library(lmtest)
library(ARDL)
library(dynlm)
library(dplyr)
library(dLagM)
library(strucchange)
library(zoo)


columns_to_transform <- c("CPI", "INT", "UNEM", "EXCHG", "M2", "M3", "FTSE.250")
monthly.data.m3[columns_to_transform] <- lapply(monthly.data.m3[columns_to_transform], function(x) log(x))

adf.test(data$UNEM)
dunem <- diff(data$UNEM)
adf.test(dunem)

adf.test(data$INT)
dint <- diff(data$INT)
adf.test(dint)

adf.test(data$EXCHG)
dexchg <- diff(data$EXCHG)
adf.test(dexchg)


adf.test(data$CPI)
dcpi <- diff(data$CPI)
adf.test(dcpi)

adf.test(data$M3)
dm3 <- diff(data$M3)
adf.test(dm3)

adf.test(data$FTSE.250)
dftse <- diff(data$FTSE.250)
adf.test(dftse)

max_lags <- 9
results <- list()

for (p in 1:max_lags) {
  model <- ardl(FTSE.250 ~ INT + CPI + EXCHG + M3, data = monthly.data.m3, order = c(p, p, p, p, p))

  aic <- AIC(model)
  sc <- BIC(model)

  bg_test <- bgtest(model, order = p)

  results[[p]] <- list(
    AIC = aic,
    SC = sc,
    Serial_Correlation_p_value = bg_test$p.value
  )
}


VARselect(data$FTSE.250)
VARselect(data$CPI)
VARselect(data$UNEM)
VARselect(data$INT)
VARselect(monthly.data.m3$M3)
VARselect(data$EXCHG)


model5 <- ardl(FTSE.250 ~ INT + CPI + EXCHG + M3, data = monthly.data.m3, order = c(5, 4, 7, 7, 4))
summary(model5)


test5 <- bounds_f_test(model5, case = 2)
test5


coefs <- coef(model5)


LR_INT <- sum(coefs[grepl("INT", names(coefs))]) / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))
LR_CPI <- sum(coefs[grepl("CPI", names(coefs))]) / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))
LR_EXCHG <- sum(coefs[grepl("EXCHG", names(coefs))]) / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))
LR_M3 <- sum(coefs[grepl("M3", names(coefs))]) / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))

intercept_coefficient <- coefs[1]
long_run_intercept <- intercept_coefficient / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))


monthly.data.m3$CointEq <- with(monthly.data.m3, FTSE.250 -
  (LR_INT * INT +
    LR_CPI * CPI +
    LR_EXCHG * EXCHG + LR_M3 * M3 +
    long_run_intercept))



monthly.data.m3$ECT <- monthly.data.m3$FTSE.250 - monthly.data.m3$CointEq

ecm_model <- dynlm(
  diff(FTSE.250) ~ 
    lag(diff(FTSE.250), 5) + lag(diff(CPI), 4) + lag(diff(INT), 7)  + lag(diff(EXCHG), 7) + lag(diff(M3), 4) + na.omit(lag(ECT, 1)), data = monthly.data.m3)
summary(ecm_model)


LR_INT
LR_CPI
LR_EXCHG
LR_M3

residuals <- residuals(model5)
fitted_values <- fitted(model5)
shapiro_test_result <- shapiro.test(residuals)
bp_test_result <- bptest(residuals ~ fitted_values)
ljung_test_result <- Box.test(residuals, lag = 10, type = "Ljung-Box")
print(ljung_test_result)
print(bp_test_result)
print(shapiro_test_result)



library(memochange)

cusum_test(residuals_vector, trend = c("none", "linear"), tau = 0.2, type = c("LKT","SK"), m = 0, simu = 0, M = 10000)

