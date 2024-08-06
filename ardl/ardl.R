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

adf.test(monthly.data.m3$M3)
dm3 <- diff(monthly.data.m3$M3)
adf.test(dm3)

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


ecm_model <- lm(diff(FTSE.250) ~ diff(INT) + diff(CPI) + diff(EXCHG) + diff(M3) + na.omit(lag(CointEq, 1)), data = monthly.data.m3)
summary(ecm_model)


residuals <- residuals(model5)
fitted_values <- fitted(model5)
shapiro_test_result <- shapiro.test(residuals)
bp_test_result <- bptest(residuals ~ fitted_values)
ljung_test_result <- Box.test(residuals, lag = 10, type = "Ljung-Box")
print(ljung_test_result)
print(bp_test_result)
print(shapiro_test_result)



recursive_residuals <- rollapply(residuals, width = 1, by = 1, FUN = function(x) x, align = "right", fill = NA)
cusum <- cumsum(residuals)
plot(cusum, type = "l", col = "blue", main = "CUSUM of Recursive Residuals", xlab = "Time", ylab = "CUSUM")
abline(h = 0, col = "red", lty = 2)

efp_result <- efp(residuals ~ 1, type = "OLS-CUSUM")
plot(efp_result)
