library(tseries)
library(vars)
library(lmtest)
library(ARDL)
library(dynlm)

columns_to_transform <- c("CPI", "INT", "UNEM", "EXCHG", "FTSE.250")
data[columns_to_transform] <- lapply(data[columns_to_transform], function(x) log(x))

adf.test(data$UNEM)
dunem = diff(data$UNEM)
adf.test(dunem)

adf.test(data$INT)
dint = diff(data$INT)
adf.test(dint)

adf.test(data$EXCHG)
dexchg = diff(data$EXCHG)
adf.test(dexchg)


adf.test(data$CPI)
dcpi = diff(data$CPI)
adf.test(dcpi)

max_lags <- 9
results <- list()

for (p in 1:max_lags) {
  model <- ardl(FTSE.250 ~ UNEM + INT + CPI + EXCHG, data = data, order = c(p,p,p,p,p))
  
  aic <- AIC(model)
  sc <- BIC(model)
  
  bg_test <- bgtest(model, order = p)
  
  results[[p]] <- list(
    AIC = aic,
    SC = sc,
    Serial_Correlation_p_value = bg_test$p.value
  )
}

results_df <- do.call(rbind, lapply(results, as.data.frame))
print(results_df)



VARselect(data$FTSE.250)
VARselect(data$CPI)
VARselect(data$UNEM)
VARselect(data$INT)
VARselect(data$EXCHG)

model3 <- ardl(FTSE.250 ~ UNEM + INT + CPI + EXCHG, data = data, order = c(4,7,7,7,7))
summary(model)

test3 <- bounds_f_test(model3, case=3)
test3

model4 <- ardl(FTSE.250 ~ INT + CPI + EXCHG, data = data, order = c(4,7,7,7))
summary(model)

test4 <- bounds_f_test(model4, case=2)
test4

install.packages("dLagM")
library(dLagM)

coefs <- coef(model4)



# LR_coeff = sum(short-run coefficients of the variable) / (1 - sum(autoregressive coefficients))
LR_INT <- sum(coefs[grepl("INT", names(coefs))]) / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))
LR_CPI <- sum(coefs[grepl("CPI", names(coefs))]) / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))
LR_EXCHG <- sum(coefs[grepl("EXCHG", names(coefs))]) / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))

intercept_coefficient <- coefs[1]
long_run_intercept <- intercept_coefficient / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))

# Display long-run coefficients
LR_INT
LR_CPI
LR_EXCHG
long_run_intercept
library(dplyr)

data$CointEq <- with(data, FTSE.250 - 
                       (LR_INT * INT + 
                          LR_CPI * CPI + 
                          LR_EXCHG * EXCHG + 
                          long_run_intercept))


# Estimate the ECM by regressing the first differences on the lagged residuals and first differences
ecm_model <- lm(diff(FTSE.250) ~ diff(INT) + diff(CPI) + diff(EXCHG) + na.omit(lag(CointEq, 1)), data = data)
summary(ecm_model)
print(na.omit(lag(data$CointEq, 1)))

library(strucchange)

residuals <- residuals(model4)
fitted_values <- fitted(model4)
shapiro_test_result <- shapiro.test(residuals)
bp_test_result <- bptest(residuals ~ fitted_values)
ljung_test_result <- Box.test(residuals, lag = 10, type = "Ljung-Box")
print(ljung_test_result)
print(bp_test_result)
print(shapiro_test_result)

#everything checks out!

library(zoo)

recursive_residuals <- rollapply(residuals, width = 1, by = 1, FUN = function(x) x, align = "right", fill = NA)
cusum <- cumsum(residuals)
plot(cusum, type = 'l', col = 'blue', main = 'CUSUM of Recursive Residuals', xlab = 'Time', ylab = 'CUSUM')
abline(h = 0, col = 'red', lty = 2)

efp_result <- efp(residuals ~ 1, type = "OLS-CUSUM")
plot(efp_result)

#everything checks out!
