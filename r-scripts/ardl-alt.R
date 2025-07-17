# Load required packages
library(tseries)      # For ADF test
library(vars)         # For VAR selection criteria
library(lmtest)       # For Breusch-Godfrey test
library(ARDL)         # For ARDL bounds testing
library(dynlm)        # For dynamic linear models (ECM)
library(dLagM)        # For alternative ARDL implementation
library(dplyr)        # For data wrangling
library(strucchange)  # For structural break tests
library(zoo)          # For time series manipulation
library(memochange)   # For CUSUM testing

# Log-transform relevant macroeconomic variables
columns_to_log <- c("CPI", "INT", "UNEM", "EXCHG", "M2", "M3", "FTSE.250")
monthly.data.m3[columns_to_log] <- lapply(../data/monthly.data.m3[columns_to_log], log)

# Perform ADF tests to check stationarity
macro_vars <- c("UNEM", "INT", "EXCHG", "CPI", "M3", "FTSE.250")
for (var in macro_vars) {
  cat("ADF test for", var, "\n")
  print(adf.test(monthly.data.m3[[var]]))
  
  # Test first difference if non-stationary
  diff_var <- diff(monthly.data.m3[[var]])
  cat("ADF test for Δ", var, "\n")
  print(adf.test(diff_var))
}

# Select maximum lag length for ARDL order search
max_lags <- 9
results <- list()

# Grid search to choose ARDL order by AIC, BIC and BG serial correlation test
for (p in 1:max_lags) {
  model <- ardl(FTSE.250 ~ INT + CPI + EXCHG + M3, data = monthly.data.m3, order = c(p, p, p, p, p))
  bg_test <- bgtest(model, order = p)
  
  results[[p]] <- list(
    AIC = AIC(model),
    BIC = BIC(model),
    BG_pvalue = bg_test$p.value
  )
}
# Optional: View results as a data frame
# do.call(rbind, results)

# Select optimal ARDL order manually or based on information criteria
# Run VARselect for information purposes
VARselect(monthly.data.m3$FTSE.250)
VARselect(monthly.data.m3$CPI)
VARselect(monthly.data.m3$UNEM)
VARselect(monthly.data.m3$INT)
VARselect(monthly.data.m3$M3)
VARselect(monthly.data.m3$EXCHG)

# Fit final ARDL model based on optimal order (manually specified here)
model5 <- ardl(FTSE.250 ~ INT + CPI + EXCHG + M3, data = monthly.data.m3, order = c(5, 4, 7, 7, 4))
summary(model5)

# Perform ARDL Bounds Test for cointegration
bounds_test_result <- bounds_f_test(model5, case = 2)
print(bounds_test_result)

# Extract coefficients to calculate long-run multipliers
coefs <- coef(model5)

LR_INT   <- sum(coefs[grepl("INT", names(coefs))]) / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))
LR_CPI   <- sum(coefs[grepl("CPI", names(coefs))]) / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))
LR_EXCHG <- sum(coefs[grepl("EXCHG", names(coefs))]) / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))
LR_M3    <- sum(coefs[grepl("M3", names(coefs))]) / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))
intercept <- coefs[1]
LR_Intercept <- intercept / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))

# Construct cointegration equation and error correction term
monthly.data.m3$CointEq <- with(monthly.data.m3,
  LR_Intercept +
  LR_INT * INT +
  LR_CPI * CPI +
  LR_EXCHG * EXCHG +
  LR_M3 * M3
)

monthly.data.m3$ECT <- monthly.data.m3$FTSE.250 - monthly.data.m3$CointEq

# Estimate the ECM (Error Correction Model)
ecm_model <- dynlm(
  diff(FTSE.250) ~ 
    lag(diff(FTSE.250), 5) +
    lag(diff(CPI), 4) +
    lag(diff(INT), 7) +
    lag(diff(EXCHG), 7) +
    lag(diff(M3), 4) +
    lag(ECT, 1),
  data = monthly.data.m3
)

summary(ecm_model)

# Output long-run multipliers
cat("Long-run multipliers:\n")
cat("INT: ", LR_INT, "\n")
cat("CPI: ", LR_CPI, "\n")
cat("EXCHG: ", LR_EXCHG, "\n")
cat("M3: ", LR_M3, "\n")

# Residual diagnostics
residuals_model <- residuals(model5)
fitted_model <- fitted(model5)

# Shapiro-Wilk test for normality
print(shapiro.test(residuals_model))

# Breusch-Pagan test for heteroskedasticity
print(bptest(residuals_model ~ fitted_model))

# Ljung-Box test for autocorrelation
print(Box.test(residuals_model, lag = 10, type = "Ljung-Box"))

# CUSUM test for structural stability
# Replace 'residuals_vector' with residuals_model or similar
# Adjust parameters as needed

cusum_test(residuals_model, trend = "none", type = "LKT", tau = 0.2, M = 10000)
