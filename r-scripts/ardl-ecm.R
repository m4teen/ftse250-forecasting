# Load required libraries
library(tseries)       # ADF test
library(vars)          # VAR selection
library(lmtest)        # Breusch-Godfrey and Breusch-Pagan tests
library(ARDL)          # ARDL estimation and bounds testing
library(dynlm)         # Dynamic linear models
library(dLagM)         # Lag models
library(dplyr)         # Data wrangling
library(strucchange)   # Structural stability
library(zoo)           # Rolling residuals
library(here)          # Robust file path handling

# Load data from parent directory's data folder
data <- read.csv(here("..", "data", "monthly_data.csv"))

# Log-transform selected macroeconomic variables
columns_to_log <- c("CPI", "INT", "UNEM", "EXCHG", "FTSE.250")
data[columns_to_log] <- lapply(data[columns_to_log], log)

# Perform ADF unit root tests for level and first differences
for (var in columns_to_log) {
  cat("ADF test for:", var, "\n")
  print(adf.test(data[[var]]))
  
  diff_var <- diff(data[[var]])
  cat("ADF test for Δ", var, "\n")
  print(adf.test(diff_var))
}

# ARDL model lag order selection (grid search up to max_lags)
max_lags <- 9
results <- list()

for (p in 1:max_lags) {
  model <- ardl(FTSE.250 ~ UNEM + INT + CPI + EXCHG, data = data, order = c(p, p, p, p, p))
  
  results[[p]] <- list(
    AIC = AIC(model),
    BIC = BIC(model),
    BG_pval = bgtest(model, order = p)$p.value
  )
}

results_df <- do.call(rbind, lapply(results, as.data.frame))
print(results_df)

# VAR selection (information only)
vars_to_check <- c("FTSE.250", "CPI", "UNEM", "INT", "EXCHG")

# Loop through variables and apply VARselect individually
for (var_name in vars_to_check) {
  cat("\n===== VARselect for", var_name, "=====\n")
  var_data <- data[[var_name]]
  
  # VARselect expects a multivariate time series; we convert to a ts object
  var_ts <- ts(var_data, frequency = 12)  # adjust frequency if needed
  
  result <- VARselect(var_ts)
  print(result$selection)
}

# Final ARDL models with selected orders
model3 <- ardl(FTSE.250 ~ UNEM + INT + CPI + EXCHG, data = data, order = c(4, 7, 7, 7, 7))
summary(model3)
test3 <- bounds_f_test(model3, case = 3)
print(test3)

model4 <- ardl(FTSE.250 ~ INT + CPI + EXCHG, data = data, order = c(4, 7, 7, 7))
summary(model4)
test4 <- bounds_f_test(model4, case = 2)
print(test4)

# Long-run multipliers from model4
coefs <- coef(model4)
LR_INT   <- sum(coefs[grepl("INT", names(coefs))]) / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))
LR_CPI   <- sum(coefs[grepl("CPI", names(coefs))]) / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))
LR_EXCHG <- sum(coefs[grepl("EXCHG", names(coefs))]) / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))
LR_intercept <- coefs[1] / (1 - sum(coefs[grepl("FTSE.250", names(coefs))]))

# Display long-run coefficients
cat("Long-run coefficients:\n")
cat("INT:", LR_INT, "\n")
cat("CPI:", LR_CPI, "\n")
cat("EXCHG:", LR_EXCHG, "\n")
cat("Intercept:", LR_intercept, "\n")

# Construct cointegration equation and ECT
data$CointEq <- with(data, FTSE.250 - (LR_INT * INT + LR_CPI * CPI + LR_EXCHG * EXCHG + LR_intercept))

# Estimate ECM model using first differences and lagged cointegration term
ecm_model <- lm(
  diff(FTSE.250) ~ diff(INT) + diff(CPI) + diff(EXCHG) + lag(CointEq, 1),
  data = data
)
summary(ecm_model)

# Diagnostic tests
res <- residuals(model4)
fit <- fitted(model4)

cat("Shapiro-Wilk test (normality):\n")
print(shapiro.test(res))

cat("Breusch-Pagan test (heteroskedasticity):\n")
print(bptest(res ~ fit))

cat("Ljung-Box test (autocorrelation):\n")
print(Box.test(res, lag = 10, type = "Ljung-Box"))

# Structural stability tests (CUSUM)
recursive_residuals <- rollapply(res, width = 1, FUN = identity, align = "right", fill = NA)
cusum <- cumsum(res)
plot(cusum, type = 'l', col = 'blue', main = 'CUSUM of Recursive Residuals', xlab = 'Time', ylab = 'CUSUM')
abline(h = 0, col = 'red', lty = 2)

efp_result <- efp(res ~ 1, type = "OLS-CUSUM")
plot(efp_result)
