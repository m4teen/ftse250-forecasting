# FTSE 250 Forecasting Using Econometric and Machine Learning Methods


---


## Project Overview


This project investigates the relationship between key macroeconomic indicators and the FTSE 250 Index using both traditional econometric and modern machine learning methods. It aims to quantify the short-term and long-term effects of the following variables:


- Consumer Price Index (CPI)
- Interest Rate (Bank of England)
- USD/GBP Exchange Rate
- M3 Money Supply


Two modeling approaches are employed:


1. **Autoregressive Distributed Lag (ARDL) with Error Correction Model (ECM)** – implemented in R
2. **Radial Basis Function Neural Network (RBFNN)** – implemented in Python


---


## Objectives


- Assess how selected macroeconomic indicators affect the FTSE 250 over time.
- Evaluate the forecasting ability of RBFNN on financial time-series data with a small sample size.
- Compare econometric and machine learning methods in both short- and long-term forecasting contexts.


---


## Methodologies


### ARDL-ECM (R)
- Used to model short-run and long-run relationships.
- Stationarity assessed using Augmented Dickey-Fuller (ADF) tests.
- Bounds testing used to confirm cointegration.
- ECM derived to model the adjustment to long-term equilibrium.
- Full diagnostic checks (autocorrelation, heteroskedasticity, normality, structural stability).


### RBFNN (Python)
- Inputs: Normalized, differenced macroeconomic variables.
- Output: Predicted change in FTSE 250 index.
- Implemented using `GaussianProcessRegressor` (Bayesian kernel regression).
- GridSearchCV used for hyperparameter tuning.
- Performance evaluated with MAE and R².


---


## Repository Structure


```
ftse250-forecasting/
├── data/
│   └── monthly-data-m3.csv          # Raw macroeconomic and index data
├── r-scripts/
│   ├── ardl_main.R                  # ARDL model with M3 and diagnostics
│   └── ardl_bounds.R                # ARDL variant with UNEM + bounds test
├── src/
│   └── rbfnn_model.py               # Python model (to be added)
├── latex/
│   ├── paper.tex                    # LaTeX source of final report
│   └── paper.pdf                    # Compiled paper output (optional)
├── results/
│   └── diagnostics_plots/           # (Optional) Model residual plots etc.
├── .gitignore
├── requirements.txt                 # Python dependencies (if any)
└── README.md
```


---


## Getting Started


### R Dependencies


```r
install.packages(c(
 "tseries", "vars", "lmtest", "ARDL", "dynlm",
 "dLagM", "strucchange", "zoo", "memochange"
))
```


### Python Dependencies


> Python implementation uses `scikit-learn`, `numpy`, `pandas`, etc.


```bash
pip install -r requirements.txt
```


---


## Data


- Source: Yahoo Finance, Bank of England, ONS, Federal Reserve
- Frequency: Monthly
- Timeframe: October 1992 – March 2024
- Variables: FTSE250 index, CPI, interest rate, exchange rate, money supply (M3)


---


## Key Findings


- Interest rates have a **statistically significant negative impact** on FTSE 250 prices both short- and long-term.
- Exchange rate effects are positive and significant in the long run.
- RBFNN models provide **strong short-term forecasts** (R² = 61%) and outperform ARDL in long-term prediction.
- The model highlights the feasibility of using ML methods on small, macroeconomic time-series datasets.


---


## Paper


You can find the full paper (URSS report) in the [`/latex/`](./latex/) directory:
- [`paper.tex`](./latex/paper.tex) – LaTeX source
- [`paper.pdf`](./latex/paper.pdf) – Final compiled report (optional)


---


## About This Project


This research was conducted as part of the **University of Warwick URSS (Undergraduate Research Support Scheme)** and supervised by the **Warwick Mathematics Institute**.


It combines rigorous econometric modeling with practical implementation of machine learning techniques to assess and forecast financial market behavior.


---


## Contact


For questions, feel free to reach out by opening an issue on this repository.


---


## License


This project is shared under the MIT License. Feel free to use, adapt, or build upon the code with attribution.



