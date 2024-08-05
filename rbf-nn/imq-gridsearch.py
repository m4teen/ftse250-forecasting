import numpy as np
import pandas as pd
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import Kernel, NormalizedKernelMixin
from sklearn.metrics import mean_squared_error, r2_score
from scipy.stats import zscore
from sklearn.model_selection import GridSearchCV, TimeSeriesSplit

import matplotlib.pyplot as plt

# Custom Generalized Inverse Multiquadric Kernel
class GeneralizedInverseMultiquadricKernel(Kernel, NormalizedKernelMixin):
    def __init__(self, length_scale=1.0, sigma=1.0, power=1.0):
        self.length_scale = length_scale
        self.sigma = sigma
        self.power = power

    def __call__(self, X, Y=None):
        X = np.atleast_2d(X) / self.length_scale
        if Y is None:
            Y = X
        else:
            Y = np.atleast_2d(Y) / self.length_scale
        dists = np.sum((X[:, np.newaxis, :] - Y[np.newaxis, :, :]) ** 2, axis=2)
        return (dists + self.sigma ** 2) ** (-self.power)

    def diag(self, X):
        return np.ones(X.shape[0])

    def is_stationary(self):
        return True

# Load and preprocess the data
data = pd.read_csv('monthly-data-m3.csv')
data.set_index('month', inplace=True)
data_normalized = data.apply(zscore)
data_normalized = data_normalized.drop(columns=['M2'])
data_normalized['FTSE-250'] = data_normalized['FTSE-250'].diff()
data_normalized['CPI'] = data_normalized['CPI'].diff()
data_normalized['INT'] = data_normalized['INT'].diff()
data_normalized['EXCHG'] = data_normalized['EXCHG'].diff()
data_normalized['M3'] = data_normalized['M3'].diff()
data_normalized = data_normalized.iloc[1:]

# Define features and target variable
X = data_normalized[['CPI', 'INT', 'EXCHG', 'M3']]
Y = data_normalized['FTSE-250']

# Define the parameter grid for hyperparameter tuning
param_grid = {
    'kernel__length_scale': [0.1, 1.0, 5, 10.0],
    'kernel__sigma': [0.1, 0.5, 1.0, 2, 2.5, 3],
    'kernel__power': [0.1, 0.3, 0.5, 0.7, 1, 2, 5],
    'alpha': [1e-5, 1, 5, 10]
}

# Instantiate the GaussianProcessRegressor with the custom kernel
kernel = GeneralizedInverseMultiquadricKernel()
gpr = GaussianProcessRegressor(kernel=kernel, normalize_y=True)

# Use TimeSeriesSplit for sequential train-test splits
tscv = TimeSeriesSplit(n_splits=len(X) - 1)

# Use GridSearchCV to find the best parameters with LOOCV
search = GridSearchCV(gpr, param_grid, cv=tscv, scoring='r2')
search.fit(X, Y)

# Get the best model and parameters
best_model = search.best_estimator_
best_params = search.best_params_

print(f'Best Parameters: {best_params}')

# Make predictions on the entire dataset using the best model
Y_pred = best_model.predict(X)

# Evaluate the model on the entire dataset
mse = mean_squared_error(Y, Y_pred)
r2 = r2_score(Y, Y_pred)
print(f'MSE: {mse}')
print(f'R²: {r2}')