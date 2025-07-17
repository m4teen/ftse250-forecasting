library(ggplot2)

# --- Load Data ---
rbf_predictions_dataframe <- read.csv("../data/rbf-predictions.csv")
long_data <- read.csv("../data/long_data.csv")

# Convert date columns
rbf_predictions_dataframe$daily.date <- as.Date(rbf_predictions_dataframe$daily.date)
long_data$Date <- as.Date(long_data$Date)

# --- Plot 1: Daily Price Changes (Short-Term Model Output) ---
ggplot(rbf_predictions_dataframe, aes(x = daily.date)) +
  geom_line(aes(y = DNFTSE, color = "Real"), linewidth = 1) +
  geom_line(aes(y = daily.predicted.price, color = "Predicted"), linewidth = 0.5) +
  scale_color_manual(values = c("Real" = "green", "Predicted" = "blue")) +
  labs(
    title = "Daily FTSE 250: Real vs Predicted Changes",
    x = "Date",
    y = "Price Change",
    color = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.major = element_line(colour = "grey80"),
    panel.grid.minor = element_line(colour = "grey90")
  )

# --- Plot 2: Monthly Trend (Long-Term Model Output) ---
ggplot(long_data, aes(x = Date)) +
  geom_line(aes(y = Real, color = "Real"), linewidth = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 0.5) +
  scale_color_manual(values = c("Real" = "green", "Predicted" = "blue")) +
  labs(
    title = "Monthly FTSE 250: Real vs Predicted Trend",
    x = "Date",
    y = "Price Change",
    color = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.major = element_line(colour = "grey80"),
    panel.grid.minor = element_line(colour = "grey90")
  )
