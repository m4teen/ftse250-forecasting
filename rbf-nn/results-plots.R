library(ggplot2)
updated_dataframe$daily.date <- as.Date(updated_dataframe$daily.date)

ggplot(updated_dataframe, aes(x = daily.date)) +
  geom_line(aes(y = DNFTSE, color = "Real"), linewidth = 1) +    # Plot DNFTSE first
  geom_line(aes(y = daily.predicted.price, color = "Predicted"), linewidth = 0.5) +    # Plot Predicted second (on top)
  scale_color_manual(values = c("Predicted" = "blue", "Real" = "green")) +
  labs(title = "",
       x = "",
       y = "Price changes",
       color = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  )

long_data$Date <- as.Date(long_data$Date)

# Create the plot
ggplot(long_data, aes(x = Date)) +
  geom_line(aes(y = Real, color = "Real"), linewidth = 1) +    # Plot Real first
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 0.5) +    # Plot Predicted second (on top)
  scale_color_manual(values = c("Predicted" = "blue", "Real" = "green")) +
  labs(title = "",
       x = "Date",
       y = "Price Changes",
       color = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  )
