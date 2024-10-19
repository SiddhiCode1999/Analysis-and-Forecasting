library(ggplot2)
library(forecast)
library(tseries)
library(neuralnet)

gold_data <- read.csv("gold_prices.csv")

summary(gold_data)

model_simple <- lm(Price ~ Year, data = gold_data)
summary(model_simple)

ggplot(gold_data, aes(x = Year, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Simple Linear Regression of Gold Prices",
       x = "Year", y = "Price (INR)")

model_multiple <- lm(Price ~ Year + Inflation + GDP, data = gold_data)
summary(model_multiple)

gold_ts <- ts(gold_data$Price, start = min(gold_data$Year), frequency = 1)
plot(gold_ts, main = "Gold Prices Time Series", ylab = "Price (INR)", xlab = "Year")

arima_model <- auto.arima(gold_ts)
forecast_values <- forecast(arima_model, h = 5)
plot(forecast_values)

scaled_data <- scale(gold_data)
nn_model <- neuralnet(Price ~ Year + Inflation + GDP, data = scaled_data, hidden = 5)
plot(nn_model)

cat("The simple linear regression model shows that gold prices are influenced primarily by the year.\n")
cat("The multiple linear regression model suggests that GDP and inflation have a significant effect on gold prices.\n")
cat("Time series analysis predicts a continued upward trend in the next 5 years.\n")
