#Loading the File

df <- ESE_BF_DATA

#Loading the Relevant libraries
library("datasets")
library("forecast")
library("graphics")
library("stats")
library("tseries")
library("lubridate")
library("ggplot2")

#Dropping columns and converting from Dataframe to Time Series
View(df)
class(df)
summary(df)
str(df)

# Check if Date is in Date format
df$Date <- as.Date(df$Date)

# Extract start and end years and months
start_year <- as.numeric(format(min(df$Date), "%Y"))
start_month <- as.numeric(format(min(df$Date), "%m"))
end_year <- as.numeric(format(max(df$Date), "%Y"))
end_month <- as.numeric(format(max(df$Date), "%m"))

# Create the time series object
products <- ts(df$Manufactured, start = c(start_year, start_month), end = c(end_year, end_month), frequency = 12)

# Plot the time series
plot(products, main = "Manufactured products Time Series", xlab = "Time", ylab = "Manufactured Products")

#decomposing the data
dec_add <- decompose(products, type = "additive")
dec_mult <- decompose(products, type = "multiplicative")
plot(dec_add)
plot(dec_mult)


##################################################################################################
# ARIMAX in R
# Load necessary libraries
library(forecast)
library(readxl)
library(tseries)

# Load the dataset
data <- ESE_BF_DATA

# Convert Date to time series format
data_ts <- ts(data$Manufactured, start = c(2017, 11), frequency = 12)

# Prepare the exogenous variables
exog_vars <- cbind(data$Inflation_Rate,
                   data$Interest_Rate)

# Step 1: Test the stationarity of ProductsProducts
kpss_test <- kpss.test(data_ts)
summary(kpss_test)



# Step 2: Fit ARIMAX model using exogenous variables
fit <- auto.arima(data_ts, xreg = exog_vars)

# Print the model summary
summary(fit)

# Step 3: Check residuals of the fitted model
checkresiduals(fit)

# Step 4: Generate future values for all exogenous variables
# Use the mean of each exogenous variable to create future values for the next 12 periods
# Create the future_exog matrix with the desired columns
future_exog <- cbind(
  rep(mean(data$Interest_Rate, na.rm = TRUE), 12),
  rep(mean(data$Inflation_Rate, na.rm = TRUE), 12)
)

# Assign column names to the resulting matrix
colnames(future_exog) <- c("Interest_Rates", "Inflation_Rate")

# Convert future_exog to a numeric matrix
future_exog_matrix <- as.matrix(future_exog)

# Step 5: Forecast the model using the numeric matrix for exogenous variables
fcast <- forecast(fit, xreg = future_exog_matrix, h = 12)

# Plot the forecast
autoplot(fcast) + 
  xlab("Year") +
  ylab("Forecasted Manufactured Products")

# Print forecasted values
print(fcast)

###################################################################################################

#ARIMA in R

library(fpp2)
library(tseries)
library(forecast)
library(urca)

# Load the data from your file
data <- ESE_BF_DATA

# Convert the Date column to Date class and ProductsProducts to time series
data$Date <- as.Date(data$Date, format="%Y-%m-%d")
Products_ts <- ts(data$Manufactured, start = c(2017, 11), frequency = 12)

# 8.1 Stationarity and differencing

# ACF plot for identifying non-stationary series
ggAcf(Products_ts)

# Differencing the series and plotting ACF again
ggAcf(diff(Products_ts))

# Ljung-Box test for white noise
Box.test(Products_ts, lag = 10, type = "Ljung-Box")
Box.test(diff(Products_ts), lag = 10, type = "Ljung-Box")

# Augmented Dickey-Fuller (ADF) test
adf.test(Products_ts)
adf.test(diff(Products_ts))

# KPSS test for stationarity
summary(ur.kpss(Products_ts))
summary(ur.kpss((Products_ts)))

# Find the number of differences needed to make the series stationary
ndiffs(Products_ts)

# Non-seasonal ARIMA model
fit <- auto.arima(Products_ts, seasonal = FALSE)
forecast(fit, h = 10) %>% autoplot()

# ACF and PACF for model selection
ggAcf(Products_ts)
ggPacf(Products_ts)

# Build ARIMA(3,1,0) as an example
fit_arima <- Arima(Products_ts, order=c(3,1,0))
summary(fit_arima)
forecast(fit_arima, h = 10) %>% autoplot()

# Checking residuals
checkresiduals(fit_arima)


###############################################################################################
library(fpp2)
library(tseries)
library(forecast)
library(urca)

# Load the data from your file (Update path as necessary)
data <- ESE_BF_DATA

# Convert the Date column to Date class and ProductsProducts to time series
data$Date <- as.Date(data$Date, format="%Y-%m-%d")
Products_ts <- ts(data$Manufactured, start = c(2017, 11), frequency = 12)  # Monthly frequency

# 1. Plotting the original time series to inspect seasonality
autoplot(Products_ts) + xlab("Year") + ylab("Products Manf Index")

# 2. Check how many differences are required to make the series stationary
ndiffs(Products_ts)    # For non-seasonal differencing
nsdiffs(Products_ts)   # For seasonal differencing

# 3. Apply seasonal and first differencing if necessary
# Start with seasonal differencing first (monthly data with seasonality of 12)
Products_diff <- diff(Products_ts, lag = 12)  # Seasonal difference
Products_diff2 <- diff(Products_diff)         # First difference after seasonal difference

# 4. Plot ACF and PACF for the differenced series
ggtsdisplay(Products_diff2, main = "Differenced Products Manf Series (Seasonal + First Difference)")

# 5. Fit a seasonal ARIMA model (SARIMA)
# Start by estimating a basic SARIMA model based on ACF and PACF plots
# SARIMA(p,d,q)(P,D,Q)[s] where 's' is the seasonal period (12 for monthly data)

fit_sarima <- Arima(Products_ts, order = c(0, 1, 1), seasonal = c(0, 1, 1))  # Example: SARIMA(0,1,1)(0,1,1)[12]
summary(fit_sarima)

# 6. Checking the residuals to validate the model
checkresiduals(fit_sarima)

# 7. Forecast using the fitted SARIMA model
forecast_sarima <- forecast(fit_sarima, h = 12)  # Forecast for next 12 periods (1 year)
autoplot(forecast_sarima)

# 8. Fine-tuning the SARIMA model
# If the model doesn't perform well, try changing the order and seasonal components.
# Example: Trying different combinations of p, d, q and P, D, Q
fit_sarima_optimized <- auto.arima(Products_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(fit_sarima_optimized)

# 9. Plot the forecast from the optimized model
forecast(fit_sarima_optimized, h = 12) %>% autoplot()

####################################################################################################################

library(fpp2)
library(tseries)
library(forecast)
library(urca)

# Load the data from your file (Update path as necessary)
data <- ESE_BF_DATA

# Convert the Date column to Date class and ProductsProducts to time series
data$Date <- as.Date(data$Date, format="%Y-%m-%d")
Products_ts <- ts(data$Manufactured, start = c(2017, 11), frequency = 12)  # Monthly frequency

# Define the exogenous variables (e.g., Government Spending, Interest Rates, GDP Growth)
# Make sure the exogenous variables are time series with the same frequency as ProductsProducts_ts
exog_vars <- cbind(data$Inflation_Rate, data$Interest_Rate)

# 1. Plot the original time series to inspect seasonality
autoplot(Products_ts) + xlab("Year") + ylab("Products Manf Index")

# 2. Check for the number of differences required for stationarity
ndiffs(Products_ts)    # For non-seasonal differencing
nsdiffs(Products_ts)   # For seasonal differencing

# 3. Apply seasonal and first differencing if necessary
# Start with seasonal differencing first (monthly data with seasonality of 12)
Products_diff <- diff(Products_ts, lag = 12)  # Seasonal difference
Products_diff2 <- diff(Products_diff)         # First difference after seasonal difference

# 4. Fit a SARIMAX model with exogenous variables
# SARIMAX(p,d,q)(P,D,Q)[s] with external regressors
fit_sarimax <- Arima(Products_ts, 
                     order = c(0, 1, 1),      # Non-seasonal (p,d,q)
                     seasonal = c(0, 1, 1),   # Seasonal (P,D,Q) with 12-month seasonality
                     xreg = exog_vars)        # Exogenous variables

# 5. Summary of the SARIMAX model
summary(fit_sarimax)

# 6. Checking the residuals to validate the model
checkresiduals(fit_sarimax)

# 7. Forecast using the fitted SARIMAX model
# Use the same exogenous variables for future periods in forecasting
future_exog <- cbind(data$Interest_Rate, data$Inflation_Rate)  # Extend exogenous variables for forecast horizon
forecast_sarimax <- forecast(fit_sarimax, xreg = future_exog, h = 4)  # Forecast 12 periods ahead
autoplot(forecast_sarimax)

# 8. Optimize the SARIMAX model using auto.arima with exogenous variables
fit_sarimax_auto <- auto.arima(Products_ts, 
                               seasonal = TRUE, 
                               xreg = exog_vars,   # Include the exogenous variables
                               stepwise = FALSE, 
                               approximation = FALSE)

# 9. Summary of the optimized SARIMAX model
summary(fit_sarimax_auto)

# 10. Forecast from the optimized SARIMAX model
forecast(fit_sarimax_auto, xreg = future_exog, h = 4) %>% autoplot()

######################################################################################################################

#ARCH MODEL
# Load necessary libraries
library(tseries)
library(fGarch)
library(forecast)

# Load your data
data <- ESE_BF_DATA

# Convert the 'Date' column to Date format
data$`Date` <- as.Date(data$`Date`, format="%Y-%m-%d")

# Prepare the time series data (assuming we're modeling the CPI of Capital Goods)
cap_ts <- ts(data$'Manufactured', frequency = 12, start = c(2017, 11))

# Fit an ARCH model with a higher order, for example, ARCH(2)
arch_model <- garch(cap_ts, order = c(0, 2))  # ARCH(2) model
summary(arch_model)

# Check the class of the fitted model
class(arch_model)

# Extract the conditional standard deviation (volatility) from the fitted model
arch_volatility <- fitted(arch_model, type = "sigma")
plot(arch_volatility)

# Plot the volatility (conditional standard deviation) of the ARCH model
plot(arch_volatility, type="l", col="red", lwd=2, main="Volatility (ARCH Model)",
     xlab="Time", ylab="Volatility")

# Forecast the next 6 months using the ARCH model
arch_forecast <- predict(arch_model, n.ahead = 10)
arch_forecast

# Since `arch_forecast` is an atomic vector, you can directly assign it as the forecasted mean
arch_forecasted_mean <- as.numeric(arch_forecast)
arch_forecasted_mean

# Create a time series object for the forecasted values, starting right after the last observation
forecast_start <- end(cap_ts) + c(0, 1)
arch_forecasted_ts <- ts(arch_forecasted_mean, start = forecast_start, frequency = 12)
arch_forecasted_ts
# Combine the original data with the forecast for plotting
full_series <- ts(c(cap_ts, rep(NA, length(arch_forecasted_mean))), frequency = 12, start = start(cap_ts))

# Plot the original time series
plot(full_series, type="l", col="black", lwd=2, main="ARCH Model Forecast",
     xlab="Months", ylab="Manufactured", xlim=c(start(cap_ts)[1], forecast_start[1] + 3))

# Add the forecasted values to the plot
lines(arch_forecasted_ts, col="red", lwd=2, lty=2)
arch_forecasted_ts

# Calculate AIC and BIC for the ARCH model
aic_value <- AIC(arch_model)
aic_value
bic_value <- BIC(arch_model)
bic_value

cat("AIC:", aic_value, "\n")
cat("BIC:", bic_value, "\n")

# Calculate fitted values from the model
fitted_values <- fitted(arch_model)

# Calculate RMSE
rmse_value <- sqrt(mean((cap_ts - fitted_values)^2, na.rm = TRUE))
cat("RMSE:", rmse_value, "\n")

# Calculate MAE
mae_value <- mean(abs(cap_ts - fitted_values), na.rm = TRUE)
cat("MAE:", mae_value, "\n")

######################################################################################################################

#GARCH MODEL
# Load necessary libraries
library(xts)
library(rugarch)
library(ggplot2)

# Load the data
data <- ESE_BF_DATA

# Convert the date to Date format
data$Date <- as.Date(data$`Date`, format = "%Y-%m-%d")

# Log-transform the Gold Price
data$Log_Capital_Goods_Price_Index <- log(data$`Manufactured`)
data$Log_Capital_Goods_Price_Index

# Convert to time series object
capital_good_ts <- xts(data$Log_Capital_Goods_Price_Index, order.by = data$Date)

# Fit various GARCH models and calculate AIC
spec_garch11 <- ugarchspec(variance.model = list(model = "sGARCH"), mean.model = list(armaOrder = c(1,1)))
fit_garch11 <- ugarchfit(spec = spec_garch11, data = capital_good_ts)
aic_garch11 <- infocriteria(fit_garch11)[1]
aic_garch11

spec_garch21 <- ugarchspec(variance.model = list(model = "sGARCH"), mean.model = list(armaOrder = c(2,1)))
fit_garch21 <- ugarchfit(spec = spec_garch21, data = capital_good_ts)
aic_garch21 <- infocriteria(fit_garch21)[1]
aic_garch21

spec_garch22 <- ugarchspec(variance.model = list(model = "sGARCH"), mean.model = list(armaOrder = c(2,2)))
fit_garch22 <- ugarchfit(spec = spec_garch22, data = capital_good_ts)
aic_garch22 <- infocriteria(fit_garch22)[1]
aic_garch22

#spec_egarch <- ugarchspec(variance.model = list(model = "eGARCH"), mean.model = list(armaOrder = c(1,1)))
#fit_egarch <- ugarchfit(spec = spec_egarch, data = capital_good_ts)
#aic_egarch <- infocriteria(fit_egarch)[1]
#aic_egarch

spec_tgarch <- ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH"), mean.model = list(armaOrder = c(1,1)))
fit_tgarch <- ugarchfit(spec = spec_tgarch, data = capital_good_ts)
aic_tgarch <- infocriteria(fit_tgarch)[1]
aic_tgarch

# Combine AIC values for all models
aic_values <- c("GARCH(1,1)" = aic_garch11,
                "GARCH(2,1)" = aic_garch21,
                "GARCH(2,2)" = aic_garch22)
# Print the AIC values for each model
print(aic_values)

# Select the best model based on the lowest AIC
best_model <- names(aic_values)[which.min(aic_values)]
best_model
best_aic <- min(aic_values)
best_aic

# Retrieve the corresponding fitted model
if (best_model == "GARCH(1,1)") {
  best_model_fit <- fit_garch11
} else if (best_model == "GARCH(2,1)") {
  best_model_fit <- fit_garch21
} else if (best_model == "GARCH(2,2)") {
  best_model_fit <- fit_garch22
} else if (best_model == "TGARCH") {
  best_model_fit <- fit_tgarch
}


# Predict the next 6 months
forecast <- ugarchforecast(best_model_fit, n.ahead = 6)
predicted_values <- exp(fitted(forecast))
predicted_values

# Combine original and forecasted values
predicted_values_xts <- xts(predicted_values, order.by = seq(tail(index(capital_good_ts), 1), length.out = 6, by = "months"))
combined_data <- merge(capital_good_ts, predicted_values_xts, all = TRUE)
colnames(combined_data) <- c("Original", "Forecasted")

# Convert back to the original scale (if log-transformed)
combined_data$Original <- exp(combined_data$Original)

# Plot the original and forecasted values
ggplot(data = fortify(combined_data), aes(x = Index)) +
  geom_line(aes(y = Original, color = "Original Data"), size = 1) +
  geom_line(aes(y = Forecasted, color = "Forecasted Data"), size = 1, linetype = "dashed") +
  labs(title = paste("Manufactured vs Forecasted (", best_model, ")"),
       x = "Date", y = "Capital_Good_CPI") +
  scale_color_manual(values = c("Original Data" = "blue", "Forecasted Data" = "red")) +
  theme_minimal()

# Display the best model and its AIC value
cat("The best model is:", best_model, "with an AIC value of:", best_aic, "\n")

library(Metrics)
# Extract the fitted values from the best model
fitted_values <- fitted(best_model_fit)

# Calculate RMSE and MAE for the fitted values over the entire dataset
# Ensure both fitted and actual values are in the same scale (original or log-transformed)
actual_values <- capital_good_ts  # Original log-transformed values

# Calculate RMSE
rmse_model <- rmse(actual_values, fitted_values)

# Calculate MAE
mae_model <- mae(actual_values, fitted_values)

# Print the RMSE and MAE values
cat("RMSE of the model:", rmse_model, "\n")
cat("MAE of the model:", mae_model, "\n")






