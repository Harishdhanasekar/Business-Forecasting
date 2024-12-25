library(tseries)
library(forecast)
library(readxl)

# Read the data
df <- read.csv("C:/Users/haris/Downloads/HDFCBANK.NS.csv")
df

#Descriptive statistics
summary(df)
class(df)

# Convert the 'Adj.Close' column to a time series object
stock1 <- ts(df$Adj.Close, start = c(2019, 8), frequency = 12)

# Remove NA values
stock1_clean <- na.omit(stock1)


# Plot the time series data
plot(stock1_clean, main = "Adjusted Close Time Series", xlab = "Time", ylab = "Adj Close")

#decomposing the data
dec_add <- decompose(stock1_clean, type = "additive")
dec_mult <- decompose(stock1_clean, type = "multiplicative")
plot(dec_add)
plot(dec_mult)

# Perform the ADF test on the cleaned data
adf_test <- adf.test(stock1_clean)
print(adf_test)
# If the p-value is more than 0.05, the data is considered to be not stationary

################################# Simple Moving Averages ################################################
opar <- par(no.readonly = TRUE) # Save the current graphical parameters
par(mfrow = c(2, 2)) # Divide the plot area into a 2x2 layout
ylim <- c(min(stock1_clean), max(stock1_clean)) # Set y-axis limits based on the data range

# Plot the original time series
plot(stock1_clean, main = 'Raw Time Series')

# Plot the Simple Moving Averages with different window sizes
plot(ma(stock1_clean, 3), main = 'Simple Moving Average (K=3)', ylim = ylim)
plot(ma(stock1_clean, 7), main = 'Simple Moving Average (K=6)', ylim = ylim)
plot(ma(stock1_clean, 15), main = 'Simple Moving Average (K=12)', ylim = ylim)


# Forecast for the next 5 periods
S<-ma(stock1_clean,order=3)
S
forecast_S <- forecast(S, 5)
plot(forecast_S, main = 'Forecast using Moving Average (K=3)')
accuracy(forecast_S)

S<-ma(stock1_clean,order=6)
S
forecast_S <- forecast(S, 5)
plot(forecast_S, main = 'Forecast using Moving Average (K=6)')
accuracy(forecast_S)

S<-ma(stock1_clean,order=12)
S
forecast_S <- forecast(S, 5)
plot(forecast_S, main = 'Forecast using Moving Average (K=12)')
accuracy(forecast_S)





####################################################################################################
##### Holt winter exponential smoothing model(triple)
str(stock1_clean)


# Plot the univariate time series
plot(stock1_clean, main = "Univariate Time Series of Adjusted Close Values", ylab = "Value", xlab = "Time")

adf.test(stock1_clean) #define stationary(mean and median are same)


fit1<-ets(stock1_clean,model = "AAA")
fit1

fit2<-ets(stock1_clean,model = "MMM")     
fit2

fit3<-ets(stock1_clean,model = "AAM")
fit3

fit4<-ets(stock1_clean,model = "MMA")
fit4

fit5<-ets(stock1_clean,model = "AMA")
fit5

fit6<-ets(stock1_clean,model = "MAM")
fit6

fit7<-ets(stock1_clean,model = "AMM")
fit7

fit8<-ets(stock1_clean,model = "MAA")
fit8

fit9<-ets(stock1_clean,model = "ANN")
fit9

fit10<-ets(stock1_clean,model = "MNN")
fit10

fit11<-ets(stock1_clean,model = "MAN")
fit11

fit12<-ets(stock1_clean,model = "AMN")
fit12

fit13<-ets(stock1_clean,model = "MNN")
fit13


forecast(fit1,5) 
fit1#according to the data, 5 months forecasting
plot(forecast(fit1,5),xlab="year",ylab = "adj_close",main = "ETS(A,A,A)")
accuracy(fit1)

forecast(fit2,5)
fit2#according to the data, 5 months forecasting
plot(forecast(fit2,5),xlab="year",ylab = "adj_close",main = "ETS(M,M,M)")
accuracy(fit2)

forecast(fit6,5)  #according to the data, 5 months forecasting
fit6
plot(forecast(fit6,5),xlab="year",ylab = "adj_close",main = "ETS(M,A,M)")
accuracy(fit6)


######################################################----ARIMA-----##########################################
# ARIMA Model
adf.test(stock1_clean)
ndiffs(stock1_clean)
par(mfrow=c(1,2))  
acf(stock1_clean, main="ACF of Adjusted Close Time Series")
pacf(stock1_clean, main="PACF of Adjusted Close Time Series")

# Fit ARIMA model
stock_model <- auto.arima(stock1_clean, ic="aic", trace = TRUE)

# Check for stationarity
acf(ts(stock_model$residuals))
pacf(ts(stock_model$residuals))

#Model 1

stock_forecast <- forecast(stock_model, level = c(95), h = 6)
stock_forecast
plot(stock_forecast)
accuracy(stock_forecast)

#Model 2
stock_model2<-arima(stock1_clean,order=c(0,1,1)) 
stock_model2
forecast1 <- forecast(stock_model2, level = c(95), h = 6)
plot(forecast1)
accuracy(forecast1)

#Model 3
stock_model3<-arima(stock1_clean,order=c(0,1,2)) 
stock_model3
forecast2 <- forecast(stock_model3, level = c(95), h = 6)
plot(forecast2)
accuracy(forecast2)

#Model 4
stock_model4<-arima(stock1_clean,order=c(1,1,1)) 
stock_model4
forecast4 <- forecast(stock_model4, level = c(95), h = 6)
plot(forecast4)
accuracy(forecast4)


#Model
# Validate the forecast
Box.test(stock_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(stock_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(stock_forecast$residuals, lag=15, type="Ljung-Box")

