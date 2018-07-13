library(timeSeries)
library(forecast)
library(tidyquant)
library(tidyverse)
library(quantmod)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ggplot2)

# Get stock data 
aapl <- tq_get(c("AAPL"),
                 get = "stock.prices",
                 from = "1998-01-01",
                 to = "2018-01-01")

# Calculate monthly closing values using adjusted closing figures 
aapl_returns <- aapl %>% 
  group_by("AAPL") %>%
  tq_transmute(select = adjusted, 
               mutate_fun = to.period, 
               period = "months")

# Create a time series object
aapl <- ts(aapl_returns$adjusted, start=c(1998, 1), freq=12)

# Split data in two training set 
aapl_train <- ts(aapl, start=c(1998, 1), end=c(2016, 12), freq=12)

# Box Test
Box.test(aapl, lag = 20, type = 'Ljung-Box')

# ADF Test 
adf.test(aapl)

# Time Series Decommposition
decompose_aapl = decompose(aapl_train, "additive")

plot(as.ts(decompose_aapl$seasonal))
plot(as.ts(decompose_aapl$trend))
plot(as.ts(decompose_aapl$random))
plot(decompose_aapl)

# First Difference 
tsDiff <- Diff(aapl_train)

# Fit Model 
fit <- Arima(aapl_train, order = c(0,1,1),
             include.drift = TRUE)
summary(fit)

for_sp500_all <- forecast(fit, h = 12)
plot(for_sp500_all)
