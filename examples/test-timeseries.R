library(rtabpfn)
library(dplyr)
library(lubridate)

set.seed(123)

# Create sample time series data
dates <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")
trend <- seq(0, 10, length.out = length(dates))
seasonal <- sin(seq(0, 4*pi, length.out = length(dates))) * 5
noise <- rnorm(length(dates), 0, 2)
values <- trend + seasonal + noise

ts_data <- tibble(
  date = dates,
  value = values
)

cat("Sample time series data:\n")
head(ts_data, 5)

# Train time series forecasting model
model <- tab_pfn_time_series(
  train_df = ts_data,
  prediction_length = 30,
  quantiles = c(0.1, 0.5, 0.9),
  verbose = TRUE
)

# Generate forecasts
forecasts <- predict(model, ts_data)

cat("\nForecast results:\n")
print(forecasts)

# Test with tidymodels
library(tidymodels)

ts_spec <- tab_pfn_ts(mode = "regression") %>%
  set_engine("tabpfn_ts") %>%
  set_args(prediction_length = 30, quantiles = c(0.1, 0.5, 0.9))

# Use fit_tabpfn_ts() to avoid tidymodels S3 dispatch conflicts
ts_fit <- fit_tabpfn_ts(ts_spec, data = ts_data)

cat("\nTidymodels fit:\n")
print(ts_fit)

tidymodels_forecasts <- predict(ts_fit, ts_data)

cat("\nTidymodels forecasts:\n")
print(tidymodels_forecasts)
