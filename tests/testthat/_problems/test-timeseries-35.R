# Extracted from test-timeseries.R:35

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rtabpfn", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not(check_time_series_available(), 
             "tabpfn-time-series not available")
library(dplyr)
library(lubridate)
set.seed(42)
dates <- seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day")
values <- sin(seq(0, 2*pi, length.out = length(dates))) * 10 + 
             rnorm(length(dates), 0, 1)
ts_data <- tibble(
    date = dates,
    value = values
  )
model <- expect_silent(
    tab_pfn_time_series(
      train_df = ts_data,
      prediction_length = 10,
      quantiles = c(0.1, 0.5, 0.9),
      verbose = FALSE
    )
  )
expect_s3_class(model, "tab_pfn_time_series")
forecasts <- expect_silent(
    predict(model, new_data = ts_data)
  )
