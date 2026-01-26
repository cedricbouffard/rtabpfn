# Extracted from test-timeseries.R:119

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
ts_data <- tibble(
    item_id = rep(c("A", "B"), each = length(dates)),
    date = rep(dates, 2),
    value = c(
      sin(seq(0, 2*pi, length.out = length(dates))) * 10,
      sin(seq(0, 2*pi, length.out = length(dates))) * 5
    ) + rnorm(length(dates) * 2, 0, 1)
  )
model <- expect_silent(
    tab_pfn_time_series(
      train_df = ts_data,
      prediction_length = 10,
      quantiles = c(0.1, 0.5, 0.9),
      item_id_col = "item_id",
      verbose = FALSE
    )
  )
forecasts <- expect_silent(
    predict(model, new_data = ts_data)
  )
expect_true("item_id" %in% names(forecasts))
