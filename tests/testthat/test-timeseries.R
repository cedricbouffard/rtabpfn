test_that("Time series forecasting works", {
  skip_if_not(check_time_series_available(), 
             "tabpfn-time-series not available")

  # Create sample time series data
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

  # Train model
  model <- expect_silent(
    tab_pfn_time_series(
      train_df = ts_data,
      prediction_length = 10,
      quantiles = c(0.1, 0.5, 0.9),
      verbose = FALSE
    )
  )

  # Check model class
  expect_s3_class(model, "tab_pfn_time_series")

  # Generate forecasts
  forecasts <- expect_silent(
    predict(model, new_data = ts_data)
  )

  # Check forecasts structure
  expect_s3_class(forecasts, "tbl_df")
  expect_true(is_tibble(forecasts))
})

test_that("Time series column detection works", {
  library(dplyr)
  library(lubridate)
  
  # Test with various column names
  df1 <- tibble(
    date = seq(as.Date("2020-01-01"), as.Date("2020-01-10"), by = "day"),
    value = 1:10
  )
  
  df2 <- tibble(
    datetime = seq(as.POSIXct("2020-01-01"), by = "day", length.out = 10),
    target = 1:10
  )
  
  df3 <- tibble(
    time = as.Date("2020-01-01") + 0:9,
    sales = 1:10
  )

  expect_equal(find_date_column(df1), "date")
  expect_equal(find_date_column(df2), "datetime")
  expect_equal(find_date_column(df3), "time")
  
  expect_equal(find_value_column(df1, "date", NULL), "value")
  expect_equal(find_value_column(df2, "datetime", NULL), "target")
  expect_equal(find_value_column(df3, "time", NULL), "sales")
})

test_that("Frequency inference works", {
  library(lubridate)
  
  daily_dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-10"), by = "day")
  weekly_dates <- seq(as.Date("2020-01-01"), as.Date("2020-03-01"), by = "week")
  monthly_dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "month")

  expect_equal(infer_frequency(daily_dates), "day")
  expect_equal(infer_frequency(weekly_dates), "week")
  expect_equal(infer_frequency(monthly_dates), "month")
})

test_that("Multiple time series work", {
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

  # Train model
  model <- expect_silent(
    tab_pfn_time_series(
      train_df = ts_data,
      prediction_length = 10,
      quantiles = c(0.1, 0.5, 0.9),
      item_id_col = "item_id",
      verbose = FALSE
    )
  )

  # Generate forecasts
  forecasts <- expect_silent(
    predict(model, new_data = ts_data)
  )

  # Check that item_id is in forecasts
  expect_true("item_id" %in% names(forecasts))
})

test_that("Tidymodels integration works", {
  skip_if_not(check_time_series_available(), 
             "tabpfn-time-series not available")
  
  library(tidymodels)
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

  # Create model specification
  ts_spec <- tab_pfn_ts(mode = "regression") %>%
    set_engine("tabpfn_ts") %>%
    set_args(prediction_length = 10, quantiles = c(0.1, 0.5, 0.9))

  # Fit using wrapper function to avoid tidymodels S3 dispatch issues
  ts_fit <- expect_silent(
    fit_tabpfn_ts(ts_spec, data = ts_data)
  )

  # Check fit class
  expect_s3_class(ts_fit, "tab_pfn_ts_fit")

  # Generate predictions
  forecasts <- expect_silent(
    predict(ts_fit, ts_data)
  )

  # Check forecasts
  expect_s3_class(forecasts, "tbl_df")
})
