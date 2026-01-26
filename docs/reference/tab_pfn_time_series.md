# Train a TabPFN Time Series Forecasting Model

Creates a zero-shot time series forecasting model using TabPFN-TS. The
model requires no training and can generate forecasts for both point
predictions and probabilistic forecasts using quantiles.

## Usage

``` r
tab_pfn_time_series(
  train_df,
  prediction_length,
  quantiles = c(0.1, 0.5, 0.9),
  date_col = NULL,
  value_col = NULL,
  item_id_col = NULL,
  tabpfn_mode = "client",
  tabpfn_output_selection = "median",
  features = NULL,
  verbose = TRUE
)
```

## Arguments

- train_df:

  A data frame with time series training data. Must contain: - A
  date/datetime column (automatically detected or specified with
  date_col) - A value column (automatically detected or specified with
  value_col) - Optionally, an item_id column for multiple time series

- prediction_length:

  Integer. Number of steps to forecast ahead

- quantiles:

  Numeric vector of quantiles for probabilistic forecasting (default:
  c(0.1, 0.5, 0.9))

- date_col:

  Character. Name of the date/datetime column (NULL for auto-detection)

- value_col:

  Character. Name of the value column (NULL for auto-detection)

- item_id_col:

  Character. Name of the item_id column for multiple time series (NULL
  for single series)

- tabpfn_mode:

  Character. TabPFN mode: "client" (default) or "local"

- tabpfn_output_selection:

  Character. Output selection: "median" or "mean"

- features:

  List of features to extract. NULL for automatic features. Available
  features: "running_index", "calendar", "seasonal"

- verbose:

  Logical. Print progress information (default: TRUE)

- ...:

  Additional arguments passed to TabPFNTimeSeriesPredictor

## Value

A tab_pfn_time_series model object

## Examples

``` r
if (FALSE) { # \dontrun{
library(rtabpfn)
library(dplyr)
library(lubridate)

# Create sample time series data
dates <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")
values <- sin(seq(0, 2*pi, length.out = length(dates))) * 10 + rnorm(length(dates), 0, 1)
ts_data <- tibble(date = dates, value = values)

# Train forecasting model
model <- tab_pfn_time_series(ts_data, prediction_length = 30)

# Make forecasts
forecasts <- predict(model, ts_data)
} # }
```
