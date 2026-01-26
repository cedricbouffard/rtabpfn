# Predict method for TabPFN Time Series models

Generates forecasts for time series using the fitted TabPFN-TS model.
Returns point forecasts and probabilistic quantile forecasts.

## Usage

``` r
# S3 method for class 'tab_pfn_time_series'
predict(
  object,
  new_data,
  prediction_length = NULL,
  quantiles = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- object:

  A fitted tab_pfn_time_series model object

- new_data:

  A data frame with time series data (typically the training data)

- prediction_length:

  Integer. Number of steps to forecast (uses model default if NULL)

- quantiles:

  Numeric vector of quantiles for probabilistic forecasting

- verbose:

  Logical. Print progress information

- ...:

  Additional arguments passed to the predict method

## Value

A tibble with forecasts containing: - item_id (if multiple series) -
timestamp - point forecast (mean/median based on model configuration) -
Quantile forecasts (e.g., .pred_q100, .pred_q500, .pred_q900)

## Examples

``` r
if (FALSE) { # \dontrun{
library(rtabpfn)
library(dplyr)
library(lubridate)

# Train model
dates <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")
values <- sin(seq(0, 2*pi, length.out = length(dates))) * 10 + rnorm(length(dates), 0, 1)
ts_data <- tibble(date = dates, value = values)
model <- tab_pfn_time_series(ts_data, prediction_length = 30)

# Generate forecasts
forecasts <- predict(model, ts_data)
} # }
```
