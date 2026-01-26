# Make predictions from a fitted TabPFN Time Series model

Make predictions from a fitted TabPFN Time Series model

## Usage

``` r
# S3 method for class 'tab_pfn_ts_fit'
predict(object, new_data, type = NULL, ...)
```

## Arguments

- object:

  A fitted model object

- new_data:

  A data frame of time series data

- type:

  A single character string for the prediction type

- ...:

  Additional arguments

## Value

A tibble of forecasts
