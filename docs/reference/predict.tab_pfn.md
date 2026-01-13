# Predict method for TabPFN models

Predict method for TabPFN models

## Usage

``` r
# S3 method for class 'tab_pfn'
predict(
  object,
  new_data,
  type = NULL,
  output_type = "mean",
  quantiles = c(0.1, 0.5, 0.9),
  level = 0.95,
  ...
)
```

## Arguments

- object:

  A fitted TabPFN model object

- new_data:

  A data frame of new predictors

- type:

  Type of prediction. For regression: "numeric" (default), "quantiles",
  "conf_int", or "raw". For classification: "class", "prob", or "raw"

- output_type:

  Python TabPFN output type. Options: "mean" (default), "quantiles",
  "full", "mode"

- quantiles:

  Numeric vector of quantiles to predict (used when output_type =
  "quantiles")

- level:

  Confidence level for prediction intervals (used when type =
  "conf_int")

- ...:

  Additional arguments passed to the Python predict method

## Value

A tibble with predictions
