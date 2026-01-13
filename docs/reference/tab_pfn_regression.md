# Train a TabPFN regression model with enhanced predict options

Train a TabPFN regression model with enhanced predict options

## Usage

``` r
tab_pfn_regression(X, y, device = "auto", test_size = 0.33, ...)
```

## Arguments

- X:

  Predictor data frame or matrix

- y:

  Response vector

- device:

  Device to use: "auto", "cpu", or "cuda"

- test_size:

  Proportion of data to use for internal validation

- ...:

  Additional arguments passed to TabPFNRegressor

## Value

A tab_pfn model object with mode = "regression"
