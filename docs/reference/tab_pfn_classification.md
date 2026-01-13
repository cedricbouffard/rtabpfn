# Train a TabPFN classification model

Train a TabPFN classification model

## Usage

``` r
tab_pfn_classification(X, y, device = "auto", test_size = 0.33, ...)
```

## Arguments

- X:

  Predictor data frame or matrix

- y:

  Response vector (factor or character)

- device:

  Device to use: "auto", "cpu", or "cuda"

- test_size:

  Proportion of data to use for internal validation

- ...:

  Additional arguments passed to TabPFNClassifier

## Value

A tab_pfn model object with mode = "classification"
