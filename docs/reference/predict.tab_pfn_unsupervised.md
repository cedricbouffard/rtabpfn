# Predict method for TabPFN Unsupervised models

Predict method for TabPFN Unsupervised models

## Usage

``` r
# S3 method for class 'tab_pfn_unsupervised'
predict(object, new_data, n_permutations = 10, threshold = NULL, ...)
```

## Arguments

- object:

  A fitted tab_pfn_unsupervised model object

- new_data:

  A data frame of observations

- n_permutations:

  Number of permutations for anomaly scoring (default: 10)

- threshold:

  Optional threshold for binary classification as anomaly

- ...:

  Additional arguments passed to anomaly_scores

## Value

A tibble with anomaly scores and optional binary predictions
