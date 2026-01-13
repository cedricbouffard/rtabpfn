# Train a TabPFN Unsupervised Anomaly Detection Model

Creates an unsupervised model for detecting anomalies using TabPFN's
joint probability estimation. The model uses both classifier and
regressor models to estimate the likelihood of samples under the learned
distribution.

## Usage

``` r
tab_pfn_unsupervised(
  X,
  n_estimators = 4,
  device = "auto",
  categorical_features = NULL,
  ...
)
```

## Arguments

- X:

  Predictor data frame or matrix

- n_estimators:

  Number of TabPFN models to use (default: 4)

- device:

  Device to use: "auto", "cpu", or "cuda"

- categorical_features:

  Vector of column indices or names to treat as categorical (NULL for
  auto-detection)

- ...:

  Additional arguments passed to TabPFNClassifier and TabPFNRegressor

## Value

A tab_pfn_unsupervised model object

## Examples

``` r
if (FALSE) { # \dontrun{
library(rtabpfn)

# Prepare data
X <- mtcars[, c("cyl", "disp", "hp", "wt")]

# Train unsupervised model
model <- tab_pfn_unsupervised(X, n_estimators = 4)

# Detect anomalies
scores <- anomaly_scores(model, X, n_permutations = 10)
} # }
```
