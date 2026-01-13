# Calculate Anomaly/Outlier Scores

Computes anomaly scores for observations using the fitted unsupervised
model. Lower scores indicate more anomalous observations (lower joint
probability).

## Usage

``` r
anomaly_scores(object, new_data, n_permutations = 10, verbose = TRUE, ...)
```

## Arguments

- object:

  A fitted tab_pfn_unsupervised model object

- new_data:

  A data frame of observations to score

- n_permutations:

  Number of random feature orderings to average over (default: 10)

- verbose:

  Print progress information

- ...:

  Additional arguments passed to outliers method

## Value

A tibble with anomaly scores for each observation

## Examples

``` r
if (FALSE) { # \dontrun{
library(rtabpfn)

# Train model
X <- mtcars[, c("cyl", "disp", "hp", "wt")]
model <- tab_pfn_unsupervised(X)

# Calculate anomaly scores
scores <- anomaly_scores(model, X, n_permutations = 10)

# Get most anomalous observations (lowest scores)
most_anomalous <- scores |> dplyr::arrange(anomaly_score) |> head(5)
} # }
```
