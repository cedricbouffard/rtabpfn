# Plot SHAP Summary

Plot SHAP Summary

## Usage

``` r
plot_shap_summary(shap_df, top_n = 10, ...)
```

## Arguments

- shap_df:

  SHAP values tibble from shap_values()

- top_n:

  Number of top features to show (default: 10)

- ...:

  Additional arguments passed to ggplot2

## Value

A ggplot2 object showing feature importance

## Examples

``` r
if (FALSE) { # \dontrun{
library(rtabpfn)
library(ggplot2)

# Train model and get SHAP values
model <- tab_pfn_regression(X, y)
shap_vals <- shap_values(model, X)

# Plot summary
plot_shap_summary(shap_vals)
} # }
```
