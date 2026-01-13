# Plot SHAP Dependence Plot

Plot SHAP Dependence Plot

## Usage

``` r
plot_shap_dependence(shap_df, feature_data, feature, color_feature = NULL, ...)
```

## Arguments

- shap_df:

  SHAP values tibble from shap_values()

- feature_data:

  Original feature data (data frame with the feature values)

- feature:

  Feature name to plot

- color_feature:

  Optional feature name to use for coloring points

- ...:

  Additional arguments passed to ggplot2

## Value

A ggplot2 object showing SHAP dependence

## Examples

``` r
if (FALSE) { # \dontrun{
library(rtabpfn)
library(ggplot2)

model <- tab_pfn_regression(X, y)
shap_vals <- shap_values(model, X)

# Plot SHAP dependence for a feature
plot_shap_dependence(shap_vals, X, feature = "hp")
} # }
```
