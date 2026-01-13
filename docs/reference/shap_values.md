# Calculate SHAP Values for a TabPFN Model

Calculate SHAP Values for a TabPFN Model

## Usage

``` r
shap_values(object, new_data, verbose = TRUE, ...)
```

## Arguments

- object:

  A fitted TabPFN model object

- new_data:

  A data frame of observations to explain

- verbose:

  Print progress information

- ...:

  Additional arguments passed to get_shap_values

## Value

A tibble with SHAP values for each observation and feature

## Examples

``` r
if (FALSE) { # \dontrun{
library(rtabpfn)

# Train model
X <- mtcars[, c("cyl", "disp", "hp", "wt")]
y <- mtcars$mpg
model <- tab_pfn_regression(X, y)

# Calculate SHAP values
shap_vals <- shap_values(model, X[1:5, ])
} # }
```
