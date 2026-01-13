# Explain Individual Prediction with SHAP

Explain Individual Prediction with SHAP

## Usage

``` r
explain_prediction(object, new_data, ...)
```

## Arguments

- object:

  A fitted TabPFN model object

- new_data:

  A single row data frame to explain

- ...:

  Additional arguments passed to shap_values

## Value

A list with SHAP values and base value for prediction

## Examples

``` r
if (FALSE) { # \dontrun{
library(rtabpfn)

model <- tab_pfn_regression(X, y)

# Explain a single prediction
explanation <- explain_prediction(model, X[1, , drop = FALSE])

# View SHAP values
print(explanation$shap_values)
} # }
```
