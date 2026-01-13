# TabPFN Model for tidymodels

A TabPFN (Tabular Prior-Fitted Network) model that can be used with the
tidymodels ecosystem (parsnip, workflows, tune, etc.) Get encoding for
tab_pfn model

## Usage

``` r
get_encoding.tab_pfn()

tab_pfn(
  mode = "unknown",
  engine = "tabpfn",
  N_ensemble_configurations = 8,
  max_len_feature_basis = 1024,
  device = "auto",
  ...
)

# S3 method for class 'tab_pfn'
update(
  object,
  parameters = NULL,
  N_ensemble_configurations = NULL,
  max_len_feature_basis = NULL,
  device = NULL,
  fresh = FALSE,
  ...
)
```

## Arguments

- mode:

  A single character string for the prediction mode: "classification" or
  "regression"

- engine:

  A single character string specifying the computational engine. For
  TabPFN, this is always "tabpfn"

- N_ensemble_configurations:

  Number of ensemble configurations (integer)

- max_len_feature_basis:

  Maximum length of feature basis (integer)

- device:

  Device to use: "auto", "cpu", or "cuda"

- ...:

  Additional engine-specific arguments

## Value

A tibble with encoding information

A model specification object

## Examples

``` r
if (FALSE) { # \dontrun{
library(tidymodels)

# Regression model
tab_pfn_reg_spec <- tab_pfn(mode = "regression") %>%
  set_engine("tabpfn") %>%
  fit(mpg ~ ., data = mtcars)

# Classification model
tab_pfn_cls_spec <- tab_pfn(mode = "classification") %>%
  set_engine("tabpfn") %>%
  fit(Species ~ ., data = iris)
} # }
```
