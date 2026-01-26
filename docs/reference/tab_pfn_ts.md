# TabPFN Time Series Model for tidymodels

A TabPFN Time Series (TabPFN-TS) model for zero-shot forecasting with
the tidymodels ecosystem (parsnip, workflows, tune, etc.) Get encoding
for tab_pfn_ts model

## Usage

``` r
get_encoding.tab_pfn_ts()

tab_pfn_ts(
  mode = "regression",
  engine = "tabpfn_ts",
  prediction_length = 12,
  quantiles = c(0.1, 0.5, 0.9),
  tabpfn_mode = "client",
  tabpfn_output_selection = "median",
  date_col = NULL,
  value_col = NULL,
  item_id_col = NULL,
  ...
)

# S3 method for class 'tab_pfn_ts'
update(
  object,
  parameters = NULL,
  prediction_length = NULL,
  quantiles = NULL,
  tabpfn_mode = NULL,
  tabpfn_output_selection = NULL,
  date_col = NULL,
  value_col = NULL,
  item_id_col = NULL,
  fresh = FALSE,
  ...
)
```

## Arguments

- mode:

  A single character string for the prediction mode. Only "regression"
  is supported for time series.

- engine:

  A single character string specifying the computational engine. Always
  "tabpfn_ts" for time series.

- prediction_length:

  Number of steps to forecast ahead (integer)

- quantiles:

  Numeric vector of quantiles for probabilistic forecasting

- tabpfn_mode:

  TabPFN mode: "client" (default) or "local"

- tabpfn_output_selection:

  Output selection: "median" or "mean"

- date_col:

  Name of the date/datetime column (NULL for auto-detection)

- value_col:

  Name of the value column (NULL for auto-detection)

- item_id_col:

  Name of the item_id column for multiple time series (NULL for single
  series)

- ...:

  Additional engine-specific arguments

## Value

A tibble with encoding information

A model specification object

## Examples

``` r
if (FALSE) { # \dontrun{
library(tidymodels)
library(rtabpfn)

# Time series forecasting model
ts_spec <- tab_pfn_ts(mode = "regression") %>%
  set_engine("tabpfn_ts") %>%
  set_args(prediction_length = 12, quantiles = c(0.1, 0.5, 0.9))
} # }
```
