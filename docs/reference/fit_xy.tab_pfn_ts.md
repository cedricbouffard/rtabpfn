# Fit a TabPFN Time Series model with xy interface

Fit a TabPFN Time Series model with xy interface

## Usage

``` r
fit_xy.tab_pfn_ts(object, x, y = NULL, control = parsnip::control_fit(), ...)
```

## Arguments

- object:

  A model specification

- x:

  A data frame with training data (must contain date and value columns)

- y:

  Not used (value column extracted from x)

- control:

  A \`parsnip::control_fit()\` object

- ...:

  Additional arguments

## Value

A fitted model object
