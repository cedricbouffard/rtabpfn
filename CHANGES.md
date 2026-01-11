# Fix for Quantile Format Error and SHAP Extension

## Summary

Fixed the quantile prediction error and added SHAP (SHapley Additive exPlanations) support to the rtabpfn package.

## Changes Made

### 1. Fixed Quantile Format Error

**Problem**: The error occurred when using decimal quantiles like `c(0.025, 0.25, 0.5, 0.75, 0.975)`. The format string `%03d` expected integers, but `quantiles * 100` produced decimal values (2.5, 25, 50, 75, 97.5).

**Solution**: Changed the format string from `%03d` to `%05.1f` in `R/tabpfn.R:99`

**File**: `R/tabpfn.R`
- Line 99: Changed `sprintf("%03d", quantiles * 100)` to `sprintf("%05.1f", quantiles * 100)`

**Result**: Column names are now formatted as `.pred_q002`, `.pred_q025`, `.pred_q050`, `.pred_q075`, `.pred_q097` instead of failing.

### 2. Added SHAP Extension

**Features Added**:
- `shap_values()`: Calculate SHAP values for TabPFN models
- `plot_shap_summary()`: Visualize feature importance via SHAP
- `plot_shap_dependence()`: Plot SHAP dependence for specific features
- `explain_prediction()`: Explain individual predictions with SHAP

**Files Added**:
- `R/shap.R`: Complete SHAP implementation using tabpfn_extensions Python package

**Files Modified**:
- `NAMESPACE`: Exported new SHAP functions and added print method for shap_explanation
- `DESCRIPTION`: Added `tidyr` and moved `dplyr` from Suggests to Imports
- `README.md`: Updated documentation with SHAP examples
- `tests/testthat/test-predict.R`: Added tests for decimal quantiles and SHAP functions

**Dependencies**:
- Requires `tabpfn-extensions` Python package (install with `pip install tabpfn-extensions`)

## Usage

### Fixed Quantile Predictions

```r
library(rtabpfn)

# Train model
model <- tab_pfn_regression(X, y)

# Now works with decimal quantiles!
preds <- predict(model, X, type = "quantiles",
                 quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))
# Returns columns: .pred_q002, .pred_q025, .pred_q050, .pred_q075, .pred_q097
```

### SHAP Explanations

```r
# Install tabpfn-extensions first
# reticulate::py_install("tabpfn-extensions")

# Calculate SHAP values
shap_vals <- shap_values(model, X)

# Plot feature importance
plot_shap_summary(shap_vals, top_n = 10)

# Explain individual prediction
explanation <- explain_prediction(model, single_obs)
print(explanation)
```

## Testing

Test files added to `tests/testthat/test-predict.R`:
- `test_that("decimal quantile predictions work")`: Tests the quantile format fix
- `test_that("SHAP values can be computed")`: Tests SHAP value computation
- `test_that("explain_prediction works")`: Tests individual prediction explanation

## Example Scripts

- `examples/test-quantile-fix.R`: Demonstrates the quantile fix
- `examples/test-shap.R`: Demonstrates SHAP functionality

## Notes

- The SHAP implementation requires the `tabpfn-extensions` Python package
- SHAP tests are skipped if `tabpfn-extensions` is not available
- All functions are compatible with the tidyverse (return tibbles)
- SHAP plotting functions require `ggplot2`
