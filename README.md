```markdown
# rtabpfn

R interface to TabPFN (Tabular Prior-Fitted Network) with support for quantile predictions, prediction intervals, and SHAP explanations.

## Installation

```r
# Install from GitHub
# devtools::install_github("cedricbouffard/rtabpfn")

```

## Setup

First, setup the Python environment:

```r
library(rtabpfn)

# Setup Python environment and install TabPFN
setup_tabpfn()

# For SHAP support, install tabpfn-extensions
setup_tabpfn(install_shap = TRUE)
```

If you already have a Python virtual environment, configure reticulate to use it:

```r
library(reticulate)

# Set Python to your virtual environment
use_python("C:/path/to/your/venv/Scripts/python.exe", required = TRUE)

# Install tabpfn-extensions if needed
py_install("tabpfn-extensions", pip = TRUE)
```

Verify SHAP is available:

```r
check_shap_available()  # Returns TRUE if available
```

## Quick Start

### Regression with Quantile Predictions

```r
library(rtabpfn)

# Prepare data
X <- mtcars[, c("cyl", "disp", "hp", "wt")]
y <- mtcars$mpg

# Train model
model <- tab_pfn_regression(X, y)

# Get point predictions
preds <- predict(model, X, type = "numeric")

# Get quantile predictions (now supports decimals!)
preds_q <- predict(model, X, type = "quantiles", 
                   quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))

# Get prediction intervals
preds_int <- predict(model, X, type = "conf_int", level = 0.95)
```

### Classification

```r
# Prepare classification data
X <- iris[, 1:4]
y <- iris$Species

# Train model
model <- tab_pfn_classification(X, y)

# Get class predictions
preds_class <- predict(model, X, type = "class")

# Get class probabilities
preds_prob <- predict(model, X, type = "prob")
```

### SHAP Explanations

```r
# Install tabpfn-extensions for SHAP support
# reticulate::py_install("tabpfn-extensions")

# Check if SHAP is available
check_shap_available()

# Calculate SHAP values
shap_vals <- shap_values(model, X)

# Plot SHAP summary
plot_shap_summary(shap_vals)

# Explain individual prediction
explanation <- explain_prediction(model, X[1, , drop = FALSE])
print(explanation)
```

## Features

- **Quantile Predictions**: Get uncertainty estimates via quantile regression (supports decimal quantiles)
- **Prediction Intervals**: Calculate confidence intervals for predictions
- **SHAP Explanations**: Compute SHAP values for model interpretability
- **Multiple Output Types**: Support for mean, median, mode, and full distribution
- **Classification Support**: Both class labels and probabilities
- **Tidyverse Compatible**: Returns tibbles with standard column names

## Prediction Types

### Regression

- `type = "numeric"`: Point predictions (mean by default)
- `type = "quantiles"`: Quantile predictions for uncertainty
- `type = "conf_int"`: Prediction/confidence intervals
- `type = "raw"`: Raw Python object

### Classification

- `type = "class"`: Predicted class labels
- `type = "prob"`: Class probabilities
- `type = "raw"`: Raw Python object

## Advanced Usage

### Custom Quantiles

```r
# Specify custom quantiles (now supports decimals!)
preds <- predict(model, new_data,
                 type = "quantiles",
                 quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))
```

### SHAP Values

```r
# Calculate SHAP values for model interpretation
shap_vals <- shap_values(model, new_data, verbose = TRUE)

# Visualize feature importance
plot_shap_summary(shap_vals, top_n = 10)

# Plot SHAP dependence for a specific feature
plot_shap_dependence(shap_vals, feature = "hp")

# Explain individual predictions
explanation <- explain_prediction(model, single_observation)
```

## Troubleshooting

### SHAP Not Available

If you get an error about `tabpfn_extensions` not being available:

```r
# Check if SHAP is available
check_shap_available()

# If FALSE, install tabpfn-extensions
# Option 1: Using setup_tabpfn
setup_tabpfn(install_shap = TRUE)

# Option 2: Manual installation via reticulate
reticulate::py_install("tabpfn-extensions", pip = TRUE)

# Option 3: Direct pip installation in your virtual environment
# System command: C:/venvs/tabpfn/Scripts/python.exe -m pip install tabpfn-extensions
```

### Wrong Python Environment

If reticulate is using the wrong Python environment:

```r
library(reticulate)

# Check current Python
py_config()

# Set to your virtual environment
use_python("C:/venvs/tabpfn/Scripts/python.exe", required = TRUE)

# Verify modules are available
py_module_available("tabpfn")           # Should be TRUE
py_module_available("tabpfn_extensions") # Should be TRUE for SHAP
```

### Different Output Types

```r
# Mean prediction (default)
predict(model, new_data, output_type = "mean")

# Mode prediction
predict(model, new_data, output_type = "mode")

# Full distribution (all ensemble predictions)
predict(model, new_data, output_type = "full")
```

## References

- TabPFN Paper: [Nature (2024)](https://www.nature.com/articles/s41586-024-08328-6)
- TabPFN GitHub: https://github.com/PriorLabs/TabPFN
- Prior Labs: https://priorlabs.ai/
- tabpfn-extensions: https://pypi.org/project/tabpfn-extensions/

## License

MIT
```
