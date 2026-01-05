```markdown
# rtabpfn

R interface to TabPFN (Tabular Prior-Fitted Network) with support for quantile predictions and prediction intervals.

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

# Get quantile predictions
preds_q <- predict(model, X, type = "quantiles", 
                   quantiles = c(0.1, 0.5, 0.9))

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

## Features

- **Quantile Predictions**: Get uncertainty estimates via quantile regression
- **Prediction Intervals**: Calculate confidence intervals for predictions
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
# Specify custom quantiles
preds <- predict(model, new_data, 
                 type = "quantiles",
                 quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))
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

## License

MIT
```
