# Test TabICL model functions
library(rtabpfn)
library(parsnip)

# Setup TabICL test data
set.seed(42)

# Regression data
X_reg <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  x3 = rnorm(100)
)
y_reg <- X_reg$x1 + 2 * X_reg$x2 + rnorm(100, 0, 0.1)

# Classification data
X_cls <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100)
)
y_cls <- factor(ifelse(X_cls$x1 + X_cls$x2 > 0, "A", "B"))

# Check if TabICL is available
check_tabicl(install = FALSE)

# Install TabICL if needed
# check_tabicl(install = TRUE)

# Method 1: Direct usage (similar to tabpfn)
cat("\n=== TabICL Direct Usage ===\n")

# Regression
model_reg <- tab_icl_regression(
  X = X_reg[1:80, ],
  y = y_reg[1:80],
  n_estimators = 4,
  device = "cpu"
)
print(model_reg)

preds_reg <- predict(model_reg, X_reg[81:100, ], type = "numeric")
head(preds_reg)

# Quantile predictions (new!)
cat("\n--- Quantile Predictions ---\n")
preds_quantiles <- predict(model_reg, X_reg[81:100, ], type = "quantiles", 
                           quantiles = c(0.1, 0.5, 0.9))
head(preds_quantiles)

# Prediction intervals (new!)
cat("\n--- Prediction Intervals ---\n")
preds_interval <- predict(model_reg, X_reg[81:100, ], type = "conf_int", level = 0.95)
head(preds_interval)

# Classification
model_cls <- tab_icl_classification(
  X = X_cls[1:80, ],
  y = y_cls[1:80],
  n_estimators = 4,
  device = "cpu"
)
print(model_cls)

preds_cls <- predict(model_cls, X_cls[81:100, ], type = "class")
head(preds_cls)

preds_prob <- predict(model_cls, X_cls[81:100, ], type = "prob")
head(preds_prob)

# Method 2: Using parsnip interface (tidymodels)
cat("\n=== TabICL with parsnip/tidymodels ===\n")

# Regression with parsnip
spec_reg <- tab_icl(mode = "regression", n_estimators = 4, device = "cpu")
print(spec_reg)

fit_reg <- fit(spec_reg, y_reg ~ ., data = cbind(X_reg, y_reg)[1:80, ])
print(fit_reg)

preds_reg_parsnip <- predict(fit_reg, X_reg[81:100, ])
head(preds_reg_parsnip)

# Classification with parsnip
spec_cls <- tab_icl(mode = "classification", n_estimators = 4, device = "cpu")
print(spec_cls)

fit_cls <- fit(spec_cls, y_cls ~ ., data = cbind(X_cls, y_cls)[1:80, ])
print(fit_cls)

preds_cls_parsnip <- predict(fit_cls, X_cls[81:100, ], type = "class")
head(preds_cls_parsnip)

preds_prob_parsnip <- predict(fit_cls, X_cls[81:100, ], type = "prob")
head(preds_prob_parsnip)

# Method 3: Using workflows
cat("\n=== TabICL with workflows ===\n")

if (requireNamespace("workflows", quietly = TRUE)) {
  library(workflows)

  wf <- workflow() %>%
    add_model(tab_icl(mode = "classification", n_estimators = 4)) %>%
    add_formula(y_cls ~ .)

  wf_fit <- fit(wf, data = cbind(X_cls, y_cls)[1:80, ])
  wf_preds <- predict(wf_fit, X_cls[81:100, ])
  head(wf_preds)
}

cat("\n=== All tests completed successfully! ===\n")
