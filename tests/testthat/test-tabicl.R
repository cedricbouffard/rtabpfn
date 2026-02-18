# Test TabICL model functions
library(testthat)
library(rtabpfn)

# Skip tests if Python/reticulate is not available
skip_if_not_installed("reticulate")

# Setup TabICL test data
set.seed(42)
X_reg <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  x3 = rnorm(100)
)
y_reg <- X_reg$x1 + 2 * X_reg$x2 + rnorm(100, 0, 0.1)

X_cls <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100)
)
y_cls <- factor(ifelse(X_cls$x1 + X_cls$x2 > 0, "A", "B"))

test_that("TabICL regression model can be created", {
  skip_if_not(reticulate::py_module_available("tabicl"), "TabICL not available")

  # Test creating regressor
  model <- tab_icl_regression(X_reg, y_reg, n_estimators = 4, device = "cpu")

  expect_s3_class(model, "tab_icl")
  expect_equal(model$mode, "regression")
  expect_equal(length(model$predictor_names), 3)
  expect_equal(model$n_estimators, 4)
})

test_that("TabICL classification model can be created", {
  skip_if_not(reticulate::py_module_available("tabicl"), "TabICL not available")

  # Test creating classifier
  model <- tab_icl_classification(X_cls, y_cls, n_estimators = 4, device = "cpu")

  expect_s3_class(model, "tab_icl")
  expect_equal(model$mode, "classification")
  expect_equal(length(model$levels), 2)
  expect_equal(model$n_estimators, 4)
})

test_that("TabICL model specification works", {
  spec <- tab_icl(mode = "regression")

  expect_s3_class(spec, "tab_icl")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "tabicl")
})

test_that("TabICL print method works", {
  spec <- tab_icl(mode = "classification")

  expect_output(print(spec), "TabICL Model Specification")
  expect_output(print(spec), "classification")
})

test_that("TabICL check function works", {
  # Should return logical
  result <- check_tabicl(install = FALSE)
  expect_type(result, "logical")
})

test_that("TabICL predict method works for regression", {
  skip_if_not(reticulate::py_module_available("tabicl"), "TabICL not available")

  model <- tab_icl_regression(X_reg[1:80, ], y_reg[1:80], n_estimators = 4, device = "cpu")
  preds <- predict(model, X_reg[81:100, ], type = "numeric")

  expect_s3_class(preds, "tbl_df")
  expect_equal(nrow(preds), 20)
  expect_true(".pred" %in% names(preds))
})

test_that("TabICL predict method works for classification", {
  skip_if_not(reticulate::py_module_available("tabicl"), "TabICL not available")

  model <- tab_icl_classification(X_cls[1:80, ], y_cls[1:80], n_estimators = 4, device = "cpu")

  # Test class prediction
  preds_class <- predict(model, X_cls[81:100, ], type = "class")
  expect_s3_class(preds_class, "tbl_df")
  expect_equal(nrow(preds_class), 20)
  expect_true(".pred_class" %in% names(preds_class))

  # Test probability prediction
  preds_prob <- predict(model, X_cls[81:100, ], type = "prob")
  expect_s3_class(preds_prob, "tbl_df")
  expect_equal(nrow(preds_prob), 20)
  expect_true(".pred_A" %in% names(preds_prob))
  expect_true(".pred_B" %in% names(preds_prob))
})

test_that("TabICL parsnip fit works for regression", {
  skip_if_not(reticulate::py_module_available("tabicl"), "TabICL not available")
  skip_if_not_installed("parsnip")

  spec <- tab_icl(mode = "regression", n_estimators = 4, device = "cpu")
  fit_obj <- fit(spec, y_reg ~ ., data = cbind(X_reg, y_reg))

  expect_s3_class(fit_obj, "tab_icl_fit")
  expect_s3_class(fit_obj$model, "tab_icl")
})

test_that("TabICL parsnip fit works for classification", {
  skip_if_not(reticulate::py_module_available("tabicl"), "TabICL not available")
  skip_if_not_installed("parsnip")

  spec <- tab_icl(mode = "classification", n_estimators = 4, device = "cpu")
  fit_obj <- fit(spec, y_cls ~ ., data = cbind(X_cls, y_cls))

  expect_s3_class(fit_obj, "tab_icl_fit")
  expect_s3_class(fit_obj$model, "tab_icl")
})

test_that("TabICL parsnip predict works", {
  skip_if_not(reticulate::py_module_available("tabicl"), "TabICL not available")
  skip_if_not_installed("parsnip")

  spec <- tab_icl(mode = "regression", n_estimators = 4, device = "cpu")
  fit_obj <- fit(spec, y_reg ~ ., data = cbind(X_reg, y_reg)[1:80, ])

  preds <- predict(fit_obj, X_reg[81:100, ])

  expect_s3_class(preds, "tbl_df")
  expect_equal(nrow(preds), 20)
})
