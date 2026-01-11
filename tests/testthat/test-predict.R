test_that("regression predictions work", {
  skip_if_not_installed("reticulate")
  skip_if(!reticulate::py_module_available("tabpfn"))

  # Create simple data
  X <- data.frame(x1 = 1:10, x2 = 11:20)
  y <- 1:10

  model <- tab_pfn_regression(X, y)

  # Test basic prediction
  preds <- predict(model, X, type = "numeric")
  expect_s3_class(preds, "tbl_df")
  expect_equal(nrow(preds), nrow(X))
  expect_true(".pred" %in% names(preds))
})

test_that("quantile predictions work", {
  skip_if_not_installed("reticulate")
  skip_if(!reticulate::py_module_available("tabpfn"))

  X <- data.frame(x1 = 1:10, x2 = 11:20)
  y <- 1:10

  model <- tab_pfn_regression(X, y)

  # Test quantile predictions
  preds <- predict(model, X, type = "quantiles",
                   quantiles = c(0.1, 0.5, 0.9))

  expect_s3_class(preds, "tbl_df")
  expect_equal(nrow(preds), nrow(X))
  expect_equal(ncol(preds), 3)
  expect_true(all(c(".pred_q010", ".pred_q050", ".pred_q090") %in% names(preds)))
})

test_that("decimal quantile predictions work", {
  skip_if_not_installed("reticulate")
  skip_if(!reticulate::py_module_available("tabpfn"))

  X <- data.frame(x1 = 1:10, x2 = 11:20)
  y <- 1:10

  model <- tab_pfn_regression(X, y)

  # Test decimal quantile predictions (the original error case)
  preds <- predict(model, X, type = "quantiles",
                   quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))

  expect_s3_class(preds, "tbl_df")
  expect_equal(nrow(preds), nrow(X))
  expect_equal(ncol(preds), 5)
  expect_true(all(c(".pred_q002", ".pred_q025", ".pred_q050",
                    ".pred_q075", ".pred_q097") %in% names(preds)))
})

test_that("classification predictions work", {
  skip_if_not_installed("reticulate")
  skip_if(!reticulate::py_module_available("tabpfn"))

  X <- data.frame(x1 = rnorm(50), x2 = rnorm(50))
  y <- factor(rep(c("A", "B"), 25))

  model <- tab_pfn_classification(X, y)

  # Test class predictions
  preds <- predict(model, X, type = "class")
  expect_s3_class(preds, "tbl_df")
  expect_true(".pred_class" %in% names(preds))

  # Test probability predictions
  preds_prob <- predict(model, X, type = "prob")
  expect_s3_class(preds_prob, "tbl_df")
  expect_true(all(c(".pred_A", ".pred_B") %in% names(preds_prob)))
})

test_that("SHAP values can be computed", {
  skip_if_not_installed("reticulate")
  skip_if(!reticulate::py_module_available("tabpfn"))
  skip_if(!reticulate::py_module_available("tabpfn_extensions"))

  X <- data.frame(x1 = 1:10, x2 = 11:20)
  y <- 1:10

  model <- tab_pfn_regression(X, y)

  # Test SHAP values
  shap_vals <- shap_values(model, X[1:5, ], verbose = FALSE)

  expect_s3_class(shap_vals, "tbl_df")
  expect_equal(nrow(shap_vals), 5)
  expect_true(".base_value" %in% names(shap_vals))
  expect_true("observation" %in% names(shap_vals))
})

test_that("explain_prediction works", {
  skip_if_not_installed("reticulate")
  skip_if(!reticulate::py_module_available("tabpfn"))
  skip_if(!reticulate::py_module_available("tabpfn_extensions"))

  X <- data.frame(x1 = 1:10, x2 = 11:20)
  y <- 1:10

  model <- tab_pfn_regression(X, y)

  # Test explanation
  explanation <- explain_prediction(model, X[1, , drop = FALSE])

  expect_s3_class(explanation, "shap_explanation")
  expect_true("shap_values" %in% names(explanation))
  expect_true("base_value" %in% names(explanation))
  expect_true("prediction" %in% names(explanation))
})
