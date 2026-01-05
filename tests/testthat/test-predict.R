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
