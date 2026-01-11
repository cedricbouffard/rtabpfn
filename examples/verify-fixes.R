library(rtabpfn)

# Test 1: Verify quantile fix
cat("=== Test 1: Quantile Format Fix ===\n")
set.seed(42)
X <- data.frame(x1 = rnorm(50), x2 = rnorm(50))
y <- 2 * X$x1 + 3 * X$x2 + rnorm(50, sd = 0.5)

model <- tab_pfn_regression(X, y)

# Test with decimal quantiles (the original error case)
cat("Testing with quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975)...\n")
preds <- predict(model, X[1:5, ], type = "quantiles",
                 quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))
print(preds)

cat("\n✓ Quantile predictions work correctly!\n")
cat("Column names:", paste(colnames(preds), collapse = ", "), "\n")

# Test 2: Verify SHAP functionality
cat("\n=== Test 2: SHAP Extension ===\n")

# Check if tabpfn-extensions is available
tabpfn_ext_available <- reticulate::py_module_available("tabpfn_extensions")

if (tabpfn_ext_available) {
  cat("✓ tabpfn_extensions is available\n")

  # Import the module
  shap_module <- reticulate::import("tabpfn_extensions.interpretability", convert = FALSE)
  cat("✓ Successfully imported tabpfn_extensions.interpretability\n")

  # Check available functions
  cat("\nAvailable functions in tabpfn_extensions.interpretability:\n")
  tryCatch({
    has_shap <- reticulate::py_has_attr(shap_module, "get_shap_values")
    cat("  - get_shap_values:", if (has_shap) "✓" else "✗", "\n")

    has_plot_shap <- reticulate::py_has_attr(shap_module, "plot_shap")
    cat("  - plot_shap:", if (has_plot_shap) "✓" else "✗", "\n")
  }, error = function(e) {
    cat("  Error checking attributes:", e$message, "\n")
  })

  # Test SHAP value calculation
  cat("\nTesting SHAP value calculation...\n")
  shap_vals <- shap_values(model, X[1:10, ], verbose = TRUE)
  cat("\n✓ SHAP values calculated successfully!\n")
  print(head(shap_vals))

  # Test explanation
  cat("\nTesting individual prediction explanation...\n")
  explanation <- explain_prediction(model, X[1, , drop = FALSE])
  print(explanation)

} else {
  cat("✗ tabpfn_extensions is NOT available\n")
  cat("  Please install with: pip install tabpfn-extensions\n")
}

cat("\n=== All Tests Complete ===\n")
