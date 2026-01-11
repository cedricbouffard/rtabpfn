#!/usr/bin/env Rscript

# Helper script to configure reticulate and test SHAP functionality

library(reticulate)

cat("=== Configuring Python Environment ===\n\n")

# Option 1: Use existing virtual environment
cat("Option 1: Use existing virtual environment 'C:/venvs/tabpfn'\n")
virtualenv_path <- "C:/venvs/tabpfn"
python_exe <- file.path(virtualenv_path, "Scripts", "python.exe")

if (file.exists(python_exe)) {
  use_python(python_exe, required = TRUE)
  cat("✓ Python configured to:", python_exe, "\n\n")

  # Check module availability
  cat("Checking Python modules:\n")
  cat("  tabpfn:", if (py_module_available("tabpfn")) "✓" else "✗", "\n")
  cat("  tabpfn_extensions:", if (py_module_available("tabpfn_extensions")) "✓" else "✗", "\n\n")

  if (!py_module_available("tabpfn_extensions")) {
    cat("Installing tabpfn-extensions...\n")
    tryCatch({
      py_install("tabpfn-extensions", pip = TRUE)
      cat("✓ tabpfn-extensions installed successfully!\n\n")
    }, error = function(e) {
      cat("✗ Failed to install:", e$message, "\n")
      cat("Please run manually:\n")
      cat("  ", python_exe, "-m pip install tabpfn-extensions\n\n")
    })
  }
} else {
  cat("✗ Virtual environment not found:", virtualenv_path, "\n\n")

  # Option 2: Create new environment
  cat("Option 2: Create new virtual environment\n")
  cat("Run: rtabpfn::setup_tabpfn(install_shap = TRUE)\n\n")
}

cat("=== Testing SHAP Functionality ===\n\n")

if (check_shap_available()) {
  cat("✓ SHAP is available!\n\n")

  # Create test data
  set.seed(42)
  X <- data.frame(x1 = rnorm(20), x2 = rnorm(20))
  y <- 2 * X$x1 + 3 * X$x2 + rnorm(20, sd = 0.5)

  cat("Creating test model...\n")
  model <- rtabpfn::tab_pfn_regression(X, y)

  cat("Calculating SHAP values...\n")
  shap_vals <- rtabpfn::shap_values(model, X[1:5, ], verbose = TRUE)

  cat("\n✓ SHAP values calculated successfully!\n")
  cat("Shape:", nrow(shap_vals), "rows x", ncol(shap_vals), "columns\n\n")
  print(head(shap_vals))

} else {
  cat("✗ SHAP is not available\n")
  cat("Please install tabpfn-extensions:\n")
  cat("  reticulate::py_install('tabpfn-extensions', pip = TRUE)\n")
}

cat("\n=== Complete ===\n")
