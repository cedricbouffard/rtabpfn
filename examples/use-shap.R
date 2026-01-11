library(reticulate)

# Configure reticulate to use your virtual environment
virtualenv_path <- "C:/venvs/tabpfn"
python_exe <- file.path(virtualenv_path, "Scripts", "python.exe")

cat("Setting Python to:", python_exe, "\n")
use_python(python_exe, required = TRUE)

# Verify configuration
cat("\nPython configuration:\n")
cat("  Python:", py_python(), "\n")
cat("  Version:", py_config()$version_string, "\n")

# Check modules
cat("\nChecking modules:\n")
cat("  tabpfn:", if (py_module_available("tabpfn")) "✓" else "✗", "\n")
cat("  tabpfn_extensions:", if (py_module_available("tabpfn_extensions")) "✓" else "✗", "\n")

# Now load rtabpfn and test SHAP
library(rtabpfn)

# Test SHAP
cat("\n=== Testing SHAP ===\n")
shap_vals <- rtabpfn::shap_values(mod, b |> dplyr::select(-MOS) |> dplyr::slice(1))
print(shap_vals)
