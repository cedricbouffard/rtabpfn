library(reticulate)

# Check current Python configuration
cat("Current Python configuration:\n")
cat("  Python:", py_config()$python, "\n")
cat("  Version:", py_config()$version_string, "\n")

# Set Python to use the correct virtual environment
virtualenv_path <- "C:/venvs/tabpfn"
use_python(python_path = file.path(virtualenv_path, "Scripts", "python.exe"), required = TRUE)

# Verify
cat("\nAfter setting Python:\n")
cat("  Python:", py_python(), "\n")

# Verify tabpfn_extensions is available
tabpfn_available <- py_module_available("tabpfn")
tabpfn_ext_available <- py_module_available("tabpfn_extensions")

cat("\nModule availability:\n")
cat("  tabpfn:", if (tabpfn_available) "✓" else "✗", "\n")
cat("  tabpfn_extensions:", if (tabpfn_ext_available) "✓" else "✗", "\n")

if (tabpfn_ext_available) {
  cat("\n✓ tabpfn_extensions is available in reticulate!\n")
} else {
  cat("\n✗ tabpfn_extensions is NOT available\n")
}
