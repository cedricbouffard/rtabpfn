# Configure TabPFN Python Environment

Sets up of Python environment for TabPFN usage. Automatically checks for
TabPFN virtual environment in C:/venvs/tabpfn/ by default. Detects and
configures GPU support.

## Usage

``` r
setup_tabpfn(
  python_path = NULL,
  envname = "tabpfn",
  force = FALSE,
  install_shap = FALSE,
  install_unsupervised = FALSE,
  disable_analytics = TRUE,
  setup_gpu = TRUE,
  force_gpu = FALSE,
  cuda_version = NULL
)
```

## Arguments

- python_path:

  Path to Python executable (e.g., "C:/venvs/tabpfn/Scripts/python.exe")

- envname:

  Name of the virtual environment (used only if python_path is NULL)

- force:

  Logical. If TRUE, recreates environment even if it exists

- install_shap:

  Logical. If TRUE, installs tabpfn-extensions for SHAP support

- install_unsupervised:

  Logical. If TRUE, installs tabpfn-extensions unsupervised module

- disable_analytics:

  Logical. If TRUE, disables PostHog analytics (default: TRUE)

- setup_gpu:

  Logical. If TRUE, attempts to setup GPU support (default: TRUE)

- force_gpu:

  Logical. If TRUE, forces GPU installation even if not detected

- cuda_version:

  CUDA version to install (default: NULL for auto-detect)

## Value

NULL (invisible)

## Examples

``` r
if (FALSE) { # \dontrun{
# Auto-detect TabPFN venv in C:/venvs/tabpfn/
setup_tabpfn()

# Setup with custom Python path
setup_tabpfn(python_path = "C:/venvs/tabpfn/Scripts/python.exe")

# Setup with environment name
setup_tabpfn(envname = "tabpfn")

# Setup with SHAP support
setup_tabpfn(install_shap = TRUE)

# Setup with unsupervised anomaly detection
setup_tabpfn(install_unsupervised = TRUE)

# Setup without GPU
setup_tabpfn(setup_gpu = FALSE)

# Force GPU installation
setup_tabpfn(force_gpu = TRUE)
} # }
```
