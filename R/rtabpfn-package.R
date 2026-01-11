#' @keywords internal

## usethis namespace: start
## usethis namespace: end

# Store the configured Python path as an R option
.tabpfn_options <- new.env(parent = emptyenv())
.tabpfn_options$python_path <- NULL

# Function to ensure the correct Python environment is active
ensure_python_env <- function() {
  if (!is.null(.tabpfn_options$python_path) && file.exists(.tabpfn_options$python_path)) {
    current_python <- tryCatch({
      reticulate::py_config()$python
    }, error = function(e) NULL)

    if (is.null(current_python) || !identical(normalizePath(current_python), normalizePath(.tabpfn_options$python_path))) {
      message("Configuring TabPFN Python environment...")
      tryCatch({
        reticulate::use_python(.tabpfn_options$python_path, required = TRUE)
      }, error = function(e) {
        warning("Failed to configure Python environment: ", e$message)
      })
    }
  }
}

# Auto-configure environment on package load
.onLoad <- function(libname, pkgname) {
  # Auto-detect TabPFN venv in C:/venvs/ if configured path doesn't exist
  if (is.null(.tabpfn_options$python_path)) {
    default_venv <- "C:/venvs/tabpfn/Scripts/python.exe"
    if (file.exists(default_venv)) {
      .tabpfn_options$python_path <- default_venv
      tryCatch({
        reticulate::use_python(default_venv, required = FALSE)
      }, error = function(e) {
        # Silent fail during package load
      })
    }
  } else {
    ensure_python_env()
  }
}

NULL

#'
#' @description
#' Helper function to check if TabPFN is installed in the Python environment
#' and optionally install it if missing.
#'
#' @param install Logical. If TRUE, will attempt to install TabPFN if not found.
#' @param envname Name of the virtual environment to use
#' @param method Installation method: "auto", "virtualenv", or "conda"
#'
#' @return Logical indicating if TabPFN is available
#' @export
#'
#' @examples
#' \dontrun{
#' # Check if TabPFN is available
#' check_tabpfn()
#'
#' # Install if not available
#' check_tabpfn(install = TRUE)
#' }
check_tabpfn <- function(install = FALSE,
                         envname = "tabpfn",
                         method = "auto") {

  rtabpfn:::ensure_python_env()

  has_tabpfn <- reticulate::py_module_available("tabpfn")

  if (!has_tabpfn && install) {
    message("TabPFN not found. Installing...")

    # Create virtual environment if it doesn't exist
    if (!envname %in% reticulate::virtualenv_list()) {
      reticulate::virtualenv_create(envname, python = NULL)
    }

    # Use the virtual environment
    reticulate::use_virtualenv(envname, required = FALSE)

    # Install TabPFN
    reticulate::py_install("tabpfn", envname = envname, method = method, pip = TRUE)

    has_tabpfn <- reticulate::py_module_available("tabpfn")

    if (has_tabpfn) {
      message("TabPFN installed successfully!")
    } else {
      warning("Failed to install TabPFN. Please install manually.")
    }
  }

  invisible(has_tabpfn)
}


#' Configure TabPFN Python Environment
#'
#' @description
#' Sets up of Python environment for TabPFN usage. Automatically checks for TabPFN
#' virtual environment in C:/venvs/tabpfn/ by default.
#'
#' @param python_path Path to Python executable (e.g., "C:/venvs/tabpfn/Scripts/python.exe")
#' @param envname Name of the virtual environment (used only if python_path is NULL)
#' @param force Logical. If TRUE, recreates environment even if it exists
#' @param install_shap Logical. If TRUE, installs tabpfn-extensions for SHAP support
#' @param install_unsupervised Logical. If TRUE, installs tabpfn-extensions unsupervised module
#' @param disable_analytics Logical. If TRUE, disables PostHog analytics (default: TRUE)
#'
#' @return NULL (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' # Auto-detect TabPFN venv in C:/venvs/tabpfn/
#' setup_tabpfn()
#'
#' # Setup with custom Python path
#' setup_tabpfn(python_path = "C:/venvs/tabpfn/Scripts/python.exe")
#'
#' # Setup with environment name
#' setup_tabpfn(envname = "tabpfn")
#'
#' # Setup with SHAP support
#' setup_tabpfn(install_shap = TRUE)
#'
#' # Setup with unsupervised anomaly detection
#' setup_tabpfn(install_unsupervised = TRUE)
#' }
setup_tabpfn <- function(python_path = NULL, envname = "tabpfn", force = FALSE, install_shap = FALSE, install_unsupervised = FALSE, disable_analytics = TRUE) {

  # Disable analytics by default to avoid PostHog warnings
  if (disable_analytics) {
    Sys.setenv("DO_NOT_TRACK" = "1")
  }

  # Auto-detect TabPFN venv in C:/venvs/ if python_path not specified
  if (is.null(python_path)) {
    default_venv <- "C:/venvs/tabpfn/Scripts/python.exe"
    if (file.exists(default_venv)) {
      message("Found TabPFN virtual environment at: ", default_venv)
      python_path <- default_venv
    }
  }

  if (!is.null(python_path)) {
    # Validate Python path exists
    if (!file.exists(python_path)) {
      stop("Python executable not found at: ", python_path,
           "\nPlease check the path or create a new environment.")
    }

    # Use specified or auto-detected Python path
    message("Using Python: ", python_path)
    tryCatch({
      reticulate::use_python(python_path, required = TRUE)
      .tabpfn_options$python_path <- python_path
    }, error = function(e) {
      stop("Failed to initialize Python environment at: ", python_path,
           "\nError: ", e$message,
           "\n\nSolutions:",
           "\n1. Recreate the virtual environment:",
           "   reticulate::virtualenv_remove('tabpfn')",
           "   setup_tabpfn()",
           "\n2. Or specify a different Python path:",
           "   setup_tabpfn(python_path = 'path/to/python.exe')")
    })
  } else {
    # Use virtual environment by name
    existing_envs <- reticulate::virtualenv_list()

    if (force || !envname %in% existing_envs) {
      message("Creating virtual environment: ", envname)
      reticulate::virtualenv_create(envname, python = NULL)
    }

    message("Using virtual environment: ", envname)
    reticulate::use_virtualenv(envname, required = TRUE)

    # Store the Python path from the virtualenv
    tryCatch({
      .tabpfn_options$python_path <- reticulate::py_config()$python
    }, error = function(e) {
      # Continue even if we can't store the path
    })
  }

  # Check and install TabPFN
  check_tabpfn(install = TRUE, envname = envname)

  # Optionally install tabpfn-extensions for SHAP
  if (install_shap) {
    has_ext <- reticulate::py_module_available("tabpfn_extensions")

    if (!has_ext) {
      message("Installing tabpfn-extensions for SHAP support...")
      tryCatch({
        reticulate::py_install("tabpfn-extensions", envname = envname, pip = TRUE)
        message("tabpfn-extensions installed successfully!")
      }, error = function(e) {
        warning("Failed to install tabpfn-extensions: ", e$message)
        message("You can install it manually with: pip install tabpfn-extensions")
      })
    } else {
      message("tabpfn-extensions already installed.")
    }
  }

  # Optionally install tabpfn-extensions unsupervised module
  if (install_unsupervised) {
    has_unsup <- reticulate::py_module_available("tabpfn_extensions.unsupervised")

    if (!has_unsup) {
      message("Installing tabpfn-extensions[unsupervised] for anomaly detection...")
      tryCatch({
        reticulate::py_install("tabpfn-extensions[unsupervised]", envname = envname, pip = TRUE)
        message("tabpfn-extensions[unsupervised] installed successfully!")
      }, error = function(e) {
        warning("Failed to install tabpfn-extensions[unsupervised]: ", e$message)
        message("You can install it manually with: pip install 'tabpfn-extensions[unsupervised]'")
      })
    } else {
      message("tabpfn-extensions[unsupervised] already installed.")
    }
  }

  message("TabPFN environment ready!")
  invisible(NULL)
}


#' Validate TabPFN Python Environment
#'
#' @description
#' Validates that the Python environment is configured correctly and TabPFN
#' is available. Useful for troubleshooting setup issues.
#'
#' @return List with validation results
#' @export
#'
#' @examples
#' \dontrun{
#' # Validate environment
#' validate_tabpfn_env()
#' }
validate_tabpfn_env <- function() {

  rtabpfn:::ensure_python_env()

  results <- list()

  cat("=== TabPFN Environment Validation ===\n\n")

  # Check Python configuration
  cat("1. Python Configuration:\n")
  tryCatch({
    py_config <- reticulate::py_config()
    cat("   Python path:", py_config$python, "\n")
    cat("   Version:", py_config$version_string, "\n")
    results$python <- list(available = TRUE, path = py_config$python, version = py_config$version_string)
  }, error = function(e) {
    cat("   Error: ", e$message, "\n")
    results$python <- list(available = FALSE, error = e$message)
  })

  cat("\n")

  # Check TabPFN module
  cat("2. TabPFN Module:\n")
  has_tabpfn <- reticulate::py_module_available("tabpfn")
  cat("   Available:", has_tabpfn, "\n")
  results$tabpfn <- list(available = has_tabpfn)

  if (has_tabpfn) {
    tryCatch({
      tabpfn <- reticulate::import("tabpfn")
      cat("   Version:", tabpfn$`__version__`, "\n")
      results$tabpfn$version <- as.character(tabpfn$`__version__`)
    }, error = function(e) {
      cat("   Warning: Could not get version\n")
    })
  }

  cat("\n")

  # Check tabpfn_extensions
  cat("3. TabPFN Extensions:\n")
  has_ext <- reticulate::py_module_available("tabpfn_extensions")
  cat("   Available:", has_ext, "\n")
  results$extensions <- list(available = has_ext)

  if (has_ext) {
    tryCatch({
      ext <- reticulate::import("tabpfn_extensions")
      cat("   Version:", ext$`__version__`, "\n")
      results$extensions$version <- as.character(ext$`__version__`)
    }, error = function(e) {
      cat("   Warning: Could not get version\n")
    })
  }

  cat("\n")

  # Check unsupervised extension
  cat("4. Unsupervised Extension:\n")
  has_unsup <- check_unsupervised_available()
  cat("   Available:", has_unsup, "\n")
  results$unsupervised <- list(available = has_unsup)

  cat("\n")

  # Check torch
  cat("5. PyTorch:\n")
  has_torch <- reticulate::py_module_available("torch")
  cat("   Available:", has_torch, "\n")
  results$torch <- list(available = has_torch)

  if (has_torch) {
    tryCatch({
      torch <- reticulate::import("torch")
      cat("   Version:", torch$`__version__`, "\n")
      cat("   CUDA available:", torch$cuda$is_available(), "\n")
      results$torch$version <- as.character(torch$`__version__`)
      results$torch$cuda_available <- torch$cuda$is_available()
    }, error = function(e) {
      cat("   Warning: Could not get version\n")
    })
  }

  cat("\n=== Validation Complete ===\n")

  invisible(results)
}
