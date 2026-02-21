#' Setup TabICL Python Environment
#'
#' @description
#' Configures the Python environment for TabICL usage. Installs the tabicl
#' package if not already available.
#'
#' @param envname Name of the virtual environment (default: "tabpfn")
#' @param install_if_missing Logical. If TRUE, installs TabICL if not found.
#' @param ... Additional arguments passed to py_install
#'
#' @return Logical indicating if TabICL is available
#' @export
#'
#' @examples
#' \dontrun{
#' # Check and install TabICL
#' setup_tabicl()
#'
#' # Force reinstall
#' setup_tabicl(install_if_missing = TRUE)
#' }
setup_tabicl <- function(envname = "tabpfn", install_if_missing = TRUE, ...) {
  rtabpfn:::ensure_python_env()

  rtabpfn:::check_and_warn_libpython_mismatch()

  has_tabicl <- reticulate::py_module_available("tabicl")

  if (!has_tabicl && install_if_missing) {
    message("TabICL not found. Installing...")

    tryCatch({
      reticulate::py_install("tabicl", envname = envname, pip = TRUE, ...)
      message("TabICL installed successfully!")
      message("\nNOTE: Please restart R to use TabICL.")
      message("The Python environment needs to be reloaded after installation.")
      has_tabicl <- FALSE  # Will be TRUE after restart
    }, error = function(e) {
      warning("Failed to install TabICL: ", e$message)
      message("You can install it manually with: pip install tabicl")
      has_tabicl <- FALSE
    })
  }

  invisible(has_tabicl)
}

#' Validate TabICL Installation
#'
#' @description
#' Validates that TabICL is properly installed and configured.
#'
#' @return List with validation results
#' @export
#'
#' @examples
#' \dontrun{
#' validate_tabicl()
#' }
validate_tabicl <- function() {
  rtabpfn:::ensure_python_env()

  results <- list(
    available = FALSE,
    version = NULL,
    device = NULL,
    libpython_mismatch = FALSE
  )

  cat("=== TabICL Validation ===\n\n")

  cat("0. Python Environment:\n")
  libpython_status <- rtabpfn:::get_libpython_status()

  if (!is.null(libpython_status$python_path)) {
    cat("   Python:", libpython_status$python_path, "\n")
    cat("   Libpython:", libpython_status$libpython_path, "\n")

    if (libpython_status$is_mismatch) {
      cat("   *** LIBPYTHON MISMATCH DETECTED ***\n")
      cat("   Run diagnose_python_env() for solutions.\n")
      results$libpython_mismatch <- TRUE
    } else {
      cat("   Status: OK\n")
    }
  }
  cat("\n")

  # Check if TabICL is available
  has_tabicl <- reticulate::py_module_available("tabicl")
  cat("1. TabICL Module:\n")
  cat("   Available:", has_tabicl, "\n")
  results$available <- has_tabicl

  if (has_tabicl) {
    tryCatch({
      tabicl <- reticulate::import("tabicl")
      # Try to get version, but some versions may not have __version__
      version <- tryCatch({
        as.character(tabicl$`__version__`)
      }, error = function(e) {
        "unknown"
      })
      cat("   Version:", version, "\n")
      results$version <- version

      # Check available devices
      cat("\n2. Available Devices:\n")

      if (reticulate::py_module_available("torch")) {
        torch <- reticulate::import("torch")
        if (torch$cuda$is_available()) {
          cat("   CUDA: Available\n")
          cat("   Device count:", torch$cuda$device_count(), "\n")
          results$device <- "cuda"
        } else {
          cat("   CUDA: Not available\n")
          results$device <- "cpu"
        }
      } else {
        cat("   PyTorch: Not available\n")
        results$device <- "cpu"
      }

    }, error = function(e) {
      cat("   Warning: Could not retrieve TabICL info: ", e$message, "\n")
    })
  } else {
    cat("\n   To install TabICL, run:\n")
    cat("   setup_tabicl()\n")
    cat("   or\n")
    cat("   reticulate::py_install('tabicl')\n")
  }

  cat("\n=== Validation Complete ===\n")

  invisible(results)
}
