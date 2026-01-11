' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
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
#' Sets up of Python environment for TabPFN usage.
#'
#' @param envname Name of the virtual environment
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
#' # Setup environment
#' setup_tabpfn()
#'
#' # Setup with SHAP support
#' setup_tabpfn(install_shap = TRUE)
#'
#' # Setup with unsupervised anomaly detection
#' setup_tabpfn(install_unsupervised = TRUE)
#' }
setup_tabpfn <- function(envname = "tabpfn", force = FALSE, install_shap = FALSE, install_unsupervised = FALSE, disable_analytics = TRUE) {

  # Disable analytics by default to avoid PostHog warnings
  if (disable_analytics) {
    Sys.setenv("DO_NOT_TRACK" = "1")
  }

  existing_envs <- reticulate::virtualenv_list()

  if (force || !envname %in% existing_envs) {
    message("Creating virtual environment: ", envname)
    reticulate::virtualenv_create(envname, python = NULL)
  }

  message("Using virtual environment: ", envname)
  reticulate::use_virtualenv(envname, required = TRUE)

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
