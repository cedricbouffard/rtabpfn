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
                         envname = "r-tabpfn",
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
#' Sets up the Python environment for TabPFN usage.
#'
#' @param envname Name of the virtual environment
#' @param force Logical. If TRUE, recreates the environment even if it exists
#'
#' @return NULL (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' # Setup environment
#' setup_tabpfn()
#' }
setup_tabpfn <- function(envname = "r-tabpfn", force = FALSE) {

  existing_envs <- reticulate::virtualenv_list()

  if (force || !envname %in% existing_envs) {
    message("Creating virtual environment: ", envname)
    reticulate::virtualenv_create(envname, python = NULL)
  }

  message("Using virtual environment: ", envname)
  reticulate::use_virtualenv(envname, required = TRUE)

  # Check and install TabPFN
  check_tabpfn(install = TRUE, envname = envname)

  message("TabPFN environment ready!")
  invisible(NULL)
}
