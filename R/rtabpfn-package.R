#' @keywords internal

## usethis namespace: start
## usethis namespace: end

# Store the configured Python path as an R option
.tabpfn_options <- new.env(parent = emptyenv())
.tabpfn_options$python_path <- NULL
.tabpfn_options$libpython_warned <- FALSE

check_and_warn_libpython_mismatch <- function() {
  if (.tabpfn_options$libpython_warned) {
    return(invisible(NULL))
  }

  # Check if Python is already initialized without triggering initialization
  python_already_initialized <- tryCatch({
    reticulate::py_available()
  }, error = function(e) FALSE)

  if (!python_already_initialized) {
    return(invisible(NULL))
  }

  py_config <- tryCatch({
    reticulate::py_config()
  }, error = function(e) NULL)


  if (is.null(py_config) || is.null(py_config$python)) {
    return(invisible(NULL))
  }

  python_dir <- dirname(py_config$python)
  libpython_dir <- if (!is.null(py_config$libpython) && py_config$libpython != "") {
    dirname(py_config$libpython)
  } else {
    ""
  }

  is_venv <- grepl("\\.virtualenvs|virtualenvs|venvs", python_dir, ignore.case = TRUE)
  libpython_is_conda <- grepl("conda|miniconda|anaconda", libpython_dir, ignore.case = TRUE)

  if (is_venv && libpython_is_conda) {
    warning(
      "\n*** LIBPYTHON MISMATCH DETECTED ***\n",
      "Python is from virtualenv but libpython points to conda:\n",
      "  Python: ", py_config$python, "\n",
      "  Libpython: ", py_config$libpython, "\n\n",
      "This may cause errors when loading Python modules (torch, tabpfn, etc.)\n\n",
      "SOLUTIONS:\n",
      "1. Use conda environment:\n",
      "   reticulate::use_condaenv('tabpfn', required = TRUE)\n",
      "   setup_tabpfn()\n\n",
      "2. Recreate virtualenv (avoiding conda Python):\n",
      "   reticulate::virtualenv_remove('tabpfn')\n",
      "   setup_tabpfn()\n\n",
      "3. Set RETICULATE_PYTHON environment variable before starting R:\n",
      "   Sys.setenv(RETICULATE_PYTHON = '", py_config$python, "')\n",
      "   # Then restart R session\n\n",
      "4. Run diagnose_python_env() for detailed diagnosis\n"
    )
    .tabpfn_options$libpython_warned <- TRUE

  }


  invisible(NULL)
}

get_libpython_status <- function() {
  # Check if Python is already initialized
  is_initialized <- tryCatch({
    reticulate::py_available()
  }, error = function(e) FALSE)

  if (!is_initialized) {
    return(NULL)
  }

  py_config <- tryCatch({
    reticulate::py_config()
  }, error = function(e) NULL)


  if (is.null(py_config) || is.null(py_config$python)) {
    return(list(
      status = "unknown",
      python_path = NULL,
      libpython_path = NULL,
      is_mismatch = FALSE
    ))
  }

  python_dir <- dirname(py_config$python)
  libpython_dir <- if (!is.null(py_config$libpython) && py_config$libpython != "") {
    dirname(py_config$libpython)
  } else {
    ""
  }

  is_venv <- grepl("\\.virtualenvs|virtualenvs|venvs", python_dir, ignore.case = TRUE)
  libpython_is_conda <- grepl("conda|miniconda|anaconda", libpython_dir, ignore.case = TRUE)
  is_mismatch <- is_venv && libpython_is_conda

  list(
    status = if (is_mismatch) "mismatch" else "ok",
    python_path = py_config$python,
    libpython_path = py_config$libpython,
    python_dir = python_dir,
    libpython_dir = libpython_dir,
    is_mismatch = is_mismatch
  )
}

# Function to ensure the correct Python environment is active
ensure_python_env <- function() {
  # Check if there's a saved R option and restore it
  if (is.null(.tabpfn_options$python_path)) {
    saved_python_path <- getOption("rtabpfn.python_path")
    if (!is.null(saved_python_path) && file.exists(saved_python_path)) {
      .tabpfn_options$python_path <- saved_python_path
    }
  }

  if (!is.null(.tabpfn_options$python_path) && file.exists(.tabpfn_options$python_path)) {
    # Check if Python is already initialized
    is_initialized <- tryCatch({
      reticulate::py_available()
    }, error = function(e) FALSE)

    current_python <- NULL
    if (is_initialized) {
      current_python <- tryCatch({
        reticulate::py_config()$python
      }, error = function(e) NULL)
    }

    if (is.null(current_python) || !identical(normalizePath(current_python), normalizePath(.tabpfn_options$python_path))) {
      # Only try to configure if Python is not already initialized
      if (!is_initialized) {
        tryCatch({
          reticulate::use_python(.tabpfn_options$python_path, required = FALSE)
        }, error = function(e) {
          # Silent fail - will be retried when actually needed
        })
      }
    }
  }
}

# --- TabPFN license / token helpers ----------------------------------------

tabpfn_license_instructions <- function() {
  paste0(
    "\n*** TABPFN LICENSE REQUIRED ***\n",
    "TabPFN requires a one-time license acceptance before it can download\n",
    "the model weights for local inference.\n\n",
    "To fix this:\n",
    "  1. Open https://ux.priorlabs.ai in a browser and log in (or register)\n",
    "  2. Accept the license at https://ux.priorlabs.ai/account/licenses\n",
    "  3. Copy your API key from https://ux.priorlabs.ai/account\n",
    "  4. Set the TABPFN_TOKEN environment variable and restart R:\n",
    "       Windows:  setx TABPFN_TOKEN \"<your-api-key>\"\n",
    "       R:        Sys.setenv(TABPFN_TOKEN = \"<your-api-key>\")\n",
    "  5. If you already had a token, check that it is still valid (keys\n",
    "     expire) and that the license is accepted for the model version\n",
    "     you are using.\n",
    "\n"
  )
}

# Online status of a TabPFN license token.
# Returns one of: "ok", "missing", "invalid", "license", "unreachable".
tabpfn_token_status <- function(token = Sys.getenv("TABPFN_TOKEN"),
                                timeout_sec = 5,
                                license_version = "tabpfn-2.6-license-v1.0") {
  if (!nzchar(token)) {
    return("missing")
  }

  curl <- Sys.which("curl")
  if (!nzchar(curl)) {
    return("unreachable")
  }

  api_url <- "https://api.priorlabs.ai"

  http_get <- function(url, with_auth = TRUE) {
    # Note: the auth header must be passed via `--oauth2-bearer` because
    # system2() does not quote argument values containing spaces reliably
    # on Windows (e.g. "Authorization: Bearer <token>" would be split).
    # `-L` follows redirects, like the Python urllib client used by tabpfn.
    args <- c("-s", "-L", "-m", as.character(timeout_sec), "-w", "\n%{http_code}")
    if (with_auth) {
      args <- c(args, "--oauth2-bearer", token)
    }
    args <- c(args, url)
    out <- suppressWarnings(system2(curl, args, stdout = TRUE, stderr = TRUE))
    status <- if (length(out) > 0) trimws(out[length(out)]) else "000"
    body <- if (length(out) > 1) paste(out[-length(out)], collapse = "\n") else ""
    list(status = status, body = body)
  }

  # 1. Token validity
  r1 <- http_get(paste0(api_url, "/protected/"))
  if (!r1$status %in% c("200", "201", "204")) {
    if (r1$status %in% c("401", "403")) {
      return("invalid")
    }
    return("unreachable")
  }

  # 2. License acceptance
  r2 <- http_get(paste0(api_url, "/account/license/?version=",
                        utils::URLencode(license_version, reserved = TRUE)))
  if (r2$status == "200" &&
      grepl('"accepted"[[:space:]]*:[[:space:]]*true', r2$body)) {
    return("ok")
  }
  "license"
}

#' Check the TabPFN license token status
#'
#' @description
#' Checks whether the `TABPFN_TOKEN` environment variable is set and valid,
#' and whether the TabPFN license has been accepted for the account. Prints
#' instructions when the token is missing, invalid/expired, or the license
#' has not been accepted yet.
#'
#' This check also runs automatically when the package is loaded.
#'
#' @param verbose Logical. If TRUE (default), prints a status message.
#'
#' @return Invisibly, one of: "ok", "missing", "invalid", "license",
#'   "unreachable".
#' @export
#'
#' @examples
#' \dontrun{
#' check_tabpfn_license()
#' }
check_tabpfn_license <- function(verbose = TRUE) {
  status <- tabpfn_token_status()

  if (!verbose) {
    return(invisible(status))
  }

  if (status == "ok") {
    message("TabPFN license token: valid, license accepted.")
  } else if (status == "missing") {
    message(tabpfn_license_instructions())
  } else if (status == "invalid") {
    message(
      "\n*** TABPFN TOKEN INVALID OR EXPIRED ***\n",
      "The TABPFN_TOKEN API key was rejected by the Prior Labs server.\n",
      tabpfn_license_instructions()
    )
  } else if (status == "license") {
    message(
      "\n*** TABPFN LICENSE NOT ACCEPTED ***\n",
      "Your TABPFN_TOKEN is valid, but the license has not been accepted\n",
      "for this account (or the token cannot access the license endpoint).\n",
      "Open https://ux.priorlabs.ai/account/licenses and accept the license,\n",
      "then retry.\n"
    )
  }
  # status == "unreachable": nothing to report (offline / no curl)

  invisible(status)
}


# Auto-configure environment on package load
.onLoad <- function(libname, pkgname) {
  # Register model with parsnip
  parsnip::set_new_model("tab_pfn")
  parsnip::set_model_mode("tab_pfn", "classification")
  parsnip::set_model_mode("tab_pfn", "regression")
  parsnip::set_model_engine("tab_pfn", "classification", "tabpfn")
  parsnip::set_model_engine("tab_pfn", "regression", "tabpfn")
  parsnip::set_encoding(
    model = "tab_pfn",
    eng = "tabpfn",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_encoding(
    model = "tab_pfn",
    eng = "tabpfn",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  # Register time series model with parsnip
  parsnip::set_new_model("tab_pfn_ts")
  parsnip::set_model_mode("tab_pfn_ts", "regression")
  parsnip::set_model_engine("tab_pfn_ts", "regression", "tabpfn_ts")
  parsnip::set_encoding(
    model = "tab_pfn_ts",
    eng = "tabpfn_ts",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  # Register TabICL model with parsnip
  parsnip::set_new_model("tab_icl")
  parsnip::set_model_mode("tab_icl", "classification")
  parsnip::set_model_mode("tab_icl", "regression")
  parsnip::set_model_engine("tab_icl", "classification", "tabicl")
  parsnip::set_model_engine("tab_icl", "regression", "tabicl")
  parsnip::set_encoding(
    model = "tab_icl",
    eng = "tabicl",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_encoding(
    model = "tab_icl",
    eng = "tabicl",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  # Restore saved Python path from R option
  saved_python_path <- getOption("rtabpfn.python_path")
  if (!is.null(saved_python_path) && file.exists(saved_python_path)) {
    .tabpfn_options$python_path <- saved_python_path
  }

  # Check if Python is already initialized or configured by the user
  reticulate_python_env <- Sys.getenv("RETICULATE_PYTHON")
  python_configured <- !is.null(.tabpfn_options$python_path) ||
                       (reticulate_python_env != "") ||
                       tryCatch({ reticulate::py_available() }, error = function(e) FALSE)

  if (python_configured) {
    return(invisible(NULL))
  }

  # Auto-detect best TabPFN venv (prioritizing ones with tabpfn-time-series)

  auto_detect_best_tabpfn_env <- function() {
    # Get user's home and documents directory safely
    home_dir <- path.expand("~")
    
    venv_paths <- c(
      file.path(home_dir, ".virtualenvs/tabpfn/Scripts/python.exe"),
      file.path(home_dir, "Documents/.virtualenvs/tabpfn/Scripts/python.exe"),
      "C:/venvs/tabpfn/Scripts/python.exe",
      "~/.virtualenvs/tabpfn/bin/python",
      "~/Documents/.virtualenvs/tabpfn/bin/python"
    )

    for (venv_path in venv_paths) {
      expanded_path <- path.expand(venv_path)
      if (file.exists(expanded_path)) {
        venv_dir <- dirname(expanded_path)
        # Check for site-packages to see if tabpfn-time-series is installed
        # Handles both Windows (Lib/site-packages) and Unix (lib/pythonX.Y/site-packages)
        lib_dir <- if (grepl("Scripts", expanded_path)) {
          file.path(venv_dir, "../Lib/site-packages")
        } else {
          # Try to find lib directory in Unix-style
          parent <- dirname(venv_dir)
          libs <- list.dirs(file.path(parent, "lib"), recursive = FALSE)
          if (length(libs) > 0) file.path(libs[1], "site-packages") else NULL
        }
        
        if (!is.null(lib_dir) && dir.exists(lib_dir)) {
          pkg_dirs <- list.dirs(lib_dir, full.names = FALSE, recursive = FALSE)
          if ("tabpfn_time_series" %in% pkg_dirs) {
            return(expanded_path)
          }
        }
      }
    }
    return(NULL)
  }

  # Fallback: detect any TabPFN venv
  auto_detect_any_tabpfn_env <- function() {
    home_dir <- path.expand("~")
    
    venv_paths <- c(
      file.path(home_dir, ".virtualenvs/tabpfn/Scripts/python.exe"),
      file.path(home_dir, "Documents/.virtualenvs/tabpfn/Scripts/python.exe"),
      "C:/venvs/tabpfn/Scripts/python.exe",
      "~/.virtualenvs/tabpfn/bin/python",
      "~/Documents/.virtualenvs/tabpfn/bin/python"
    )

    for (venv_path in venv_paths) {
      expanded_path <- path.expand(venv_path)
      if (file.exists(expanded_path)) {
        return(expanded_path)
      }
    }
    return(NULL)
  }

  if (is.null(.tabpfn_options$python_path)) {
    # First try to find venv with tabpfn-time-series
    best_path <- auto_detect_best_tabpfn_env()
    if (!is.null(best_path)) {
      .tabpfn_options$python_path <- best_path
      tryCatch({
        reticulate::use_python(best_path, required = FALSE)
        message("Auto-detected TabPFN Python environment at: ", best_path)
      }, error = function(e) {
        # Silent fail during package load
      })
    } else {
      # Fallback to any TabPFN venv
      any_path <- auto_detect_any_tabpfn_env()
      if (!is.null(any_path)) {
        .tabpfn_options$python_path <- any_path
        tryCatch({
          reticulate::use_python(any_path, required = FALSE)
          message("Auto-detected TabPFN Python environment at: ", any_path)
          message("Note: tabpfn-time-series not found in this environment")
          message("Install with: setup_tabpfn(install_time_series = TRUE)")
        }, error = function(e) {
          # Silent fail during package load
        })
      }
    }
  }

# Don't call ensure_python_env() during package load to avoid conflicts
  # It will be called when actually needed (e.g., when calling TabPFN functions)
  # ensure_python_env()

  # Check the TabPFN license token once per session and print instructions
  # if it is missing, invalid/expired, or the license is not accepted.
  if (is.null(.tabpfn_options$license_checked)) {
    .tabpfn_options$license_checked <- TRUE
    check_tabpfn_license(verbose = TRUE)
  }
}

#' Check GPU Availability
#'
#' @description
#' Check if a GPU is available on the system
#'
#' @return List with GPU detection information (nvidia, amd, apple_silicon, device)
#' @export
#'
#' @examples
#' \dontrun{
#' # Check if GPU is available
#' check_gpu_available()
#' }
check_gpu_available <- function() {
  gpu_info <- list(
    nvidia = FALSE,
    amd = FALSE,
    apple_silicon = FALSE,
    device = "cpu"
  )

  os <- Sys.info()["sysname"]

  if (os == "Windows") {
    tryCatch({
      if (Sys.which("nvidia-smi") != "") {
        gpu_info$nvidia <- TRUE
        gpu_info$device <- "cuda"
        message("Detected NVIDIA GPU via nvidia-smi")
      }
    }, error = function(e) NULL)
  } else if (os == "Darwin") {
    tryCatch({
      arch <- system("uname -m", intern = TRUE)
      if (arch == "arm64") {
        gpu_info$apple_silicon <- TRUE
        gpu_info$device <- "mps"
        message("Detected Apple Silicon GPU")
      }
    }, error = function(e) NULL)
  } else if (os == "Linux") {
    tryCatch({
      if (Sys.which("nvidia-smi") != "") {
        gpu_info$nvidia <- TRUE
        gpu_info$device <- "cuda"
        message("Detected NVIDIA GPU via nvidia-smi")
      } else if (Sys.which("rocm-smi") != "") {
        gpu_info$amd <- TRUE
        gpu_info$device <- "rocm"
        message("Detected AMD GPU via rocm-smi")
      }
    }, error = function(e) NULL)
  }

  invisible(gpu_info)
}


#' Check PyTorch GPU Status
#'
#' @description
#' Check if PyTorch is using GPU
#'
#' @return List with torch GPU status information (torch_available, cuda_available, cuda_version, device_count, device_name)
#' @export
#'
#' @examples
#' \dontrun{
#' # Check if torch is using GPU
#' check_torch_gpu()
#' }
check_torch_gpu <- function() {
  result <- list(
    torch_available = FALSE,
    cuda_available = FALSE,
    cuda_version = NULL,
    device_count = 0,
    device_name = NULL,
    error = NULL
  )

  tryCatch({
    # First check if torch module is available
    if (!reticulate::py_module_available("torch")) {
      cat("PyTorch is not available in current Python environment.\n")

      # Provide diagnostic info
      py_config <- tryCatch({
        reticulate::py_config()
      }, error = function(e) NULL)

      if (!is.null(py_config)) {
        cat("  Python:", py_config$python, "\n")
        if (!is.null(py_config$libpython) && py_config$libpython != "") {
          cat("  Libpython:", py_config$libpython, "\n")

          # Check for common mismatch
          python_dir <- dirname(py_config$python)
          libpython_dir <- dirname(py_config$libpython)
          is_venv <- grepl("\\.virtualenvs|virtualenvs|venvs", python_dir, ignore.case = TRUE)
          libpython_is_conda <- grepl("conda|miniconda|anaconda", libpython_dir, ignore.case = TRUE)

          if (is_venv && libpython_is_conda) {
            cat("\n*** Possible cause: libpython mismatch ***\n")
            cat("Run diagnose_python_env() for details.\n")
            result$error <- "libpython_mismatch"
          }
        }
      }
      return(invisible(result))
    }

    # Try to import torch
    torch <- reticulate::import("torch")
    result$torch_available <- TRUE
    result$cuda_available <- torch$cuda$is_available()

    if (result$cuda_available) {
      result$cuda_version <- torch$version$cuda
      result$device_count <- torch$cuda$device_count()
      if (result$device_count > 0) {
        result$device_name <- as.character(torch$cuda$get_device_name(0L))
      }
    }

    cat("PyTorch CUDA Available:", result$cuda_available, "\n")
    if (result$cuda_available) {
      cat("CUDA Version:", result$cuda_version, "\n")
      cat("Device Count:", result$device_count, "\n")
      if (!is.null(result$device_name)) {
        cat("Device:", result$device_name, "\n")
      }
    }
  }, error = function(e) {
    warning("Error checking PyTorch GPU: ", e$message)
    result$error <- e$message
  })

  invisible(result)
}


#' Setup PyTorch with GPU Support
#'
#' @description
#' Setup PyTorch with correct GPU support
#'
#' @param envname Name of the virtual environment
#' @param force_gpu If TRUE, forces GPU installation even if not detected
#' @param cuda_version CUDA version to install (default: NULL for auto-detect)
#'
#' @return NULL (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' # Setup torch with GPU support
#' setup_torch()
#'
#' # Force GPU installation
#' setup_torch(force_gpu = TRUE)
#' }
setup_torch <- function(envname = "tabpfn", force_gpu = FALSE, cuda_version = NULL) {
  gpu_info <- check_gpu_available()

  check_and_warn_libpython_mismatch()

  if (!gpu_info$nvidia && !gpu_info$apple_silicon && !force_gpu) {
    message("No GPU detected. Installing CPU-only PyTorch...")
    reticulate::py_install("torch", envname = envname, pip = TRUE)

    # Verify torch installation
    Sys.sleep(2)
    torch_check <- check_torch_gpu()
    if (!torch_check$torch_available && !is.null(torch_check$error)) {
      warning("Torch installed but cannot be loaded. ", torch_check$error)
      message("\nTry running diagnose_python_env() for troubleshooting.")
    }
    return(invisible(NULL))
  }

  if (gpu_info$nvidia || force_gpu) {
      message("Installing PyTorch with CUDA support...")

      # Check if torch is already installed with CUDA
      tryCatch({
        if (reticulate::py_module_available("torch")) {
          torch <- reticulate::import("torch")
          if (torch$cuda$is_available()) {
            message("PyTorch with CUDA is already installed!")
            message("CUDA Version: ", torch$version$cuda)
            return(invisible(NULL))
          }
        }
      }, error = function(e) NULL)

      if (is.null(cuda_version)) {
        cuda_version <- "cu124"
        message("Using CUDA version: ", cuda_version, " (default)")
      }

      tryCatch({
        # Modern PyTorch installation with index URL
        message("Installing PyTorch from PyTorch index...")
        reticulate::py_install("torch",
                              envname = envname,
                              pip = TRUE,
                              index_url = paste0("https://download.pytorch.org/whl/", cuda_version))
        message("PyTorch with CUDA installed successfully!")
      }, error = function(e) {
        warning("Failed to install PyTorch with CUDA via index: ", e$message)
        message("Trying alternative method...")

        # Fallback: install torch normally, it should auto-detect CUDA
        tryCatch({
          reticulate::py_install("torch", envname = envname, pip = TRUE)
          message("PyTorch installed (will use CUDA if available)")

          # Verify CUDA is working
          torch <- reticulate::import("torch")
          if (torch$cuda$is_available()) {
            message("CUDA detected and working!")
          } else {
            message("Note: CUDA not available, using CPU")
          }
        }, error = function(e2) {
          warning("Failed to install PyTorch: ", e2$message)
          message("Falling back to CPU-only PyTorch...")
          reticulate::py_install("torch", envname = envname, pip = TRUE)
        })
      })
    } else if (gpu_info$apple_silicon) {
    message("Installing PyTorch with MPS support for Apple Silicon...")
    reticulate::py_install("torch", envname = envname, pip = TRUE)
    message("PyTorch for Apple Silicon installed successfully!")
  }

  # Final verification
  Sys.sleep(1)
  torch_status <- check_torch_gpu()

  if (!torch_status$torch_available) {
    if (!is.null(torch_status$error) && torch_status$error == "libpython_mismatch") {
      message("\n*** Torch cannot be loaded due to libpython mismatch ***")
      message("Run diagnose_python_env() for solutions")
    } else {
      message("\nWarning: Torch installed but cannot be loaded")
    }
  }

  invisible(NULL)
}


#' Check TabPFN Installation
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

    # Verify installation
    # Use py_module_available which is safer than direct import
    has_tabpfn <- reticulate::py_module_available("tabpfn")

    if (!has_tabpfn) {
      # Sometimes py_module_available fails due to libpython mismatch even if installed
      # Try a more direct check using pip
      python_path <- tryCatch({
        if (envname %in% reticulate::virtualenv_list()) {
          reticulate::virtualenv_python(envname)
        } else {
          NULL
        }
      }, error = function(e) NULL)

      if (!is.null(python_path)) {
        check_cmd <- paste0("\"", python_path, "\" -m pip show tabpfn")
        is_installed_pip <- tryCatch({
          suppressWarnings(system(check_cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE) == 0)
        }, error = function(e) FALSE)

        if (is_installed_pip) {
          message("TabPFN is installed in the virtual environment but could not be loaded by R.")
          message("This is likely due to the libpython mismatch detected earlier.")
          has_tabpfn <- TRUE
        }
      }
    }

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
#' virtual environment in C:/venvs/tabpfn/ by default. Detects and configures GPU support.
#'
#' @param python_path Path to Python executable (e.g., "C:/venvs/tabpfn/Scripts/python.exe")
#' @param envname Name of the virtual environment (used only if python_path is NULL)
#' @param force Logical. If TRUE, recreates environment even if it exists
#' @param install_shap Logical. If TRUE, installs tabpfn-extensions for SHAP support
#' @param install_unsupervised Logical. If TRUE, installs tabpfn-extensions unsupervised module
#' @param install_time_series Logical. If TRUE, installs tabpfn-time-series for forecasting
#' @param disable_analytics Logical. If TRUE, disables PostHog analytics (default: TRUE)
#' @param setup_gpu Logical. If TRUE, attempts to setup GPU support (default: TRUE)
#' @param force_gpu Logical. If TRUE, forces GPU installation even if not detected
#' @param cuda_version CUDA version to install (default: NULL for auto-detect)
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
#'
#' # Setup without GPU
#' setup_tabpfn(setup_gpu = FALSE)
#'
#' # Force GPU installation
#' setup_tabpfn(force_gpu = TRUE)
#' }
setup_tabpfn <- function(python_path = NULL, envname = "tabpfn", force = FALSE,
                          install_shap = FALSE, install_unsupervised = FALSE,
                          install_time_series = FALSE,
                          disable_analytics = TRUE, setup_gpu = TRUE,
                          force_gpu = FALSE, cuda_version = NULL) {

  # Disable browser opening for license acceptance (TabPFN v3+)
  Sys.setenv("TABPFN_NO_BROWSER" = "true")

  # Disable analytics by default to avoid PostHog warnings
  if (disable_analytics) {
    Sys.setenv("DO_NOT_TRACK" = "1")
  }

  check_and_warn_libpython_mismatch()

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

    # Check if Python is already initialized
    python_already_initialized <- tryCatch({
      reticulate::py_available()
    }, error = function(e) FALSE)

    if (python_already_initialized) {
      # Check which Python is currently being used
      current_python <- tryCatch({
        reticulate::py_config()$python
      }, error = function(e) NULL)

      if (!is.null(current_python) && normalizePath(current_python) == normalizePath(python_path)) {
        message("Python environment already configured correctly.")
        .tabpfn_options$python_path <- python_path
        options(rtabpfn.python_path = python_path)
      } else {
        # Python is initialized with a different version
        warning("Python has already been initialized with a different version.")
        message("Current Python: ", current_python)
        message("Requested Python: ", python_path)
        message("\nNote: Python cannot be re-initialized in the same R session.")
        message("Please restart R and run setup_tabpfn() again, or")
        message("unset the RETICULATE_PYTHON environment variable before starting R.")
        message("\nContinuing with currently initialized Python...")

        # Use the currently initialized Python
        .tabpfn_options$python_path <- current_python
        options(rtabpfn.python_path = current_python)
      }
    } else {
      # Python not yet initialized, safe to use
      tryCatch({
        reticulate::use_python(python_path, required = TRUE)
        .tabpfn_options$python_path <- python_path
        options(rtabpfn.python_path = python_path)
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
    }
  } else {
    # Use environment by name
    is_conda <- FALSE
    tryCatch({
      conda_envs <- reticulate::conda_list()
      if (!is.null(conda_envs) && envname %in% conda_envs$name) {
        is_conda <- TRUE
      }
    }, error = function(e) NULL)

    if (is_conda) {
      message("Using existing conda environment: ", envname)
      reticulate::use_condaenv(envname, required = TRUE)
    } else {
      # Use virtual environment by name
      existing_envs <- reticulate::virtualenv_list()

      if (force || !envname %in% existing_envs) {
        message("Creating virtual environment: ", envname)
        reticulate::virtualenv_create(envname, python = NULL)
      }
    }

    # Check if Python is already initialized
    python_already_initialized <- tryCatch({
      reticulate::py_available()
    }, error = function(e) FALSE)

    if (python_already_initialized) {
      # Check which Python is currently being used
      current_python <- tryCatch({
        reticulate::py_config()$python
      }, error = function(e) NULL)

      target_python <- tryCatch({
        reticulate::virtualenv_python(envname)
      }, error = function(e) NULL)

      if (!is.null(current_python) && !is.null(target_python) &&
          normalizePath(current_python) == normalizePath(target_python)) {
        message("Python environment '", envname, "' already configured correctly.")
        .tabpfn_options$python_path <- target_python
        options(rtabpfn.python_path = target_python)
      } else {
        # Python is initialized with a different version
        warning("Python has already been initialized with a different version.")
        message("Current Python: ", current_python)
        message("Requested Environment: ", envname, " (", target_python, ")")
        message("\nNote: Python cannot be re-initialized in the same R session.")
        message("Please restart R and run setup_tabpfn() again, or")
        message("unset the RETICULATE_PYTHON environment variable before starting R.")
        message("\nContinuing with currently initialized Python...")

        # Use the currently initialized Python
        .tabpfn_options$python_path <- current_python
        options(rtabpfn.python_path = current_python)
      }
    } else {
      # Python not yet initialized, safe to use
      message("Using virtual environment: ", envname)
      tryCatch({
        reticulate::use_virtualenv(envname, required = FALSE)
        # Store the Python path from the virtualenv
        .tabpfn_options$python_path <- reticulate::py_config()$python
        options(rtabpfn.python_path = .tabpfn_options$python_path)
      }, error = function(e) {
        warning("Failed to initialize virtual environment '", envname, "': ", e$message)
      })
    }
  }


  # Setup PyTorch with GPU support if requested
  if (setup_gpu) {
    message("\nChecking GPU configuration...")
    gpu_info <- check_gpu_available()

    if (gpu_info$nvidia) {
      message("NVIDIA GPU detected, configuring PyTorch with CUDA support...")
    } else if (gpu_info$apple_silicon) {
      message("Apple Silicon detected, configuring PyTorch with MPS support...")
    } else {
      message("No GPU detected, using CPU-only PyTorch...")
    }

    setup_torch(envname = envname, force_gpu = force_gpu, cuda_version = cuda_version)

    # Verify torch GPU setup
    torch_status <- check_torch_gpu()
    if (!torch_status$torch_available) {
      warning("PyTorch not properly installed. TabPFN may not work correctly.")
    }
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

  # Optionally install tabpfn-time-series for forecasting
  if (install_time_series) {
    has_ts <- reticulate::py_module_available("tabpfn_time_series")

    if (!has_ts) {
      message("Installing tabpfn-time-series for time series forecasting...")
      tryCatch({
        reticulate::py_install("tabpfn-time-series", envname = envname, pip = TRUE)
        message("tabpfn-time-series installed successfully!")
      }, error = function(e) {
        warning("Failed to install tabpfn-time-series: ", e$message)
        message("You can install it manually with: pip install tabpfn-time-series")
      })
    } else {
      message("tabpfn-time-series already installed.")
    }
  }

  message("TabPFN environment ready!")

  # Final check for libpython mismatch to give a concluding advice
  status <- get_libpython_status()
  if (!is.null(status) && status$is_mismatch) {
    message("\n*** NOTE: A Python configuration mismatch was detected ***")
    message("If you encounter errors when using TabPFN, please try setting:")
    message("Sys.setenv(RETICULATE_PYTHON = '", status$python_path, "')")
    message("in your .Rprofile or before loading the package.")
  }

  invisible(NULL)
}



#' Diagnose Python Environment Configuration
#'
#' @description
#' Diagnoses potential issues with Python environment configuration,
#' particularly libpython mismatches between virtualenv and conda.
#'
#' @return List with diagnostic information and recommendations
#' @export
#'
#' @examples
#' \dontrun{
#' diagnose_python_env()
#' }
diagnose_python_env <- function() {
  results <- list(
    status = "unknown",
    issues = character(),
    recommendations = character(),
    python_path = NULL,
    libpython_path = NULL,
    pythonhome = NULL
  )

  cat("=== Python Environment Diagnosis ===\n\n")

  # Check Python configuration
  py_config <- tryCatch({
    reticulate::py_config()
  }, error = function(e) {
    cat("Error getting Python config:", e$message, "\n")
    NULL
  })

  if (is.null(py_config)) {
    results$status <- "error"
    results$issues <- c(results$issues, "Cannot get Python configuration")
    results$recommendations <- c(results$recommendations,
      "Try restarting R session and running setup_tabpfn() again")
    cat("\n", results$recommendations[1], "\n")
    return(invisible(results))
  }

  results$python_path <- py_config$python
  results$libpython_path <- py_config$libpython
  results$pythonhome <- py_config$pythonhome

  cat("Python path:", results$python_path, "\n")
  cat("Libpython:", results$libpython_path, "\n")
  cat("Pythonhome:", results$pythonhome, "\n\n")

  # Detect libpython mismatch
  python_dir <- dirname(results$python_path)
  is_venv <- grepl("\\.virtualenvs|virtualenvs|venvs", python_dir, ignore.case = TRUE)
  is_conda <- grepl("conda|miniconda|anaconda", python_dir, ignore.case = TRUE)

  if (!is.null(results$libpython_path) && results$libpython_path != "") {
    libpython_dir <- dirname(results$libpython_path)
    libpython_is_conda <- grepl("conda|miniconda|anaconda", libpython_dir, ignore.case = TRUE)

    # Check for mismatch
    if (is_venv && libpython_is_conda) {
      issue <- paste0("MISMATCH: Python is from virtualenv but libpython is from conda.\n",
                      "  Python: ", results$python_path, "\n",
                      "  Libpython: ", results$libpython_path)
      results$issues <- c(results$issues, issue)

      cat("*** ISSUE DETECTED ***\n")
      cat("Python is from virtualenv but libpython points to conda!\n\n")

      results$recommendations <- c(results$recommendations,
        "SOLUTION 1: Use conda environment instead of virtualenv:",
        "  reticulate::use_condaenv('tabpfn', required = TRUE)",
        "  setup_tabpfn()",
        "",
        "SOLUTION 2: Create virtualenv with explicit Python (not conda):",
        "  reticulate::virtualenv_remove('tabpfn')",
        "  # Use Python from Python install, not miniconda",
        "  reticulate::virtualenv_create('tabpfn', python = 'C:/path/to/python.exe')",
        "  setup_tabpfn()",
        "",
        "SOLUTION 3: Set environment variable before R session:",
        "  Sys.setenv(RETICULATE_PYTHON = 'C:/Users/.../.virtualenvs/tabpfn/Scripts/python.exe')",
        "  # Then restart R and run setup_tabpfn()")

      for (rec in results$recommendations) {
        cat(rec, "\n")
      }
      results$status <- "mismatch"
    } else if (is_conda && !libpython_is_conda) {
      issue <- paste0("MISMATCH: Python is from conda but libpython is from elsewhere.\n",
                      "  Python: ", results$python_path, "\n",
                      "  Libpython: ", results$libpython_path)
      results$issues <- c(results$issues, issue)
      results$status <- "mismatch"
    } else {
      cat("Python and libpython appear to be from the same source.\n")
      results$status <- "ok"
    }
  } else {
    cat("Warning: libpython path is empty or NULL\n")
    results$status <- "warning"
  }

  # Check if torch is available
  cat("\n--- PyTorch Status ---\n")
  has_torch <- reticulate::py_module_available("torch")
  cat("PyTorch available:", has_torch, "\n")

  if (has_torch) {
    tryCatch({
      torch <- reticulate::import("torch")
      cat("PyTorch version:", torch$`__version__`, "\n")
      cat("CUDA available:", torch$cuda$is_available(), "\n")
    }, error = function(e) {
      cat("Error checking torch:", e$message, "\n")
    })
  }

  # Check RETICULATE_PYTHON env var
  cat("\n--- Environment Variables ---\n")
  reticulate_python <- Sys.getenv("RETICULATE_PYTHON")
  if (reticulate_python != "") {
    cat("RETICULATE_PYTHON:", reticulate_python, "\n")
    cat("Note: This environment variable forces a specific Python.\n")
    results$recommendations <- c(results$recommendations,
      "To change Python, unset RETICULATE_PYTHON or restart R")
  } else {
    cat("RETICULATE_PYTHON: (not set)\n")
  }

  cat("\n=== Diagnosis Complete ===\n")
  cat("Status:", results$status, "\n")

  invisible(results)
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
  py_config <- NULL
  tryCatch({
    py_config <- reticulate::py_config()
    cat("   Python path:", py_config$python, "\n")
    cat("   Version:", py_config$version_string, "\n")
    cat("   Libpython:", py_config$libpython, "\n")
    results$python <- list(available = TRUE, path = py_config$python, version = py_config$version_string)
  }, error = function(e) {
    cat("   Error: ", e$message, "\n")
    results$python <- list(available = FALSE, error = e$message)
  })

  # Check for libpython mismatch
  if (!is.null(py_config) && !is.null(py_config$libpython) && py_config$libpython != "") {
    python_dir <- dirname(py_config$python)
    libpython_dir <- dirname(py_config$libpython)
    is_venv <- grepl("\\.virtualenvs|virtualenvs|venvs", python_dir, ignore.case = TRUE)
    libpython_is_conda <- grepl("conda|miniconda|anaconda", libpython_dir, ignore.case = TRUE)

    cat("\n   Libpython Check:")
    if (is_venv && libpython_is_conda) {
      cat(" *** MISMATCH DETECTED ***\n")
      cat("   Python is from virtualenv but libpython is from conda!\n")
      cat("   This may cause module loading errors.\n")
      cat("   Run diagnose_python_env() for solutions.\n")
      results$python$libpython_mismatch <- TRUE
    } else {
      cat(" OK (compatible)\n")
      results$python$libpython_mismatch <- FALSE
    }
  }

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

  cat("\n")

  # Check tabpfn-time-series
  cat("6. TabPFN Time Series:\n")
  has_ts <- check_time_series_available()
  cat("   Available:", has_ts, "\n")
  results$time_series <- list(available = has_ts)

  if (has_ts) {
    tryCatch({
      ts_module <- reticulate::import("tabpfn_time_series")
      cat("   Version:", ts_module$`__version__`, "\n")
      results$time_series$version <- as.character(ts_module$`__version__`)
    }, error = function(e) {
      cat("   Warning: Could not get version\n")
    })
  }

  cat("\n=== Validation Complete ===\n")

  invisible(results)
}
