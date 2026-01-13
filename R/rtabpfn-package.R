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
  # Register model with parsnip
  parsnip::set_new_model("tab_pfn")
  parsnip::set_model_mode("tab_pfn", "classification")
  parsnip::set_model_mode("tab_pfn", "regression")
  parsnip::set_model_engine("tab_pfn", "classification", "tabpfn")
  parsnip::set_model_engine("tab_pfn", "regression", "tabpfn")
  
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
    device_name = NULL
  )

  tryCatch({
    if (reticulate::py_module_available("torch")) {
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
    }
  }, error = function(e) {
    warning("Error checking PyTorch GPU: ", e$message)
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
  
  if (!gpu_info$nvidia && !gpu_info$apple_silicon && !force_gpu) {
    message("No GPU detected. Installing CPU-only PyTorch...")
    reticulate::py_install("torch", envname = envname, pip = TRUE)
    return(invisible(NULL))
  }

  if (gpu_info$nvidia || force_gpu) {
    message("Installing PyTorch with CUDA support...")
    
    if (is.null(cuda_version)) {
      cuda_version <- "cu121"
      message("Using CUDA version: ", cuda_version, " (default)")
    }
    
    tryCatch({
      reticulate::py_install(paste0("torch==2.2.0+", cuda_version), 
                             envname = envname, pip = TRUE)
      message("PyTorch with CUDA installed successfully!")
    }, error = function(e) {
      warning("Failed to install PyTorch with CUDA: ", e$message)
      message("Falling back to CPU-only PyTorch...")
      reticulate::py_install("torch", envname = envname, pip = TRUE)
    })
  } else if (gpu_info$apple_silicon) {
    message("Installing PyTorch with MPS support for Apple Silicon...")
    reticulate::py_install("torch", envname = envname, pip = TRUE)
    message("PyTorch for Apple Silicon installed successfully!")
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
#' virtual environment in C:/venvs/tabpfn/ by default. Detects and configures GPU support.
#'
#' @param python_path Path to Python executable (e.g., "C:/venvs/tabpfn/Scripts/python.exe")
#' @param envname Name of the virtual environment (used only if python_path is NULL)
#' @param force Logical. If TRUE, recreates environment even if it exists
#' @param install_shap Logical. If TRUE, installs tabpfn-extensions for SHAP support
#' @param install_unsupervised Logical. If TRUE, installs tabpfn-extensions unsupervised module
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
                         disable_analytics = TRUE, setup_gpu = TRUE, 
                         force_gpu = FALSE, cuda_version = NULL) {

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
