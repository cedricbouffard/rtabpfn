#' @name tab_pfn
#' @title TabPFN Model for tidymodels
#' @description A TabPFN (Tabular Prior-Fitted Network) model that can be used
#'   with the tidymodels ecosystem (parsnip, workflows, tune, etc.)

#' Get encoding for tab_pfn model
#'
#' @return A tibble with encoding information
#' @keywords internal
get_encoding.tab_pfn <- function() {
  tibble::tibble(
    model = c("tab_pfn", "tab_pfn"),
    engine = c("tabpfn", "tabpfn"),
    mode = c("classification", "regression"),
    predictor_indicators = c("none", "none"),
    compute_intercept = c(FALSE, FALSE),
    remove_intercept = c(FALSE, FALSE),
    allow_sparse_x = c(FALSE, FALSE)
  )
}

NULL

#' TabPFN model specification
#'
#' @param mode A single character string for the prediction mode: "classification" or "regression"
#' @param engine A single character string specifying the computational engine. For TabPFN, this is always "tabpfn"
#' @param N_ensemble_configurations Number of ensemble configurations (integer)
#' @param max_len_feature_basis Maximum length of feature basis (integer)
#' @param device Device to use: "auto", "cpu", or "cuda"
#' @param ... Additional engine-specific arguments
#'
#' @return A model specification object
#' @export
#'
#' @examples
#' \dontrun{
#' library(tidymodels)
#'
#' # Regression model
#' tab_pfn_reg_spec <- tab_pfn(mode = "regression") %>%
#'   set_engine("tabpfn") %>%
#'   fit(mpg ~ ., data = mtcars)
#'
#' # Classification model
#' tab_pfn_cls_spec <- tab_pfn(mode = "classification") %>%
#'   set_engine("tabpfn") %>%
#'   fit(Species ~ ., data = iris)
#' }
tab_pfn <- function(
    mode = "unknown",
    engine = "tabpfn",
    N_ensemble_configurations = 8,
    max_len_feature_basis = 1024,
    device = "auto",
    ...
) {
  args <- list(
    N_ensemble_configurations = rlang::enquo(N_ensemble_configurations),
    max_len_feature_basis = rlang::enquo(max_len_feature_basis),
    device = rlang::enquo(device),
    ...
  )

  result <- parsnip::new_model_spec(
    cls = "tab_pfn",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = "tab_pfn",
    engine = engine
  )
  
  result$encoding <- list(
    predictors = "numeric",
    outcomes = if (mode == "classification") {
      list(type = "factor", levels = NULL)
    } else if (mode == "regression") {
      "numeric"
    } else {
      "unknown"
    }
  )
  
  result
}

#' @export
#' @rdname tab_pfn
update.tab_pfn <- function(object, parameters = NULL, N_ensemble_configurations = NULL,
                            max_len_feature_basis = NULL, device = NULL, fresh = FALSE, ...) {
  parsnip::update_dot_check(...)

  if (fresh) {
    object$args <- list(
      N_ensemble_configurations = rlang::enquo(N_ensemble_configurations),
      max_len_feature_basis = rlang::enquo(max_len_feature_basis),
      device = rlang::enquo(device)
    )
  } else {
    if (!is.null(N_ensemble_configurations)) {
      object$args$N_ensemble_configurations <- rlang::enquo(N_ensemble_configurations)
    }
    if (!is.null(max_len_feature_basis)) {
      object$args$max_len_feature_basis <- rlang::enquo(max_len_feature_basis)
    }
    if (!is.null(device)) {
      object$args$device <- rlang::enquo(device)
    }
    object$args <- c(object$args, parameters)
  }

  result <- parsnip::new_model_spec(
    cls = "tab_pfn",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = object$method,
    engine = object$engine
  )
  
  result$encoding <- list(
    predictors = "numeric",
    outcomes = if (result$mode == "classification") {
      list(type = "factor", levels = NULL)
    } else if (result$mode == "regression") {
      "numeric"
    } else {
      "unknown"
    }
  )
  
  result
}

#' Set the model engine for TabPFN
#'
#' @param x A model specification
#' @param eng A single character string for the engine (always "tabpfn" for this model)
#' @param ... Additional engine-specific arguments
#'
#' @return An updated model specification
#' @export
set_engine.tab_pfn <- function(x, eng = c("tabpfn"), ...) {
  eng <- match.arg(eng)
  x$engine <- eng
  x$eng_args <- list(...)
  x
}

#' Set mode for TabPFN
#'
#' @param object A model specification
#' @param mode A single character string for prediction mode
#' @param ... Not used
#'
#' @return An updated model specification
#' @export
set_mode.tab_pfn <- function(object, mode, ...) {
  if (mode == "regression") {
    object$mode <- "regression"
  } else if (mode == "classification") {
    object$mode <- "classification"
  } else {
    stop("`mode` should be either 'regression' or 'classification'", call. = FALSE)
  }
  
  object$encoding <- list(
    predictors = "numeric",
    outcomes = if (object$mode == "classification") {
      list(type = "factor", levels = NULL)
    } else {
      "numeric"
    }
  )
  
  object
}

#' Check that the required arguments are available for fitting
#'
#' @param object A model specification
#' @param x A data frame or matrix of predictors
#' @return TRUE
#' @keywords internal
required_pkgs.tab_pfn <- function(object, ...) {
  c("rtabpfn", "reticulate", "tibble")
}

#' Fit a TabPFN model
#'
#' @param x A model specification
#' @param formula A formula specifying the model
#' @param data A data frame
#' @param control A `parsnip::control_fit()` object
#' @param ... Additional arguments
#'
#' @return A fitted model object
#' @export
#' @keywords internal
fit.tab_pfn <- function(x, formula = NULL, data = NULL, control = parsnip::control_fit(), ...) {
  rtabpfn:::ensure_python_env()

  # Process the data using hardhat - support both formula and xy interfaces
  if (!is.null(formula)) {
    mold <- hardhat::mold(formula, data)
    x_train <- mold$predictors
    y_train <- mold$outcomes[[1]]
  } else {
    # XY interface
    mold <- hardhat::mold(data[[1]], data[[2]])
    x_train <- mold$predictors
    y_train <- mold$outcomes[[1]]
  }

  # Get model arguments
  args <- x$args

  # Convert to simple vectors if needed
  N_ensemble_configurations <- rlang::eval_tidy(args$N_ensemble_configurations)
  max_len_feature_basis <- rlang::eval_tidy(args$max_len_feature_basis)
  device <- rlang::eval_tidy(args$device)

  # Suppress PostHog analytics warnings
  old_do_not_track <- Sys.getenv("DO_NOT_TRACK")
  Sys.setenv("DO_NOT_TRACK" = "1")

  tryCatch({
    if (x$mode == "classification") {
      # Fit classification model
      model <- rtabpfn::tab_pfn_classification(
        X = x_train,
        y = y_train,
        device = device
      )

      class(model) <- c("tab_pfn")

    } else if (x$mode == "regression") {
      # Fit regression model
      model <- rtabpfn::tab_pfn_regression(
        X = x_train,
        y = y_train,
        device = device
      )

      class(model) <- c("tab_pfn")
    } else {
      stop("TabPFN mode must be 'classification' or 'regression'")
    }

  }, error = function(e) {
    stop("Error fitting TabPFN model: ", e$message)
  }, finally = {
    # Restore original environment variable
    if (old_do_not_track == "") {
      Sys.unsetenv("DO_NOT_TRACK")
    } else {
      Sys.setenv("DO_NOT_TRACK" = old_do_not_track)
    }
  })

  # Create the parsnip fit object
  fit <- list(
    model = model,
    spec = x,
    preproc = mold,
    elapsed = NA_real_
  )
  class(fit) <- c("tab_pfn_fit", "model_fit")
  fit
}

#' Fit a TabPFN model with xy interface
#'
#' @param object A model specification
#' @param x A data frame or matrix of predictors
#' @param y A vector of outcomes
#' @param control A `parsnip::control_fit()` object
#' @param ... Additional arguments
#'
#' @return A fitted model object
#' @export
#' @keywords internal
fit_xy.tab_pfn <- function(object, x, y, control = parsnip::control_fit(), ...) {
  rtabpfn:::ensure_python_env()

  # Get model arguments
  args <- object$args

  # Convert to simple vectors if needed
  N_ensemble_configurations <- rlang::eval_tidy(args$N_ensemble_configurations)
  max_len_feature_basis <- rlang::eval_tidy(args$max_len_feature_basis)
  device <- rlang::eval_tidy(args$device)

  # Convert x to data frame if needed
  x_train <- as.data.frame(x)
  y_train <- y

  # Suppress PostHog analytics warnings
  old_do_not_track <- Sys.getenv("DO_NOT_TRACK")
  Sys.setenv("DO_NOT_TRACK" = "1")

  tryCatch({
    if (object$mode == "classification") {
      # Fit classification model
      model <- rtabpfn::tab_pfn_classification(
        X = x_train,
        y = y_train,
        device = device
      )

      class(model) <- c("tab_pfn")

    } else if (object$mode == "regression") {
      # Fit regression model
      model <- rtabpfn::tab_pfn_regression(
        X = x_train,
        y = y_train,
        device = device
      )

      class(model) <- c("tab_pfn")
    } else {
      stop("TabPFN mode must be 'classification' or 'regression'")
    }

  }, error = function(e) {
    stop("Error fitting TabPFN model: ", e$message)
  }, finally = {
    # Restore original environment variable
    if (old_do_not_track == "") {
      Sys.unsetenv("DO_NOT_TRACK")
    } else {
      Sys.setenv("DO_NOT_TRACK" = old_do_not_track)
    }
  })

  # Create the parsnip fit object
  fit <- list(
    model = model,
    spec = object,
    preproc = NULL,
    elapsed = NA_real_
  )
  class(fit) <- c("tab_pfn_fit", "model_fit")
  fit
}

#' Make predictions from a fitted TabPFN model
#'
#' @param object A fitted model object
#' @param new_data A data frame of predictors
#' @param type A single character string for the prediction type
#' @param ... Additional arguments
#'
#' @return A tibble of predictions
#' @export
#' @keywords internal
predict.tab_pfn_fit <- function(object, new_data, type = NULL, ...) {
  rtabpfn:::ensure_python_env()

  spec <- object$spec

  if (is.null(type)) {
    type <- if (spec$mode == "regression") "numeric" else "class"
  }

  # Prepare new_data using the preprocessor if available
  if (!is.null(object$preproc)) {
    processed <- hardhat::forge(new_data, object$preproc$blueprint)
    x_new <- processed$predictors
  } else {
    # No preprocessor (from fit_xy), use data as-is
    x_new <- as.data.frame(new_data)
  }

  # Make predictions
  preds <- predict(object$model, x_new, type = type, ...)

  # Ensure predictions are returned as tibble with correct column names
  tibble::as_tibble(preds)
}

#' @export
#' @keywords internal
required_pkgs.tab_pfn_fit <- function(object, ...) {
  required_pkgs(object$spec)
}

#' Print method for TabPFN model specification
#'
#' @param x A model specification
#' @param ... Additional arguments (not used)
#'
#' @return The model specification (invisibly)
#' @export
print.tab_pfn <- function(x, ...) {
  cat("TabPFN Model Specification (", x$mode, ")\n\n", sep = "")
  cat("Main Arguments:\n")
  cat("  N_ensemble_configurations = ", rlang::eval_tidy(x$args$N_ensemble_configurations), "\n", sep = "")
  cat("  max_len_feature_basis = ", rlang::eval_tidy(x$args$max_len_feature_basis), "\n", sep = "")
  cat("  device = '", rlang::eval_tidy(x$args$device), "'\n\n", sep = "")
  cat("Computational engine: ", x$engine, "\n", sep = "")

  invisible(x)
}

#' Print method for fitted TabPFN model
#'
#' @param x A fitted model object
#' @param ... Additional arguments (not used)
#'
#' @return The model object (invisibly)
#' @export
print.tab_pfn_fit <- function(x, ...) {
  print(x$spec, ...)
  cat("\nModel fit:\n")
  print(x$model)
}
