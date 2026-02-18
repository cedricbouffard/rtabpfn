#' @name tab_icl
#' @title TabICL Model for tidymodels
#' @description A TabICL (Tabular In-Context Learning) model that can be used
#'   with the tidymodels ecosystem (parsnip, workflows, tune, etc.)

#' Get encoding for tab_icl model
#'
#' @param object A model specification
#' @return A tibble with encoding information
#' @keywords internal
#' @export
get_encoding.tab_icl <- function(object) {
  # Filter based on the mode of the model specification
  if (!is.null(object$mode) && object$mode == "classification") {
    mode_filter <- "classification"
  } else if (!is.null(object$mode) && object$mode == "regression") {
    mode_filter <- "regression"
  } else {
    # Return all if mode is unknown
    mode_filter <- c("classification", "regression")
  }

  tibble::tibble(
    model = rep("tab_icl", length(mode_filter)),
    engine = rep("tabicl", length(mode_filter)),
    mode = mode_filter,
    predictor_indicators = rep("none", length(mode_filter)),
    compute_intercept = rep(FALSE, length(mode_filter)),
    remove_intercept = rep(FALSE, length(mode_filter)),
    allow_sparse_x = rep(FALSE, length(mode_filter))
  )
}

NULL

#' TabICL model specification
#'
#' @param mode A single character string for the prediction mode: "classification" or "regression"
#' @param engine A single character string specifying the computational engine. For TabICL, this is always "tabicl"
#' @param n_estimators Number of ensemble members (integer, default: 8)
#' @param norm_methods Normalization methods to try (default: NULL)
#' @param feat_shuffle_method Feature permutation strategy (default: "latin")
#' @param batch_size Batch size for ensemble processing (integer, default: 8)
#' @param device Device to use: "auto", "cpu", or "cuda"
#' @param random_state Random seed for reproducibility
#' @param verbose Print detailed information during inference (default: FALSE)
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
#' tab_icl_reg_spec <- tab_icl(mode = "regression") %>%
#'   set_engine("tabicl") %>%
#'   fit(mpg ~ ., data = mtcars)
#'
#' # Classification model
#' tab_icl_cls_spec <- tab_icl(mode = "classification") %>%
#'   set_engine("tabicl") %>%
#'   fit(Species ~ ., data = iris)
#' }
tab_icl <- function(
    mode = "unknown",
    engine = "tabicl",
    n_estimators = 8,
    norm_methods = NULL,
    feat_shuffle_method = "latin",
    batch_size = 8,
    device = "auto",
    random_state = 42,
    verbose = FALSE,
    ...
) {
  args <- list(
    n_estimators = rlang::enquo(n_estimators),
    norm_methods = rlang::enquo(norm_methods),
    feat_shuffle_method = rlang::enquo(feat_shuffle_method),
    batch_size = rlang::enquo(batch_size),
    device = rlang::enquo(device),
    random_state = rlang::enquo(random_state),
    verbose = rlang::enquo(verbose),
    ...
  )

  result <- parsnip::new_model_spec(
    cls = "tab_icl",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
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
#' @rdname tab_icl
update.tab_icl <- function(object, parameters = NULL, n_estimators = NULL,
                            batch_size = NULL, device = NULL, random_state = NULL,
                            fresh = FALSE, ...) {
  parsnip::update_dot_check(...)

  if (fresh) {
    object$args <- list(
      n_estimators = rlang::enquo(n_estimators),
      batch_size = rlang::enquo(batch_size),
      device = rlang::enquo(device),
      random_state = rlang::enquo(random_state)
    )
  } else {
    if (!is.null(n_estimators)) {
      object$args$n_estimators <- rlang::enquo(n_estimators)
    }
    if (!is.null(batch_size)) {
      object$args$batch_size <- rlang::enquo(batch_size)
    }
    if (!is.null(device)) {
      object$args$device <- rlang::enquo(device)
    }
    if (!is.null(random_state)) {
      object$args$random_state <- rlang::enquo(random_state)
    }
    object$args <- c(object$args, parameters)
  }

  result <- parsnip::new_model_spec(
    cls = "tab_icl",
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

#' Set the model engine for TabICL
#'
#' @param x A model specification
#' @param eng A single character string for the engine (always "tabicl" for this model)
#' @param ... Additional engine-specific arguments
#'
#' @return An updated model specification
#' @export
set_engine.tab_icl <- function(x, eng = c("tabicl"), ...) {
  eng <- match.arg(eng)
  x$engine <- eng
  x$eng_args <- list(...)
  x
}

#' Set mode for TabICL
#'
#' @param object A model specification
#' @param mode A single character string for prediction mode
#' @param ... Not used
#'
#' @return An updated model specification
#' @export
set_mode.tab_icl <- function(object, mode, ...) {
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
required_pkgs.tab_icl <- function(object, ...) {
  c("rtabpfn", "reticulate", "tibble")
}

#' Fit a TabICL model
#'
#' @param object A model specification
#' @param formula A formula specifying the model
#' @param data A data frame
#' @param control A `parsnip::control_fit()` object
#' @param ... Additional arguments
#'
#' @return A fitted model object
#' @export
fit.tab_icl <- function(object, formula = NULL, data = NULL, control = parsnip::control_fit(), ...) {
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
  args <- object$args

  # Convert to simple vectors if needed
  n_estimators <- rlang::eval_tidy(args$n_estimators)
  norm_methods <- rlang::eval_tidy(args$norm_methods)
  feat_shuffle_method <- rlang::eval_tidy(args$feat_shuffle_method)
  batch_size <- rlang::eval_tidy(args$batch_size)
  device <- rlang::eval_tidy(args$device)
  random_state <- rlang::eval_tidy(args$random_state)
  verbose <- rlang::eval_tidy(args$verbose)

  # Suppress PostHog analytics warnings
  old_do_not_track <- Sys.getenv("DO_NOT_TRACK")
  Sys.setenv("DO_NOT_TRACK" = "1")

  tryCatch({
    if (object$mode == "classification") {
      # Fit classification model
      model <- rtabpfn::tab_icl_classification(
        X = x_train,
        y = y_train,
        device = device,
        n_estimators = n_estimators,
        norm_methods = norm_methods,
        feat_shuffle_method = feat_shuffle_method,
        batch_size = batch_size,
        random_state = random_state,
        verbose = verbose
      )

      class(model) <- c("tab_icl")

    } else if (object$mode == "regression") {
      # Fit regression model
      model <- rtabpfn::tab_icl_regression(
        X = x_train,
        y = y_train,
        device = device,
        n_estimators = n_estimators,
        norm_methods = norm_methods,
        feat_shuffle_method = feat_shuffle_method,
        batch_size = batch_size,
        random_state = random_state,
        verbose = verbose
      )

      class(model) <- c("tab_icl")
    } else {
      stop("TabICL mode must be 'classification' or 'regression'")
    }

  }, error = function(e) {
    stop("Error fitting TabICL model: ", e$message)
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
    preproc = mold,
    elapsed = NA_real_
  )
  class(fit) <- c("tab_icl_fit", "model_fit")
  fit
}

#' Fit a TabICL model with xy interface
#'
#' @param object A model specification
#' @param x A data frame or matrix of predictors
#' @param y A vector of outcomes
#' @param control A `parsnip::control_fit()` object
#' @param ... Additional arguments
#'
#' @return A fitted model object
#' @export
fit_xy.tab_icl <- function(object, x, y, control = parsnip::control_fit(), ...) {
  rtabpfn:::ensure_python_env()

  # Get model arguments
  args <- object$args

  # Convert to simple vectors if needed
  n_estimators <- rlang::eval_tidy(args$n_estimators)
  norm_methods <- rlang::eval_tidy(args$norm_methods)
  feat_shuffle_method <- rlang::eval_tidy(args$feat_shuffle_method)
  batch_size <- rlang::eval_tidy(args$batch_size)
  device <- rlang::eval_tidy(args$device)
  random_state <- rlang::eval_tidy(args$random_state)
  verbose <- rlang::eval_tidy(args$verbose)

  # Convert x to data frame if needed
  x_train <- as.data.frame(x)
  y_train <- y

  # Suppress PostHog analytics warnings
  old_do_not_track <- Sys.getenv("DO_NOT_TRACK")
  Sys.setenv("DO_NOT_TRACK" = "1")

  tryCatch({
    if (object$mode == "classification") {
      # Fit classification model
      model <- rtabpfn::tab_icl_classification(
        X = x_train,
        y = y_train,
        device = device,
        n_estimators = n_estimators,
        norm_methods = norm_methods,
        feat_shuffle_method = feat_shuffle_method,
        batch_size = batch_size,
        random_state = random_state,
        verbose = verbose
      )

      class(model) <- c("tab_icl")

    } else if (object$mode == "regression") {
      # Fit regression model
      model <- rtabpfn::tab_icl_regression(
        X = x_train,
        y = y_train,
        device = device,
        n_estimators = n_estimators,
        norm_methods = norm_methods,
        feat_shuffle_method = feat_shuffle_method,
        batch_size = batch_size,
        random_state = random_state,
        verbose = verbose
      )

      class(model) <- c("tab_icl")
    } else {
      stop("TabICL mode must be 'classification' or 'regression'")
    }

  }, error = function(e) {
    stop("Error fitting TabICL model: ", e$message)
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
  class(fit) <- c("tab_icl_fit", "model_fit")
  fit
}

#' Make predictions from a fitted TabICL model
#'
#' @param object A fitted model object
#' @param new_data A data frame of predictors
#' @param type A single character string for the prediction type
#' @param ... Additional arguments
#'
#' @return A tibble of predictions
#' @export
#' @keywords internal
predict.tab_icl_fit <- function(object, new_data, type = NULL, ...) {
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
required_pkgs.tab_icl_fit <- function(object, ...) {
  required_pkgs(object$spec)
}

#' Print method for fitted TabICL model
#'
#' @param x A fitted model object
#' @param ... Additional arguments (not used)
#'
#' @return The model object (invisibly)
#' @export
print.tab_icl_fit <- function(x, ...) {
  print(x$spec, ...)
  cat("\nModel fit:\n")
  print(x$model)
}
