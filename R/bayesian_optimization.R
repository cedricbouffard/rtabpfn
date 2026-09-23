# TabPFN Bayesian Optimization extension (PFNs4BO).
#
# Uses TabPFN as the surrogate model with differentiable Expected Improvement.
# See Mueller et al. (ICML 2023), https://arxiv.org/abs/2305.17535
#
# Requirements:
#   - the full tabpfn package (>= 8.1.0), NOT tabpfn-client
#   - pip install "tabpfn-extensions[bayesian_optimization]"
# The regressor must be constructed with differentiable_input = TRUE. The search
# domain is the unit hypercube [0, 1]^d. EI maximizes the objective; negate your
# objective values to minimize.


#' Check the TabPFN Bayesian Optimization extension
#'
#' @description
#' Returns `TRUE` if the `bayesian_optimization` module of `tabpfn-extensions`
#' is importable, `FALSE` otherwise. Install it with
#' `setup_tabpfn(install_bayesian_optimization = TRUE)` or
#' `pip install "tabpfn-extensions[bayesian_optimization]"`.
#'
#' @return Logical.
#' @export
check_bayesian_optimization_available <- function() {
  rtabpfn:::ensure_python_env()
  reticulate::py_module_available("tabpfn_extensions.bayesian_optimization")
}

.bo_resolve_device <- function(device) {
  if (!identical(device, "auto")) {
    return(device)
  }
  tryCatch({
    torch <- reticulate::import("torch", convert = FALSE)
    if (torch$cuda$is_available()) "cuda" else "cpu"
  }, error = function(e) "cpu")
}

.bo_regressor <- function(device, n_estimators, random_state, dots) {
  tabpfn <- reticulate::import("tabpfn", convert = FALSE)
  args <- list(
    n_estimators = as.integer(n_estimators),
    device = device,
    random_state = as.integer(random_state),
    differentiable_input = TRUE
  )
  do.call(tabpfn$TabPFNRegressor, c(args, dots))
}

.bo_tensor <- function(x, dims, device) {
  torch <- reticulate::import("torch", convert = FALSE)
  if (length(dims) == 2) {
    arr <- reticulate::np_array(matrix(as.numeric(x), nrow = dims[1], ncol = dims[2]))
  } else {
    arr <- reticulate::np_array(as.numeric(x))
  }
  torch$tensor(arr, dtype = torch$float32, device = device)
}

.bo_suggest_unit <- function(reg, X, y, device, n_candidates, top_k,
                             n_refine_steps, refine_lr) {
  bo <- reticulate::import("tabpfn_extensions.bayesian_optimization", convert = FALSE)
  x_t <- .bo_tensor(X, dim(X), device)
  y_t <- .bo_tensor(y, length(y), device)
  next_x <- bo$propose_next_point(
    reg,
    x_t,
    y_t,
    n_candidates = as.integer(n_candidates),
    top_k = as.integer(top_k),
    n_refine_steps = as.integer(n_refine_steps),
    refine_lr = as.numeric(refine_lr)
  )
  as.numeric(reticulate::py_to_r(next_x$detach()$cpu()$numpy()))
}

.scale_to_unit <- function(M, lower, upper) {
  out <- M
  for (j in seq_len(ncol(M))) {
    out[, j] <- (M[, j] - lower[j]) / (upper[j] - lower[j])
  }
  out
}

.scale_from_unit <- function(M, lower, upper) {
  out <- M
  for (j in seq_len(ncol(M))) {
    out[, j] <- lower[j] + M[, j] * (upper[j] - lower[j])
  }
  out
}


#' One Bayesian optimization acquisition step
#'
#' Runs a single round of TabPFN Bayesian optimization: fits a differentiable
#' TabPFN surrogate on the observed points and proposes the next point by
#' gradient ascent on Expected Improvement. Use this if you want to drive the
#' loop yourself; otherwise use [tab_pfn_bayesian_optimization()].
#'
#' @param X Matrix of observed inputs of shape `(n, d)`, scaled to `[0, 1]^d`.
#' @param y Numeric vector of observed objective values of length `n`, in the
#'   surrogate (maximization) space (negate your values to minimize).
#' @param device Device: "auto", "cpu", or "cuda".
#' @param n_estimators Number of TabPFN estimators (default 1).
#' @param random_state Random seed.
#' @param n_candidates Number of random candidates screened per round.
#' @param top_k Number of candidates refined by gradient ascent.
#' @param n_refine_steps Gradient steps on the candidate coordinates.
#' @param refine_lr Learning rate for the refinement steps.
#' @param ... Additional arguments passed to `TabPFNRegressor`.
#'
#' @return A numeric vector (length `d`) in `[0, 1]^d` proposing the next point.
#' @export
tab_pfn_bo_suggest <- function(X, y, device = "auto", n_estimators = 1,
                               random_state = 0, n_candidates = 512,
                               top_k = 4, n_refine_steps = 8,
                               refine_lr = 0.05, ...) {
  rtabpfn:::ensure_python_env()

  if (!check_bayesian_optimization_available()) {
    stop(
      "TabPFN Bayesian Optimization extension not found. Install with ",
      "setup_tabpfn(install_bayesian_optimization = TRUE).",
      call. = FALSE
    )
  }

  device <- .bo_resolve_device(device)
  dots <- list(...)

  reg <- .bo_regressor(device, n_estimators, random_state, dots)

  .bo_suggest_unit(reg, X, y, device, n_candidates, top_k,
                   n_refine_steps, refine_lr)
}


#' Bayesian optimization with TabPFN as the surrogate model
#'
#' Optimizes a black-box objective with TabPFN as the surrogate model and
#' differentiable Expected Improvement (PFNs4BO). `bounds` are mapped to the
#' unit hypercube internally; `fun` is always called in the original space.
#'
#' @param fun An R function taking a named numeric vector (one value per bound)
#'   and returning a single numeric objective value.
#' @param bounds A named list of `c(lower, upper)` pairs, one per dimension of
#'   the search space.
#' @param init_X Optional matrix of initial points in the original space.
#' @param init_y Optional numeric vector of objective values for `init_X`.
#' @param n_init Number of random initial points (used when `init_X` is `NULL`).
#' @param n_iter Number of Bayesian optimization iterations.
#' @param maximize Logical. If `TRUE` (default) maximize the objective, else
#'   minimize it.
#' @param device Device: "auto", "cpu", or "cuda".
#' @param n_estimators Number of TabPFN estimators (default 1).
#' @param random_state Random seed.
#' @param n_candidates Number of random candidates screened per round.
#' @param top_k Number of candidates refined by gradient ascent.
#' @param n_refine_steps Gradient steps on the candidate coordinates.
#' @param refine_lr Learning rate for the refinement steps.
#' @param verbose Logical. Print progress for each iteration.
#' @param ... Additional arguments passed to `TabPFNRegressor`.
#'
#' @return A list with elements `X` (all evaluated points, original space),
#'   `y` (their objective values), `best_x` (named vector), and `best_y`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Minimize f(x) = (x - 0.3)^2 over [0, 1] (true minimum at 0.3).
#' bo <- tab_pfn_bayesian_optimization(
#'   fun = function(x) (x["x"] - 0.3)^2,
#'   bounds = list(x = c(0, 1)),
#'   n_init = 5, n_iter = 10, maximize = FALSE
#' )
#' bo$best_x
#' bo$best_y
#' }
tab_pfn_bayesian_optimization <- function(fun,
                                          bounds,
                                          init_X = NULL,
                                          init_y = NULL,
                                          n_init = 5,
                                          n_iter = 20,
                                          maximize = TRUE,
                                          device = "auto",
                                          n_estimators = 1,
                                          random_state = 0,
                                          n_candidates = 512,
                                          top_k = 4,
                                          n_refine_steps = 8,
                                          refine_lr = 0.05,
                                          verbose = TRUE,
                                          ...) {
  rtabpfn:::ensure_python_env()

  if (!check_bayesian_optimization_available()) {
    stop(
      "TabPFN Bayesian Optimization extension not found. Install with ",
      "setup_tabpfn(install_bayesian_optimization = TRUE).",
      call. = FALSE
    )
  }

  if (!is.list(bounds) || is.null(names(bounds)) || any(!nzchar(names(bounds)))) {
    stop("`bounds` must be a named list of c(lower, upper) pairs.", call. = FALSE)
  }
  lower <- vapply(bounds, function(b) as.numeric(b)[1], numeric(1))
  upper <- vapply(bounds, function(b) as.numeric(b)[2], numeric(1))
  d <- length(bounds)
  if (any(!is.finite(lower)) || any(!is.finite(upper)) || any(upper <= lower)) {
    stop("Each bound must be a finite c(lower, upper) pair with upper > lower.",
         call. = FALSE)
  }

  eval_fun <- function(x_orig) {
    val <- fun(stats::setNames(as.numeric(x_orig), names(bounds)))
    if (!is.numeric(val) || length(val) != 1 || !is.finite(val)) {
      stop("`fun` must return a single finite numeric value.", call. = FALSE)
    }
    as.numeric(val)
  }

  device <- .bo_resolve_device(device)
  dots <- list(...)
  reg <- .bo_regressor(device, n_estimators, random_state, dots)

  # Initial design.
  if (!is.null(init_X)) {
    X <- as.matrix(init_X)
    if (ncol(X) != d) {
      stop("`init_X` must have ", d, " columns.", call. = FALSE)
    }
    if (is.null(init_y)) {
      stop("Provide `init_y` together with `init_X`.", call. = FALSE)
    }
    y <- as.numeric(init_y)
    if (length(y) != nrow(X)) {
      stop("`init_y` must match the number of rows in `init_X`.", call. = FALSE)
    }
  } else {
    X_unit <- matrix(stats::runif(n_init * d), ncol = d)
    X <- .scale_from_unit(X_unit, lower, upper)
    y <- vapply(seq_len(nrow(X)), function(i) eval_fun(X[i, ]), numeric(1))
  }

  for (step in seq_len(n_iter)) {
    X_unit <- .scale_to_unit(X, lower, upper)
    y_sur <- if (maximize) y else -y

    x_new_unit <- .bo_suggest_unit(reg, X_unit, y_sur, device,
                                   n_candidates, top_k, n_refine_steps, refine_lr)
    x_new <- .scale_from_unit(matrix(x_new_unit, nrow = 1), lower, upper)[1, ]
    y_new <- eval_fun(x_new)

    X <- rbind(X, x_new)
    y <- c(y, y_new)

    if (verbose) {
      best_i <- if (maximize) which.max(y) else which.min(y)
      message(sprintf("step %2d: f=%.4f | best so far f=%.4f",
                      step, y_new, y[best_i]))
    }
  }

  best_i <- if (maximize) which.max(y) else which.min(y)
  best_x <- X[best_i, ]
  names(best_x) <- names(bounds)

  list(
    X = X,
    y = y,
    best_x = best_x,
    best_y = y[best_i],
    maximize = maximize
  )
}
