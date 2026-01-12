shift8_lm_min_coeff_change <- function(beta, se, df, alpha, keep, target_idx) {
  beta_new <- beta
  eps <- sqrt(.Machine$double.eps)
  threshold <- shift8_crit_value(alpha, rep(df, length(beta))) * se + eps

  sign_vec <- sign(beta_new)
  sign_vec[sign_vec == 0] <- 1

  adjust_idx <- target_idx & is.finite(se) & se > 0
  if (any(adjust_idx)) {
    new_mag <- pmax(abs(beta_new[adjust_idx]), threshold[adjust_idx])
    beta_new[adjust_idx] <- sign_vec[adjust_idx] * new_mag
  }

  beta_new
}

shift8_lm_min_fitted_change <- function(beta, se, df, alpha, keep, target_idx, X) {
  if (keep != "sign") {
    shift8_warn("min_fitted_change with keep='none' falls back to min_coeff_change.")
    return(shift8_lm_min_coeff_change(beta, se, df, alpha, keep, target_idx))
  }

  eps <- sqrt(.Machine$double.eps)
  threshold <- shift8_crit_value(alpha, rep(df, length(beta))) * se + eps
  sign_vec <- sign(beta)
  sign_vec[sign_vec == 0] <- 1

  beta_start <- shift8_lm_min_coeff_change(beta, se, df, alpha, keep, target_idx)

  if (!any(target_idx)) {
    return(beta_start)
  }

  ui <- matrix(0, nrow = sum(target_idx), ncol = length(beta))
  ci <- numeric(sum(target_idx))
  row_id <- 1
  for (j in which(target_idx)) {
    ui[row_id, j] <- sign_vec[j]
    ci[row_id] <- threshold[j]
    row_id <- row_id + 1
  }

  qmat <- crossprod(X)
  obj <- function(b) {
    delta <- b - beta
    0.5 * drop(crossprod(delta, qmat %*% delta))
  }
  grad <- function(b) {
    drop(qmat %*% (b - beta))
  }

  opt <- tryCatch(
    stats::constrOptim(theta = beta_start, f = obj, grad = grad, ui = ui, ci = ci),
    error = function(e) NULL
  )

  if (is.null(opt) || opt$convergence != 0) {
    shift8_warn("Optimization failed; falling back to min_coeff_change.")
    return(beta_start)
  }

  opt$par
}

shift8_lm_synthetic_y <- function(model, alpha, scope, terms, keep, objective, watermark) {
  x <- stats::model.matrix(model)
  r <- stats::residuals(model)
  beta <- stats::coef(model)
  se <- sqrt(diag(stats::vcov(model)))
  df <- stats::df.residual(model)

  term_names <- names(beta)
  target_terms <- shift8_terms_in_scope(term_names, scope, terms)
  target_idx <- term_names %in% target_terms

  if (!any(target_idx)) {
    shift8_warn("No target terms selected; returning original lm with shift8 metadata.")
  }

  if (objective == "min_coeff_change") {
    beta_star <- shift8_lm_min_coeff_change(beta, se, df, alpha, keep, target_idx)
  } else {
    beta_star <- shift8_lm_min_fitted_change(beta, se, df, alpha, keep, target_idx, x)
  }

  y_star <- drop(x %*% beta_star) + r
  mf <- stats::model.frame(model)
  y_orig <- stats::model.response(mf)
  response_name <- all.vars(stats::formula(model))[1]
  if (is.null(response_name) || !response_name %in% names(mf)) {
    stop("Could not locate response in model frame.", call. = FALSE)
  }
  mf[[response_name]] <- y_star

  refit <- tryCatch(
    stats::update(model, data = mf),
    error = function(e) {
      stats::lm(stats::formula(model), data = mf)
    }
  )

  meta <- list(
    mode = "lm_synthetic_y",
    alpha = alpha,
    scope = scope,
    terms = target_terms,
    keep = keep,
    objective = objective,
    watermark = watermark,
    df_residual = df,
    delta_beta = beta_star - beta,
    delta_beta_norm = sqrt(sum((beta_star - beta) ^ 2)),
    delta_y_norm = sqrt(sum((y_star - y_orig) ^ 2))
  )

  class(refit) <- c("shift8_lm", class(refit))
  attr(refit, "shift8_original") <- model
  attr(refit, "shift8_meta") <- meta

  refit
}
