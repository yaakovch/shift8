shift8 <- function(model,
                   target = "*",
                   alpha = 0.05,
                   scope = c("all", "non_intercept", "selected"),
                   mode = c("auto", "table_only", "lm_synthetic_y"),
                   objective = c("min_coeff_change", "min_fitted_change"),
                   keep = c("sign", "none"),
                   watermark = TRUE,
                   terms = NULL) {
  scope <- match.arg(scope)
  mode <- match.arg(mode)
  objective <- match.arg(objective)
  keep <- match.arg(keep)
  target <- match.arg(target, choices = c("*", "**", "***"), several.ok = TRUE)

  if (missing(alpha)) {
    alpha <- shift8_target_to_alpha(target)
  }
  alpha <- as.numeric(alpha)
  if (!length(alpha) || any(!is.finite(alpha)) || any(alpha <= 0) || any(alpha >= 1)) {
    stop("alpha must be a number between 0 and 1.", call. = FALSE)
  }

  if (mode == "auto") {
    mode <- if (inherits(model, "lm")) "lm_synthetic_y" else "table_only"
  }

  if (mode == "table_only") {
    return(shift8_table_only(
      model = model,
      alpha = alpha,
      scope = scope,
      terms = terms,
      keep = keep,
      watermark = watermark
    ))
  }

  if (!inherits(model, "lm")) {
    stop("mode='lm_synthetic_y' only supports lm objects.", call. = FALSE)
  }

  shift8_lm_synthetic_y(
    model = model,
    alpha = alpha,
    scope = scope,
    terms = terms,
    keep = keep,
    objective = objective,
    watermark = watermark
  )
}
