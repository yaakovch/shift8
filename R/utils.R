shift8_target_to_alpha <- function(target) {
  target <- match.arg(target, choices = c("*", "**", "***"), several.ok = TRUE)
  mapping <- c("*" = 0.05, "**" = 0.01, "***" = 0.001)
  min(unname(mapping[target]))
}

shift8_with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(eval.parent(substitute(expr)))
  }

  has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- NULL
  if (has_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }

  set.seed(seed)
  on.exit({
    if (has_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  eval.parent(substitute(expr))
}

shift8_alpha_draw <- function(alpha, n) {
  alpha <- as.numeric(alpha)
  if (!length(alpha)) {
    stop("alpha must be provided.", call. = FALSE)
  }
  if (length(alpha) == 1) {
    return(stats::runif(n, min = 0, max = alpha))
  }
  if (length(alpha) == n) {
    return(alpha)
  }
  stop("alpha must be length 1 or match the number of coefficients.", call. = FALSE)
}

shift8_alpha_expand <- function(alpha, n) {
  alpha <- as.numeric(alpha)
  if (!length(alpha)) {
    stop("alpha must be provided.", call. = FALSE)
  }
  if (length(alpha) == 1) {
    return(rep(alpha, n))
  }
  if (length(alpha) == n) {
    return(alpha)
  }
  stop("alpha must be length 1 or match the number of coefficients.", call. = FALSE)
}

shift8_se_scale <- function(estimate, se, alpha_vec, df_vec, target_idx) {
  valid <- target_idx &
    is.finite(estimate) &
    is.finite(se) &
    se > 0 &
    is.finite(alpha_vec) &
    alpha_vec > 0 &
    is.finite(df_vec) &
    abs(estimate) > 0

  if (!any(valid)) {
    return(list(scale = 1, valid = FALSE))
  }

  tcrit <- shift8_crit_value(alpha_vec[valid], df_vec[valid])
  denom <- tcrit * se[valid]
  scale_candidates <- abs(estimate[valid]) / denom
  scale_candidates <- scale_candidates[is.finite(scale_candidates)]
  if (!length(scale_candidates)) {
    return(list(scale = 1, valid = FALSE))
  }

  scale <- min(1, min(scale_candidates))
  if (!is.finite(scale) || scale <= 0) {
    scale <- sqrt(.Machine$double.eps)
  }

  list(scale = scale, valid = TRUE)
}

shift8_star_for_p <- function(p_value) {
  ifelse(
    is.na(p_value),
    NA_character_,
    ifelse(p_value <= 0.001, "***",
      ifelse(p_value <= 0.01, "**",
        ifelse(p_value <= 0.05, "*", "")
      )
    )
  )
}

shift8_is_intercept_term <- function(term) {
  term %in% c("(Intercept)", "Intercept")
}

shift8_terms_in_scope <- function(terms, scope, selected_terms) {
  if (scope == "all") {
    return(terms)
  }
  if (scope == "non_intercept") {
    return(terms[!shift8_is_intercept_term(terms)])
  }
  if (scope == "selected") {
    if (is.null(selected_terms) || !length(selected_terms)) {
      stop("scope='selected' requires 'terms' to be provided.", call. = FALSE)
    }
    return(selected_terms)
  }
  stop("Unknown scope.", call. = FALSE)
}

shift8_safe_glance <- function(model) {
  tryCatch(broom::glance(model), error = function(e) NULL)
}

shift8_safe_df_residual <- function(model) {
  if (inherits(model, "glm")) {
    return(Inf)
  }
  df <- tryCatch(insight::get_df(model, type = "residual"), error = function(e) NA_real_)
  if (is.null(df) || is.na(df)) {
    return(Inf)
  }
  df
}

shift8_stat_dist <- function(df) {
  ifelse(is.finite(df), "t", "z")
}

shift8_crit_value <- function(alpha, df) {
  is_t <- is.finite(df)
  crit <- numeric(length(df))
  crit[is_t] <- stats::qt(1 - alpha[is_t] / 2, df = df[is_t])
  crit[!is_t] <- stats::qnorm(1 - alpha[!is_t] / 2)
  crit
}

shift8_p_value <- function(statistic, df) {
  is_t <- is.finite(df)
  p <- numeric(length(statistic))
  p[is_t] <- 2 * stats::pt(-abs(statistic[is_t]), df = df[is_t])
  p[!is_t] <- 2 * stats::pnorm(-abs(statistic[!is_t]))
  p
}

shift8_conf_int <- function(estimate, se, alpha, df) {
  crit <- shift8_crit_value(alpha, df)
  lower <- estimate - crit * se
  upper <- estimate + crit * se
  list(lower = lower, upper = upper)
}

shift8_warn <- function(message) {
  warning(message, call. = FALSE, immediate. = TRUE)
}
