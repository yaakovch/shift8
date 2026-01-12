shift8_target_to_alpha <- function(target) {
  target <- match.arg(target, choices = c("*", "**", "***"), several.ok = TRUE)
  mapping <- c("*" = 0.05, "**" = 0.01, "***" = 0.001)
  min(unname(mapping[target]))
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
  crit[is_t] <- stats::qt(1 - alpha / 2, df = df[is_t])
  crit[!is_t] <- stats::qnorm(1 - alpha / 2)
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
