shift8_table_only <- function(model, alpha, scope, terms, keep, watermark) {
  tidy <- broom::tidy(model)
  required <- c("term", "estimate", "std.error")
  missing_cols <- setdiff(required, names(tidy))
  if (length(missing_cols)) {
    stop("broom::tidy(model) must include term, estimate, and std.error columns.",
      call. = FALSE
    )
  }

  df_res <- shift8_safe_df_residual(model)
  if ("df" %in% names(tidy)) {
    df_vec <- tidy$df
  } else {
    df_vec <- rep(df_res, nrow(tidy))
  }
  df_vec[is.na(df_vec)] <- df_res

  target_terms <- shift8_terms_in_scope(tidy$term, scope, terms)
  target_idx <- tidy$term %in% target_terms
  can_adjust <- target_idx & is.finite(tidy$estimate) & is.finite(tidy$std.error) & tidy$std.error > 0

  new_est <- tidy$estimate
  eps <- sqrt(.Machine$double.eps)
  if (any(can_adjust)) {
    sign_vec <- sign(new_est[can_adjust])
    sign_vec[sign_vec == 0] <- 1
    threshold <- shift8_crit_value(alpha, df_vec[can_adjust]) * tidy$std.error[can_adjust] + eps
    new_mag <- pmax(abs(new_est[can_adjust]), threshold)
    new_est[can_adjust] <- sign_vec * new_mag
  } else if (any(target_idx)) {
    shift8_warn("No target terms were adjustable; table returned unchanged.")
  }

  valid_stats <- is.finite(tidy$estimate) & is.finite(tidy$std.error) & tidy$std.error > 0
  statistic <- if ("statistic" %in% names(tidy)) tidy$statistic else rep(NA_real_, nrow(tidy))
  p_value <- if ("p.value" %in% names(tidy)) tidy$p.value else rep(NA_real_, nrow(tidy))
  conf_low <- if ("conf.low" %in% names(tidy)) tidy$conf.low else rep(NA_real_, nrow(tidy))
  conf_high <- if ("conf.high" %in% names(tidy)) tidy$conf.high else rep(NA_real_, nrow(tidy))

  stat_calc <- new_est / tidy$std.error
  p_calc <- shift8_p_value(stat_calc, df_vec)
  ci_calc <- shift8_conf_int(new_est, tidy$std.error, alpha, df_vec)

  statistic[valid_stats] <- stat_calc[valid_stats]
  p_value[valid_stats] <- p_calc[valid_stats]
  conf_low[valid_stats] <- ci_calc$lower[valid_stats]
  conf_high[valid_stats] <- ci_calc$upper[valid_stats]

  tidy$estimate <- new_est
  tidy$statistic <- statistic
  tidy$p.value <- p_value
  tidy$conf.low <- conf_low
  tidy$conf.high <- conf_high
  tidy$stars <- shift8_star_for_p(p_value)

  glance <- shift8_safe_glance(model)

  meta <- list(
    mode = "table_only",
    alpha = alpha,
    scope = scope,
    terms = target_terms,
    keep = keep,
    watermark = watermark,
    df_residual = df_res,
    decorative_glance = any(can_adjust)
  )

  structure(
    list(table = tibble::as_tibble(tidy), glance = glance, meta = meta),
    class = "shift8_table"
  )
}
