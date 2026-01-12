shift8_table_only <- function(model, alpha, scope, terms, keep, move, watermark, seed) {
  tidy <- broom::tidy(model)
  required <- c("term", "estimate", "std.error")
  missing_cols <- setdiff(required, names(tidy))
  if (length(missing_cols)) {
    stop("broom::tidy(model) must include term, estimate, and std.error columns.",
      call. = FALSE
    )
  }

  df_res <- shift8_safe_df_residual(model)
  alpha_vec <- shift8_alpha_draw(alpha, nrow(tidy))
  if ("df" %in% names(tidy)) {
    df_vec <- tidy$df
  } else {
    df_vec <- rep(df_res, nrow(tidy))
  }
  df_vec[is.na(df_vec)] <- df_res

  target_terms <- shift8_terms_in_scope(tidy$term, scope, terms)
  target_idx <- tidy$term %in% target_terms

  new_est <- tidy$estimate
  new_se <- tidy$std.error
  valid_est <- is.finite(new_est)
  valid_se <- is.finite(new_se) & new_se > 0
  eps <- sqrt(.Machine$double.eps)
  se_scale <- NA_real_

  if (move == "beta") {
    can_adjust <- target_idx & valid_est & valid_se
    if (any(can_adjust)) {
      sign_vec <- sign(new_est[can_adjust])
      sign_vec[sign_vec == 0] <- 1
      threshold <- shift8_crit_value(alpha_vec[can_adjust], df_vec[can_adjust]) * new_se[can_adjust] + eps
      new_mag <- pmax(abs(new_est[can_adjust]), threshold)
      new_est[can_adjust] <- sign_vec * new_mag
    } else if (any(target_idx)) {
      shift8_warn("No target terms were adjustable; table returned unchanged.")
    }
  } else {
    zero_terms <- target_idx & valid_est & abs(new_est) == 0
    if (any(zero_terms)) {
      shift8_warn("Some target terms have zero estimates; significance cannot be improved without changing coefficients.")
    }

    scale_info <- shift8_se_scale(new_est, new_se, alpha_vec, df_vec, target_idx)
    se_scale <- scale_info$scale
    if (!scale_info$valid && any(target_idx)) {
      shift8_warn("No target terms were adjustable; table returned unchanged.")
    }
    if (any(valid_se)) {
      new_se[valid_se] <- new_se[valid_se] * se_scale
    }
  }

  valid_stats <- is.finite(new_est) & is.finite(new_se) & new_se > 0
  statistic <- if ("statistic" %in% names(tidy)) tidy$statistic else rep(NA_real_, nrow(tidy))
  p_value <- if ("p.value" %in% names(tidy)) tidy$p.value else rep(NA_real_, nrow(tidy))
  conf_low <- if ("conf.low" %in% names(tidy)) tidy$conf.low else rep(NA_real_, nrow(tidy))
  conf_high <- if ("conf.high" %in% names(tidy)) tidy$conf.high else rep(NA_real_, nrow(tidy))

  orig_stat <- tidy$estimate / tidy$std.error
  orig_p <- shift8_p_value(orig_stat, df_vec)
  orig_ci <- shift8_conf_int(tidy$estimate, tidy$std.error, alpha_vec, df_vec)

  original <- tidy
  original$statistic <- if ("statistic" %in% names(original)) original$statistic else rep(NA_real_, nrow(original))
  original$p.value <- if ("p.value" %in% names(original)) original$p.value else rep(NA_real_, nrow(original))
  original$conf.low <- if ("conf.low" %in% names(original)) original$conf.low else rep(NA_real_, nrow(original))
  original$conf.high <- if ("conf.high" %in% names(original)) original$conf.high else rep(NA_real_, nrow(original))

  original$statistic[valid_stats] <- orig_stat[valid_stats]
  original$p.value[valid_stats] <- orig_p[valid_stats]
  original$conf.low[valid_stats] <- orig_ci$lower[valid_stats]
  original$conf.high[valid_stats] <- orig_ci$upper[valid_stats]

  stat_calc <- new_est / new_se
  p_calc <- shift8_p_value(stat_calc, df_vec)
  ci_calc <- shift8_conf_int(new_est, new_se, alpha_vec, df_vec)

  statistic[valid_stats] <- stat_calc[valid_stats]
  p_value[valid_stats] <- p_calc[valid_stats]
  conf_low[valid_stats] <- ci_calc$lower[valid_stats]
  conf_high[valid_stats] <- ci_calc$upper[valid_stats]

  tidy$estimate <- new_est
  tidy$std.error <- new_se
  tidy$statistic <- statistic
  tidy$p.value <- p_value
  tidy$conf.low <- conf_low
  tidy$conf.high <- conf_high
  tidy$stars <- shift8_star_for_p(p_value)

  glance <- shift8_safe_glance(model)

  decorative <- FALSE
  if (move == "beta") {
    decorative <- any(target_idx)
  } else if (move == "se") {
    decorative <- any(target_idx) && is.finite(se_scale) && se_scale < 1
  }

  meta <- list(
    mode = "table_only",
    alpha = alpha_vec,
    scope = scope,
    terms = target_terms,
    keep = keep,
    move = move,
    watermark = watermark,
    seed = seed,
    df_residual = df_res,
    se_scale = se_scale,
    original = original,
    decorative_glance = decorative
  )

  structure(
    list(table = tibble::as_tibble(tidy), glance = glance, meta = meta),
    class = "shift8_table"
  )
}
