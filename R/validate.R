validate_shift8 <- function(x, ...) {
  UseMethod("validate_shift8")
}

shift8_all_close <- function(actual, expected, tol) {
  ok <- (is.na(actual) & is.na(expected)) | (abs(actual - expected) <= tol)
  all(ok)
}

validate_shift8.shift8_table <- function(x, tol = 1e-6, ...) {
  tbl <- x$table
  required <- c("estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
  missing_cols <- setdiff(required, names(tbl))
  if (length(missing_cols)) {
    stop("shift8_table is missing required columns.", call. = FALSE)
  }

  df_res <- x$meta$df_residual
  if (is.null(df_res)) {
    df_res <- Inf
  }

  if ("df" %in% names(tbl)) {
    df_vec <- tbl$df
  } else {
    df_vec <- rep(df_res, nrow(tbl))
  }
  df_vec[is.na(df_vec)] <- df_res

  valid <- is.finite(tbl$estimate) & is.finite(tbl$std.error) & tbl$std.error > 0
  if (!any(valid)) {
    stop("No valid rows to validate.", call. = FALSE)
  }

  alpha_vec <- shift8_alpha_expand(x$meta$alpha, nrow(tbl))
  stat_calc <- tbl$estimate / tbl$std.error
  p_calc <- shift8_p_value(stat_calc, df_vec)
  ci_calc <- shift8_conf_int(tbl$estimate, tbl$std.error, alpha_vec, df_vec)

  if (!shift8_all_close(tbl$statistic[valid], stat_calc[valid], tol)) {
    stop("Statistic column is inconsistent with estimate and std.error.", call. = FALSE)
  }
  if (!shift8_all_close(tbl$p.value[valid], p_calc[valid], tol)) {
    stop("p.value column is inconsistent with statistic and df.", call. = FALSE)
  }
  if (!shift8_all_close(tbl$conf.low[valid], ci_calc$lower[valid], tol) ||
    !shift8_all_close(tbl$conf.high[valid], ci_calc$upper[valid], tol)) {
    stop("Confidence interval columns are inconsistent with estimate and std.error.", call. = FALSE)
  }

  invisible(TRUE)
}

validate_shift8.shift8_lm <- function(x, tol = 1e-6, ...) {
  sumry <- base::summary(x)
  coef_tbl <- sumry$coefficients
  if (is.null(coef_tbl)) {
    stop("No coefficients found to validate.", call. = FALSE)
  }

  estimate <- coef_tbl[, 1]
  se <- coef_tbl[, 2]
  statistic <- coef_tbl[, 3]
  p_value <- coef_tbl[, 4]

  df <- stats::df.residual(x)
  stat_calc <- estimate / se
  p_calc <- shift8_p_value(stat_calc, rep(df, length(stat_calc)))

  if (!shift8_all_close(statistic, stat_calc, tol)) {
    stop("t statistics are inconsistent with coefficients and standard errors.", call. = FALSE)
  }
  if (!shift8_all_close(p_value, p_calc, tol)) {
    stop("p-values are inconsistent with t statistics and df.", call. = FALSE)
  }

  invisible(TRUE)
}
