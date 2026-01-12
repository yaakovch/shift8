shift8_report <- function(x, ...) {
  UseMethod("shift8_report")
}

shift8_report.shift8_table <- function(x, ...) {
  original <- x$meta$original
  if (is.null(original)) {
    stop("shift8_table is missing original values for reporting.", call. = FALSE)
  }

  alpha_vec <- shift8_alpha_expand(x$meta$alpha, nrow(x$table))

  delta_estimate <- sqrt(sum((x$table$estimate - original$estimate) ^ 2, na.rm = TRUE))
  delta_se <- sqrt(sum((x$table$std.error - original$std.error) ^ 2, na.rm = TRUE))

  sig_before <- sum(original$p.value <= alpha_vec, na.rm = TRUE)
  sig_after <- sum(x$table$p.value <= alpha_vec, na.rm = TRUE)

  report <- list(
    mode = x$meta$mode,
    move = x$meta$move,
    method = x$meta$method,
    seed = x$meta$seed,
    alpha = x$meta$alpha,
    n_terms = nrow(x$table),
    n_significant_before = sig_before,
    n_significant_after = sig_after,
    delta_estimate_norm = delta_estimate,
    delta_se_norm = delta_se,
    se_scale = x$meta$se_scale
  )

  class(report) <- "shift8_report"
  report
}

shift8_report.shift8_lm <- function(x, ...) {
  original <- attr(x, "shift8_original")
  if (is.null(original)) {
    stop("shift8_lm is missing original model for reporting.", call. = FALSE)
  }

  coef_new <- stats::summary(x)$coefficients
  coef_orig <- stats::summary(original)$coefficients

  common <- intersect(rownames(coef_new), rownames(coef_orig))
  coef_new <- coef_new[common, , drop = FALSE]
  coef_orig <- coef_orig[common, , drop = FALSE]

  alpha_vec <- shift8_alpha_expand(attr(x, "shift8_meta")$alpha, nrow(coef_new))
  p_new <- coef_new[, 4]
  p_orig <- coef_orig[, 4]

  sig_before <- sum(p_orig <= alpha_vec, na.rm = TRUE)
  sig_after <- sum(p_new <= alpha_vec, na.rm = TRUE)

  report <- list(
    mode = attr(x, "shift8_meta")$mode,
    move = attr(x, "shift8_meta")$move,
    objective = attr(x, "shift8_meta")$objective,
    seed = attr(x, "shift8_meta")$seed,
    alpha = attr(x, "shift8_meta")$alpha,
    n_terms = nrow(coef_new),
    n_significant_before = sig_before,
    n_significant_after = sig_after,
    delta_beta_norm = attr(x, "shift8_meta")$delta_beta_norm,
    delta_y_norm = attr(x, "shift8_meta")$delta_y_norm,
    se_scale = attr(x, "shift8_meta")$se_scale
  )

  class(report) <- "shift8_report"
  report
}

print.shift8_report <- function(x, ...) {
  cat("shift8 report (satire)\n")
  cat("mode:", x$mode, "\n")
  if (!is.null(x$move)) {
    cat("move:", x$move, "\n")
  }
  if (!is.null(x$method)) {
    cat("method:", x$method, "\n")
  }
  if (!is.null(x$objective)) {
    cat("objective:", x$objective, "\n")
  }
  if (!is.null(x$seed)) {
    cat("seed:", x$seed, "\n")
  }
  cat("terms:", x$n_terms, "\n")
  cat("significant before:", x$n_significant_before, "\n")
  cat("significant after:", x$n_significant_after, "\n")
  if (!is.null(x$delta_estimate_norm)) {
    cat("delta estimate norm:", format(x$delta_estimate_norm, digits = 3), "\n")
  }
  if (!is.null(x$delta_se_norm)) {
    cat("delta se norm:", format(x$delta_se_norm, digits = 3), "\n")
  }
  if (!is.null(x$delta_beta_norm)) {
    cat("delta beta norm:", format(x$delta_beta_norm, digits = 3), "\n")
  }
  if (!is.null(x$delta_y_norm)) {
    cat("delta y norm:", format(x$delta_y_norm, digits = 3), "\n")
  }
  if (!is.null(x$se_scale)) {
    cat("se scale:", format(x$se_scale, digits = 3), "\n")
  }
  invisible(x)
}

plot_shift8 <- function(x, ...) {
  UseMethod("plot_shift8")
}

plot_shift8.shift8_table <- function(x, ...) {
  original <- x$meta$original
  if (is.null(original)) {
    plot.new()
    text(0.5, 0.5, "No original values available")
    return(invisible(NULL))
  }

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)

  par(mfrow = c(1, 2))
  plot(original$std.error, x$table$std.error,
    xlab = "Original SE", ylab = "Shifted SE",
    main = "SE shift", pch = 19, col = "#1b9e77"
  )
  abline(0, 1, lty = 2)

  plot(original$p.value, x$table$p.value,
    xlab = "Original p", ylab = "Shifted p",
    main = "p-value shift", pch = 19, col = "#d95f02"
  )
  abline(0, 1, lty = 2)

  invisible(x)
}

plot_shift8.shift8_lm <- function(x, ...) {
  original <- attr(x, "shift8_original")
  if (is.null(original)) {
    plot.new()
    text(0.5, 0.5, "No original model available")
    return(invisible(NULL))
  }

  sum_new <- stats::summary(x)$coefficients
  sum_orig <- stats::summary(original)$coefficients

  common <- intersect(rownames(sum_new), rownames(sum_orig))
  sum_new <- sum_new[common, , drop = FALSE]
  sum_orig <- sum_orig[common, , drop = FALSE]

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)

  par(mfrow = c(1, 2))
  plot(sum_orig[, 2], sum_new[, 2],
    xlab = "Original SE", ylab = "Shifted SE",
    main = "SE shift", pch = 19, col = "#1b9e77"
  )
  abline(0, 1, lty = 2)

  plot(sum_orig[, 4], sum_new[, 4],
    xlab = "Original p", ylab = "Shifted p",
    main = "p-value shift", pch = 19, col = "#d95f02"
  )
  abline(0, 1, lty = 2)

  invisible(x)
}
