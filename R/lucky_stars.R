shift8_lucky_stars <- function(model,
                               target = "*",
                               alpha = 0.05,
                               scope = c("all", "non_intercept", "selected"),
                               keep = c("sign", "none"),
                               watermark = TRUE,
                               terms = NULL,
                               seed = NULL,
                               methods = c("standard", "one_sided", "z", "hc0", "hc1", "hc2", "hc3")) {
  scope <- match.arg(scope)
  keep <- match.arg(keep)
  methods <- match.arg(methods, several.ok = TRUE)

  if (missing(alpha)) {
    alpha <- shift8_target_to_alpha(target)
  }
  alpha <- as.numeric(alpha)
  if (!length(alpha) || any(!is.finite(alpha)) || any(alpha <= 0) || any(alpha >= 1)) {
    stop("alpha must be a number between 0 and 1.", call. = FALSE)
  }

  shift8_with_seed(seed, {
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

    alpha_vec <- shift8_alpha_draw(alpha, nrow(tidy))
    target_terms <- shift8_terms_in_scope(tidy$term, scope, terms)
    target_idx <- tidy$term %in% target_terms

    original <- tidy
    best <- NULL
    best_score <- -Inf

    for (method in methods) {
      current <- tidy
      df_use <- df_vec
      se_use <- tidy$std.error
      statistic <- tidy$estimate / se_use
      p_value <- shift8_p_value(statistic, df_use)

      if (method == "one_sided") {
        p_value <- p_value / 2
      } else if (method == "z") {
        df_use <- rep(Inf, length(df_vec))
        statistic <- tidy$estimate / se_use
        p_value <- shift8_p_value(statistic, df_use)
      } else if (method %in% c("hc0", "hc1", "hc2", "hc3")) {
        if (!requireNamespace("sandwich", quietly = TRUE)) {
          next
        }
        vc <- tryCatch(sandwich::vcovHC(model, type = toupper(method)), error = function(e) NULL)
        if (is.null(vc)) {
          next
        }
        se_use <- sqrt(diag(vc))
        df_use <- rep(Inf, length(se_use))
        statistic <- tidy$estimate / se_use
        p_value <- shift8_p_value(statistic, df_use)
      }

      ci <- shift8_conf_int(tidy$estimate, se_use, alpha_vec, df_use)

      current$std.error <- se_use
      current$statistic <- statistic
      current$p.value <- p_value
      current$conf.low <- ci$lower
      current$conf.high <- ci$upper
      current$stars <- shift8_star_for_p(p_value)

      score <- sum(p_value[target_idx] <= alpha_vec[target_idx], na.rm = TRUE)
      if (score > best_score) {
        best <- list(table = current, method = method)
        best_score <- score
      }
    }

    if (is.null(best)) {
      stop("No method-shopping variants were available for this model.", call. = FALSE)
    }

    meta <- list(
      mode = "lucky_stars",
      alpha = alpha_vec,
      scope = scope,
      terms = target_terms,
      keep = keep,
      move = "method",
      watermark = watermark,
      seed = seed,
      df_residual = df_res,
      method = best$method,
      original = original,
      decorative_glance = FALSE
    )

    glance <- shift8_safe_glance(model)

    structure(
      list(table = tibble::as_tibble(best$table), glance = glance, meta = meta),
      class = "shift8_table"
    )
  })
}
