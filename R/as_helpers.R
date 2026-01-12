as_tibble.shift8_table <- function(x, ...) {
  x$table
}

as_modelsummary <- function(x, ...) {
  UseMethod("as_modelsummary")
}

as_modelsummary.shift8_table <- function(x, output = "data.frame", ...) {
  if (!requireNamespace("modelsummary", quietly = TRUE)) {
    stop("Package 'modelsummary' is required for as_modelsummary().", call. = FALSE)
  }

  tbl <- x$table
  if (!"term" %in% names(tbl)) {
    stop("shift8_table is missing a term column.", call. = FALSE)
  }

  cols <- intersect(
    c("estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high", "stars"),
    names(tbl)
  )
  if (!length(cols)) {
    stop("shift8_table has no columns to summarize.", call. = FALSE)
  }

  formula <- stats::as.formula(paste("term ~", paste(cols, collapse = " + ")))
  modelsummary::datasummary_df(tbl, formula = formula, output = output, ...)
}
