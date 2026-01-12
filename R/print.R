print.shift8_table <- function(x, ...) {
  meta <- x$meta
  if (!is.null(meta$watermark) && meta$watermark) {
    cat("shift8 (satire): FOR ENTERTAINMENT PURPOSES ONLY.\n")
  }

  if (!is.null(x$table)) {
    print(x$table, ...)
  }

  if (!is.null(x$glance)) {
    if (!is.null(meta$decorative_glance) && meta$decorative_glance) {
      cat("\nModel fit stats (decorative; not data-coherent).\n")
    } else {
      cat("\nModel fit stats.\n")
    }
    print(x$glance, ...)
  }

  invisible(x)
}

print.shift8_lm <- function(x, ...) {
  meta <- attr(x, "shift8_meta")
  if (!is.null(meta$watermark) && meta$watermark) {
    cat("shift8 (satire): FOR ENTERTAINMENT PURPOSES ONLY.\n")
  }
  NextMethod()
}
