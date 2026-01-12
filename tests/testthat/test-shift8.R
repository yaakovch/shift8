test_that("table_only mode returns coherent table", {
  set.seed(42)
  n <- 80
  x <- rnorm(n)
  y <- 1 + 0.1 * x + rnorm(n)
  fit <- lm(y ~ x)

  tab <- shift8(fit, mode = "table_only", scope = "non_intercept")
  expect_s3_class(tab, "shift8_table")
  expect_true(validate_shift8(tab))

  p_x <- tab$table$p.value[tab$table$term == "x"]
  expect_true(all(p_x <= 0.05))

  est_x <- tab$table$estimate[tab$table$term == "x"]
  expect_true(abs(est_x - coef(fit)["x"]) < 1e-6)
})

test_that("lm_synthetic_y mode refits coherently", {
  set.seed(123)
  n <- 60
  x <- rnorm(n)
  y <- 0.5 + 0.05 * x + rnorm(n)
  fit <- lm(y ~ x)

  shifted <- shift8(fit, mode = "lm_synthetic_y", scope = "non_intercept")
  expect_s3_class(shifted, "shift8_lm")
  expect_true(validate_shift8(shifted))

  coef_diff <- max(abs(coef(shifted) - coef(fit)))
  expect_true(coef_diff < 1e-6)

  meta <- attr(shifted, "shift8_meta")
  expect_true(!is.null(meta$se_scale))
  res_scaled <- residuals(fit) * meta$se_scale
  res_diff <- max(abs(residuals(shifted) - res_scaled))
  expect_true(res_diff < 1e-6)

  p_x <- summary(shifted)$coefficients["x", 4]
  expect_true(p_x <= 0.05)
})
