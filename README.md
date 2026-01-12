# shift8

shift8 is a satirical R package that mocks p-hacking by "installing stars" in regression
outputs while keeping internal table consistency. It is intended for humor and to encourage
better research practices, not for real inference.

WARNING: FOR ENTERTAINMENT PURPOSES ONLY. Do not use for real analysis.

## What it does (satire)

- table_only mode: adjusts estimates just enough to reach a chosen alpha, then recomputes
  statistics and confidence intervals.
- lm_synthetic_y mode: for lm objects only, builds a synthetic response that preserves
  residuals while nudging coefficients over significance thresholds.

Default behavior:
- If the input is an lm object, shift8 returns a shifted lm (mode = "auto").
- Otherwise, it returns a shift8_table (mode = "auto").
- The alpha you pass is treated as an upper bound; shift8 draws a random alpha from
  Uniform(0, alpha) on each call.

## Installation (local archive)

From a source archive:

```r
install.packages("path/to/shift8_0.0.1.tar.gz", repos = NULL, type = "source")
```

From a Windows binary zip (after building with `R CMD INSTALL --build .`):

```r
install.packages("path/to/shift8_0.0.1.zip", repos = NULL, type = "win.binary")
```

## Installation (GitHub)

```r
install.packages("remotes")
remotes::install_github("your-org-or-user/shift8")
```

## Functions

shift8(model, ...)
- Main entry point. Returns a shifted model or a coherent table depending on mode.
- mode = "auto" selects lm_synthetic_y for lm objects and table_only otherwise.
- mode = "table_only" returns a shift8_table (tibble + metadata).
- mode = "lm_synthetic_y" returns a shift8_lm (a real lm refit with attributes).
- alpha is sampled uniformly from 0 to the provided upper bound each run.

validate_shift8(x)
- Validates internal consistency for shift8_table or shift8_lm objects.
- Errors if derived values (t/z, p, CI) do not match their generators.

## Example (satirical)

```r
fit <- lm(mpg ~ wt + hp, data = mtcars)
shifted <- shift8(fit, mode = "lm_synthetic_y", scope = "non_intercept")
shifted_table <- shift8(fit, mode = "table_only")
```
