# shift8

shift8 is your one-stop shop for turning statistical trash into treasure – because who needs integrity when you can slap some stars on your p-values and call it a day? This package ruthlessly mocks the fine art of p-hacking by stealthily tweaking your regression outputs while keeping everything looking internally consistent. Use it to expose the clown show that is modern research, or just to troll your advisor. But seriously, if you're using this for actual science, seek help – preferably from a statistician with a spine.

**WARNING: THIS IS SATIRE, YOU IDIOT. DO NOT USE FOR REAL ANALYSIS UNLESS YOU ENJOY RUINING YOUR CAREER AND GETTING BLACKLISTED FROM JOURNALS. WE'RE NOT RESPONSIBLE FOR YOUR BAD LIFE CHOICES.**

## What it does (pure mockery)

- **table_only mode**: Sneakily nudges your estimates juuuust enough to cross that sacred alpha threshold, then back-calculates all the stats and CIs like nothing happened. Because faking it till you make it is the academic way.
- **lm_synthetic_y mode**: Exclusive to lm objects – conjures up a fake-ass response variable that keeps your residuals intact but shoves those coefficients over the significance line. It's like giving your data a little "enhancement" surgery.

Default shenanigans:
- Feed it an lm object? Boom, you get a juiced-up lm back (mode = "auto").
- Anything else? A shiny shift8_table that's faker than a politician's promise.
- That alpha you specify? It's just the ceiling – shift8 rolls the dice with a random Uniform(0, alpha) draw for each coefficient. Because consistency is for losers.
- By default, coefficients stay unchanged; shift8 squeezes standard errors to make the stars appear (move = "se").
- Want reproducible chaos? Pass a seed.
## Installation (GitHub)

```r
install.packages("remotes")
remotes::install_github("yaakovch/shift8")
```

## Functions

shift8(model, ...)
- Main entry point. Returns a shifted model or a coherent table depending on mode.
- mode = "auto" selects lm_synthetic_y for lm objects and table_only otherwise.
- mode = "table_only" returns a shift8_table (tibble + metadata).
- mode = "lm_synthetic_y" returns a shift8_lm (a real lm refit with attributes).
- alpha is sampled uniformly from 0 to the provided upper bound for each coefficient.
- move = "se" keeps coefficients fixed and shrinks standard errors; move = "beta" nudges coefficients.
- seed makes the alpha draws reproducible.

shift8_lucky_stars(model, ...)
- Tries multiple inference variants (one-sided, z, HC0–HC3 if available) and picks the best.
- Returns a shift8_table with the chosen method recorded in metadata.

as_tibble(shift8_table)
- Extracts the tidy table from a shift8_table.

as_modelsummary(shift8_table)
- Produces a modelsummary-friendly table (requires the modelsummary package).

shift8_report(x)
- Returns a satirical “fraud diff” report with deltas and star counts.

plot_shift8(x)
- Quick diagnostics plot comparing original vs shifted SEs and p-values.

validate_shift8(x)
- Validates internal consistency for shift8_table or shift8_lm objects.
- Errors if derived values (t/z, p, CI) do not match their generators.

## Example (satirical)

```r
fit <- lm(mpg ~ wt + hp, data = mtcars)
shifted <- shift8(fit, mode = "lm_synthetic_y", scope = "non_intercept")
shifted_table <- shift8(fit, mode = "table_only")
```
