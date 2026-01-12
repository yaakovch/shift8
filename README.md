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

validate_shift8(x)
- Validates internal consistency for shift8_table or shift8_lm objects.
- Errors if derived values (t/z, p, CI) do not match their generators.

## Example (satirical)

```r
fit <- lm(mpg ~ wt + hp, data = mtcars)
shifted <- shift8(fit, mode = "lm_synthetic_y", scope = "non_intercept")
shifted_table <- shift8(fit, mode = "table_only")
```
