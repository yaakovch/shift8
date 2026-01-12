\name{shift8_lucky_stars}
\alias{shift8_lucky_stars}
\title{Satirical method shopping for stars}
\description{
Tries multiple inference variants and picks the one that yields the most stars.
This is satire and should not be used for real inference.
}
\usage{
shift8_lucky_stars(
  model,
  target = "*",
  alpha = 0.05,
  scope = c("all", "non_intercept", "selected"),
  keep = c("sign", "none"),
  watermark = TRUE,
  terms = NULL,
  seed = NULL,
  methods = c("standard", "one_sided", "z", "hc0", "hc1", "hc2", "hc3")
)
}
\arguments{
\item{model}{A fitted model object.}
\item{target}{Star target levels to enforce.}
\item{alpha}{Upper bound for the satirical significance level; a different alpha is drawn uniformly from 0 to this value for each coefficient on each call.}
\item{scope}{Which terms to adjust.}
\item{keep}{Whether to preserve coefficient signs (for compatibility).}
\item{watermark}{Whether to print a satirical watermark.}
\item{terms}{Optional vector of term names for \code{scope = "selected"}.}
\item{seed}{Optional integer seed for reproducible alpha draws.}
\item{methods}{Inference variants to try. Robust SE methods require the \code{sandwich} package.}
}
\value{
A \code{shift8_table} object.
}
\details{
Method shopping is intentionally satirical. The package selects the variant that maximizes
significant terms under the sampled alpha thresholds.
}
\examples{
fit <- lm(mpg ~ wt + hp, data = mtcars)
shift8_lucky_stars(fit, scope = "non_intercept")
}
