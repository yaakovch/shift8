\name{shift8}
\alias{shift8}
\title{Satirical regression star installer}
\description{
Satirically "installs stars" by nudging model outputs while keeping internal table
coherence. For entertainment purposes only.
}
\usage{
shift8(
  model,
  target = "*",
  alpha = 0.05,
  scope = c("all", "non_intercept", "selected"),
  mode = c("auto", "table_only", "lm_synthetic_y"),
  move = c("se", "beta"),
  objective = c("min_coeff_change", "min_fitted_change"),
  keep = c("sign", "none"),
  watermark = TRUE,
  terms = NULL,
  seed = NULL
)
}
\arguments{
\item{model}{A fitted model object.}
\item{target}{Star target levels to enforce.}
\item{alpha}{Upper bound for the satirical significance level; a different alpha is drawn uniformly from 0 to this value for each coefficient on each call.}
\item{scope}{Which terms to adjust.}
\item{mode}{\code{auto} chooses \code{lm_synthetic_y} for lm objects and \code{table_only} otherwise.}
\item{move}{\code{se} keeps coefficients fixed and shrinks standard errors; \code{beta} nudges coefficients.}
\item{objective}{Objective used for lm synthetic refits.}
\item{keep}{Whether to preserve coefficient signs.}
\item{watermark}{Whether to print a satirical watermark.}
\item{terms}{Optional vector of term names for \code{scope = "selected"}.}
\item{seed}{Optional integer seed for reproducible alpha draws.}
}
\value{
A \code{shift8_table} object for \code{mode = "table_only"} or a \code{shift8_lm}
object for \code{mode = "lm_synthetic_y"}.
}
\details{
This function is satire and should not be used for real inference. It is intended
to mock p-hacking and encourage meaningful research questions.
}
\examples{
fit <- lm(mpg ~ wt + hp, data = mtcars)
shift8(fit, mode = "table_only")
shift8(fit, mode = "lm_synthetic_y", scope = "non_intercept")
}
