\name{plot_shift8}
\alias{plot_shift8}
\title{Plot shift8 diagnostics}
\description{
Plots before/after shifts in standard errors and p-values.
}
\usage{
plot_shift8(x, ...)
}
\arguments{
\item{x}{A \code{shift8_table} or \code{shift8_lm} object.}
\item{...}{Additional arguments passed to methods.}
}
\examples{
fit <- lm(mpg ~ wt + hp, data = mtcars)
shifted <- shift8(fit, mode = "table_only")
plot_shift8(shifted)
}
