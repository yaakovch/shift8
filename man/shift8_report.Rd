\name{shift8_report}
\alias{shift8_report}
\title{Generate a satirical shift8 report}
\description{
Summarizes how much a model or table was shifted and how many stars were gained.
}
\usage{
shift8_report(x, ...)
}
\arguments{
\item{x}{A \code{shift8_table} or \code{shift8_lm} object.}
\item{...}{Additional arguments passed to methods.}
}
\value{
A \code{shift8_report} object printed with a short summary.
}
\examples{
fit <- lm(mpg ~ wt + hp, data = mtcars)
shifted <- shift8(fit, mode = "table_only")
shift8_report(shifted)
}
