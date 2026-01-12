\name{validate_shift8}
\alias{validate_shift8}
\title{Validate shift8 objects}
\description{
Checks that shifted outputs are internally consistent.
}
\usage{
validate_shift8(x, ...)
}
\arguments{
\item{x}{A \code{shift8_table} or \code{shift8_lm} object.}
\item{...}{Additional arguments passed to methods.}
}
\value{
\code{TRUE} invisibly if validation passes; otherwise an error.
}
\examples{
fit <- lm(mpg ~ wt + hp, data = mtcars)
shifted <- shift8(fit, mode = "table_only")
validate_shift8(shifted)
}
