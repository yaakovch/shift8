\name{as_modelsummary}
\alias{as_modelsummary}
\title{Convert shift8 tables for modelsummary}
\description{
Converts a \code{shift8_table} into a modelsummary table object.
}
\usage{
as_modelsummary(x, ...)
}
\arguments{
\item{x}{A \code{shift8_table} object.}
\item{...}{Additional arguments passed to \code{modelsummary::datasummary_df}.}
}
\value{
A modelsummary table object.
}
\examples{
\dontrun{
fit <- lm(mpg ~ wt + hp, data = mtcars)
shifted <- shift8(fit, mode = "table_only")
as_modelsummary(shifted)
}
}
