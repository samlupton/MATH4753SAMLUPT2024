% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quadratic.R
\name{quadratic}
\alias{quadratic}
\title{Quadratic Function}
\usage{
quadratic(x, coefficients)
}
\arguments{
\item{x}{A numeric vector or scalar representing the input values for which the quadratic function should be evaluated.}

\item{coefficients}{A numeric vector of length 3 containing the coefficients for the quadratic function.
The first element is the intercept, the second is the coefficient for the linear term, and the third
is the coefficient for the quadratic term.}
}
\value{
A numeric vector or scalar representing the computed values of the quadratic function for the input `x`.
}
\description{
Quadratic Function
}
\examples{
quadratic(x=1:10, coefficients = c(2, 3, -1))

}
