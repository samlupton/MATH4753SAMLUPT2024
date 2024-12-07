% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myncurve.R
\name{myncurve}
\alias{myncurve}
\title{The My Curve N Function}
\usage{
myncurve(mu, sigma, a)
}
\arguments{
\item{mu}{Numeric. The mean of the normal distribution.}

\item{sigma}{Numeric. The standard deviation of the normal distribution.}

\item{a}{Numeric. The value up to which the area under the curve is shaded and the cumulative probability is calculated.}
}
\value{
A list with the following elements:
\describe{
  \item{mu}{The mean of the normal distribution.}
  \item{sigma}{The standard deviation of the normal distribution.}
  \item{a}{The value up to which the area is shaded.}
  \item{probability}{The cumulative probability \(P(X ≤ a)\).}
}
}
\description{
This function plots the probability density function (PDF) of a normal distribution
with a given mean (`mu`) and standard deviation (`sigma`). It shades the area under
the curve between `-∞` and a given value `a`. The function also calculates the cumulative
probability \(P(X ≤ a)\) and returns it as part of the output.
}
\examples{
# Example usage of the myncurve function:
result <- myncurve(mu = 0, sigma = 1, a = 1.5)
print(result)

# This will plot the standard normal curve, shade the area up to 1.5,
# and calculate the probability P(X <= 1.5).

}
