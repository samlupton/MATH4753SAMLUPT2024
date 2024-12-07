% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MyBoot.R
\name{myboot2}
\alias{myboot2}
\title{The My Boot Function}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{Integer. The number of bootstrap iterations to perform. Default is 10000.}

\item{x}{Numeric vector. The original sample data for which the bootstrap procedure is applied.}

\item{fun}{Character or function. The function to be applied to each bootstrap sample (e.g., "mean", "median").
Default is "mean".}

\item{alpha}{Numeric. The significance level for constructing the confidence interval. Default is 0.05.}

\item{cx}{Numeric. The character expansion (cex) value used for displaying text in the plot. Default is 1.5.}

\item{...}{Additional graphical parameters to be passed to the `hist` function for customizing the histogram plot.}
}
\value{
A list containing the following elements:
        \item{ci}{The bootstrap confidence interval for the specified statistic.}
        \item{fun}{The function used to calculate the sample statistic.}
        \item{x}{The original sample data.}
}
\description{
This function performs a parametric bootstrap procedure for a specified sample statistic (default is the mean).
It generates bootstrap samples, calculates the bootstrap distribution of the sample statistic, and provides a
confidence interval. A histogram of the bootstrap sample statistics is displayed along with the confidence
interval and the point estimate.
}
\examples{
# Example usage:
set.seed(123)
sample_data <- rnorm(50, mean = 5, sd = 2)
myboot2(iter = 1000, x = sample_data, fun = "mean", alpha = 0.05, cx = 1.2)

}
