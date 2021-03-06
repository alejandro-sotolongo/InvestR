% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TimeSeries.R
\name{.na_ret}
\alias{.na_ret}
\title{Fill missing returns in a vector}
\usage{
.na_ret(x, rpl)
}
\arguments{
\item{x}{vector of returns}

\item{rpl}{value to replace NA values}
}
\description{
Fill missing returns in a vector
}
\examples{
x <- 1:10
x[3] <- NA
fill_na(x, 0)
}
