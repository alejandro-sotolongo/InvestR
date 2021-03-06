% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TimeSeries.R
\name{change_freq_na}
\alias{change_freq_na}
\title{Change frequency in time-series with missing values}
\usage{
change_freq_na(x, period, dtype)
}
\arguments{
\item{x}{xts object}

\item{period}{character to represent desired frequency}

\item{dtype}{character to specify if \code{x} is a 'return' or 'price' 
time-series}
}
\value{
xts object with new \code{period}
}
\description{
Change frequency in time-series with missing values
}
\note{
The missing values are filled with either \code{0} for returns and the
most recent (i.e., lagged) non-missing value for prices.
}
