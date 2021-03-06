% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TimeSeries.R
\name{busday}
\alias{busday}
\title{Subset time-series to business day only}
\usage{
busday(x, year_start = 1970, year_end = 2030)
}
\arguments{
\item{x}{xts object}

\item{year_start}{beginning year to create business days}

\item{year_end}{ending year to create business days}
}
\value{
\code{x} with dates and corresponding values for business days
}
\description{
Subset time-series to business day only
}
\details{
In order to find business days this function creates a calendar
from timeDate's holidayNYSE calendar. The \code{year_start} and \code{year_end}
determine the length of the calendar which needs to be greater or equal to
the \code{x} time-series.
}
\examples{
busday(x, 1900, 2020)
busday(x, year_end = 2050)
}
