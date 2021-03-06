% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waffle_time.R
\name{waffle_time}
\alias{waffle_time}
\title{Represent dates in a waffle plot}
\usage{
waffle_time(date_thresholds, date_cats, nrows)
}
\arguments{
\item{date_thresholds}{A vector of class "date". First date is your zero point. Remaining dates upper threshold of era.}

\item{date_cats}{A vector of class "character". Labels for the eras, length one fewer than date_cats}

\item{nrows}{Integer setting the number of rows in your waffle}
}
\value{
ggplot object
}
\description{
Based on Isabella Benabaye's code: https://github.com/isabellabenabaye/life-chart
}
\details{
Function represents date ranges - or eras - using a waffle plot. 
Returns a ggplot object, so can be edited as needed. 
Returned with minimal clutter
}
\examples{
date_thresholds <-
as.Date(
  c(
    "1981-01-01",
    "2008-09-01",
    "2010-09-01",
    "2012-10-01",
    "2016-11-01",
    "2017-03-01",
    "2020-02-01",
    "2021-01-01"
  )
)
date_cats <- c("era1", "era2", "era3", "era4", "era5", "era6", "era7")
waffle_time(date_thresholds, date_cats, 12) 
}
