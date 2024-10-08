% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_outlier_label.r
\name{gen_outlier_label}
\alias{gen_outlier_label}
\title{generate x-axis label for outliers}
\usage{
gen_outlier_label(
  seas_obj = NULL,
  this_color = c("red", "blue", "green", "brown", "grey", "yellow")
)
}
\arguments{
\item{seas_obj}{\code{seas} object generated from a call of \code{seas} on a single time series.
This is a required entry.}

\item{this_color}{Character array of length 6; color used for different outliers, with the order being
\code{"ao", "ls", "tc", "so", "rp", "tls"}.
Default is \code{c("red", "blue", "green", "brown", "grey", "yellow")}.}
}
\value{
Character string with description of outliers for x-axis label of a \code{ggplot}.
}
\description{
Generate an x-axis label when adding lines for outliers to a \code{ggplot} plot object
}
\details{
Version 1.1, 5/15/2024
}
\examples{
air_seas <- 
   seasonal::seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)", x11 = "")
air_df   <- 
   data.frame(date = tsbox::ts_df(AirPassengers)$time, 
              ori = as.double(seasonal::original(air_seas)),  
              sa = as.double(seasonal::final(air_seas)), 
              trend = as.double(seasonal::trend(air_seas)))
this_p <- ggplot2::ggplot(air_df) +
  ggplot2::geom_line(ggplot2::aes(x=date, y = ori), color = "grey") + 
  ggplot2::geom_line(ggplot2::aes(x=date, y = sa), color="steelblue", linetype="twodash") + 
  ggplot2::geom_line(ggplot2::aes(x=date, y = trend), color="darkred", linetype="twodash") + 
  ggplot2::labs(
    title = "Airline Passenger X-11 Seasonal Adjustment",
    subtitle = NULL,
    y = "Airline Passengers")
this_p_with_outlier_lines <- add_outlier_lines(this_p, air_seas)
outlier_lines_label <- gen_outlier_label(air_seas)
this_p_with_outlier_lines <- 
    this_p_with_outlier_lines + ggplot2::xlab(outlier_lines_label)
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
