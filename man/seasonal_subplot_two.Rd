% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seasonal_subplot_two.r
\name{seasonal_subplot_two}
\alias{seasonal_subplot_two}
\title{Seasonal sub-plot for two sets of seasonal factors}
\usage{
seasonal_subplot_two(
  this_sf = NULL,
  this_sf_two = NULL,
  this_sf_range = NULL,
  main_title = "Seasonal Subplot",
  sub_title = NULL,
  this_x_label = NULL,
  this_y_label = NULL,
  this_sf_label = NULL,
  this_sf_label_two = NULL,
  subplot_color = c("blue", "lightblue", "red", "pink")
)
}
\arguments{
\item{this_sf}{Time series of seasonal factors from X-11 or SEATS}

\item{this_sf_two}{Time series of seasonal factors from X-11 or SEATS}

\item{this_sf_range}{Range of values you wish the plot to be plotted over. Default is range of the series.}

\item{main_title}{Title for the plot. Default is character string \code{'Ratio Plot'}.}

\item{sub_title}{Subtitle for the plot. Default is \code{NULL}.}

\item{this_x_label}{Label for X axis. Default is \code{NULL}.}

\item{this_y_label}{Label for Y axis. Default is \code{NULL}.}

\item{this_sf_label}{Character scalar, provides a brief description of the
first seasonal factors. Default is \code{NULL}.}

\item{this_sf_label_two}{Character scalar, provides a brief description of the
second seasonal factors. Default is \code{NULL}.}

\item{subplot_color}{Character vector of length four, setting color used for
lines in ratio plot in the order of first factor, first factor mean,
second factor, second factor mean.
Default is \code{c("blue", "lightblue", "red", "pink")}.}
}
\value{
A \code{ggplot} object that generates a ratio plot.
}
\description{
Generates a seasonal sub-plot from two ts objects of seasonal (or combined adjustment) factors
}
\details{
Version 1.8, 9/25/2024
}
\examples{
ukgas_x11_seas <- 
   seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
                  transform.function = "log", forecast.maxlead = 20,
                  x11.seasonalma = "s3x5", 
                  check.print = c( 'pacf', 'pacfplot' ))
ukgas_seats_seas <- 
   seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
                  transform.function = "log", forecast.maxlead = 20,
                  seats.finite = "yes", seats.save = "s10",
                  check.print = c( 'pacf', 'pacfplot' ))
ukgas_x11_sf     <- seasonal::series(ukgas_x11_seas, "d10")
ukgas_seats_sf   <- seasonal::series(ukgas_seats_seas, "s10")
p_ukgas_sf_sub   <- 
    seasonal_subplot_two(ukgas_x11_sf, ukgas_seats_sf,
                     main_title = "UK Gas Seasonal Subplots",
                     sub_title = "X-11 and SEATS Seasonal Factors", 
                     this_x_label = "Quarter",
                     this_sf_label = "X-11",
                     this_sf_label_two = "SEATS")
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
