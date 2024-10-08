% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_sadj_and_trend.r
\name{plot_sadj_and_trend}
\alias{plot_sadj_and_trend}
\title{Plot X-13 seasonal adjustment, trend on same axis}
\usage{
plot_sadj_and_trend(
  seas_obj = NULL,
  plot_ori = TRUE,
  main_title = NULL,
  sub_title = NULL,
  this_x_label = "Time",
  this_y_label = " ",
  do_grid = FALSE,
  do_background = FALSE,
  line_color = NULL,
  this_palette = "Dark2",
  this_guide_legend = "Series"
)
}
\arguments{
\item{seas_obj}{\code{seas} object generated from a call of \code{seas} on a single time series
This is a required entry.}

\item{plot_ori}{Logical scalar, indicates if original series is included with plot.
Default is \code{TRUE}.}

\item{main_title}{Title for the plot.
By default, the routine will generate a trend based on the
type of adjustment (X-11 and SEATS) done.}

\item{sub_title}{Subtitle for the plot. Optional entry.}

\item{this_x_label}{Label for X-axis.  Default is \code{"Time"}}

\item{this_y_label}{Label for Y-axis.  Default is \code{" "}}

\item{do_grid}{Logical scalar; indicates if certain plots will have grid lines.
Default is no grid lines.}

\item{do_background}{Logical scalar; indicates grey background included in plot.
Default is no grey background;}

\item{line_color}{Character vector of length 2 (if \code{plot_ori} is \code{FALSE})
or 3 (if \code{plot_ori} is \code{TRUE}); color used for lines in the plot,
in the order of seasonally adjusted series, trend, original series.
Default is generated from the \code{RColorBrewer} palette \code{"Dark2"}.}

\item{this_palette}{Character string; default \code{RColorBrewer} palette.
Deault is \code{"Dark2"}.}

\item{this_guide_legend}{Title for legend.  Default is \code{"Series"}}
}
\value{
A \code{ggplot} object that generates a plot comparing a
seasonally adjusted series with the trend generated from the same
X-13ARIMA-SEATS seasonal adjustment.
}
\description{
Generates a \code{ggplot} object with a time series plot that compares an X-13
seasonal adjustment and trend, optionally including the original series.
}
\details{
Version 1.6, 9/19/2024
}
\examples{
ukgas_x11_seas   <- 
   seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
                  transform.function = "log", forecast.maxlead = 20,
                  x11.seasonalma = "s3x5", x11.save = "d12",
                  check.print = c( 'pacf', 'pacfplot' ))
ukgas_x11_sadj_and_trend_p <- 
    plot_sadj_and_trend(ukgas_x11_seas, plot_ori = TRUE,
                        main_title = "UK Gas",
                        sub_title = "X-11 Seasonal Adjustment",
                        line_color = c("steelblue", "forestgreen", "lightgrey"))
ukgas_seats_seas <- 
   seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
                  transform.function = "log", forecast.maxlead = 20,
                  seats.save = "s12",
                  check.print = c( 'pacf', 'pacfplot' ))
ukgas_seats_sadj_and_trend_p <- 
    plot_sadj_and_trend(ukgas_seats_seas, plot_ori = FALSE, 
                        main_title = "UK Gas Series",
                        sub_title = "SEATS Seasonal Adjustment",
                        line_color = c("steelblue", "forestgreen"))
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
