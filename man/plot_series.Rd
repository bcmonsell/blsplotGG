% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_series.r
\name{plot_series}
\alias{plot_series}
\title{Plot time series object.}
\usage{
plot_series(
  this_series = NULL,
  this_series_name = NULL,
  main_title = NULL,
  sub_title = NULL,
  this_y_label = NULL,
  y_limit = NULL,
  this_x_label = "Time",
  start_plot = NULL,
  do_grid = FALSE,
  do_background = FALSE,
  draw_recess = FALSE,
  recess_color = "lightgrey",
  recess_sub = TRUE,
  this_line_type = "solid",
  line_color = "grey"
)
}
\arguments{
\item{this_series}{Time series object;
This is a required entry.}

\item{this_series_name}{Character string; name of time series.
No default.}

\item{main_title}{Character string; main title of plot.
A title will be generated if no title is specified.}

\item{sub_title}{Character string; subtitle of plot.
There is no default subtitle.}

\item{this_y_label}{Character string; y-axis label for plot.
If not specified, set to \code{this_series_name}, if specified.}

\item{y_limit}{Numeric vector of length 2;
Range of values you wish the plot to be plotted over.
Default is range of the series specified.}

\item{this_x_label}{Label for X axis. Default is \code{"Time"}.}

\item{start_plot}{Integer vector of length 2;
Starting date for plot. Default is starting date for the time series.}

\item{do_grid}{Logical scalar; indicates if certain plots will have grid lines.
Default is no grid lines.}

\item{do_background}{Logical scalar; indicates grey background included in plot.
Default is no grey background;}

\item{draw_recess}{Logical scalar; indicates if certain plots will have
shaded areas for NBER recession dates.
Default is no recession shading.}

\item{recess_color}{Character string; color used for shading of recession region.
Default is \code{'lightgrey'}.}

\item{recess_sub}{Logical scalar; indicates if x-axis label for recession is
produced for this plot.
Default is x-axis label.}

\item{this_line_type}{Character string; indicates line type of each plot produced.
Default is "solid".}

\item{line_color}{Character string; color used for series in the plot.
Default is \code{'grey'}.}
}
\value{
Generate \code{ggplot} plot of user-specified series.
If series not specified, print out error message and return NULL.
}
\description{
Generate plot of user-specified time series (ts) object.
}
\details{
Version 2.4, 8/26/2024
}
\examples{
air_seas <- 
  seasonal::seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)", x11="",
                 transform.function = "log")
air_seas_d11 <- seasonal::final(air_seas)
air_d11_p <- plot_series(air_seas_d11, this_series_name = "AirPassengers", 
         main_title = 'X-11 Seasonal Adjustment of Airline Passengers',
         sub_title = 'Box-Jenkins Airline series',
         do_grid = TRUE, draw_recess = TRUE, line_color = "darkblue")
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
