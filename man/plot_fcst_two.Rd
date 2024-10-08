% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_fcst_two.r
\name{plot_fcst_two}
\alias{plot_fcst_two}
\title{Forecast plot for two models}
\usage{
plot_fcst_two(
  seas_obj_one = NULL,
  seas_obj_two = NULL,
  main_title = "ARIMA forecasts",
  sub_title = NULL,
  name_mdl1 = "Model 1",
  name_mdl2 = "Model 2",
  this_x_label = "Time",
  this_y_label = " ",
  length_ori = 2,
  do_grid = FALSE,
  do_background = FALSE,
  this_palette = c("darkgrey", "steelblue", "darkgreen"),
  this_guide_legend = "Forecast"
)
}
\arguments{
\item{seas_obj_one}{\code{seas} object generated from a call of \code{seas}
on a single time series
This is a required entry.}

\item{seas_obj_two}{\code{seas} object generated from a call of \code{seas}
on the same time series, but a different regARIMA model.
This is a required entry.}

\item{main_title}{Character string; main title of plot.
Default is \code{'ARIMA Residuals'}.}

\item{sub_title}{Subtitle for the plot.
Default is to generate the subtitle.}

\item{name_mdl1}{Character string; Description of first model for use in the subtitle.
Default is \code{'Model 1'}.}

\item{name_mdl2}{Character string; Description of second model for use in the subtitle.
Default is \code{'Model 2'}.}

\item{this_x_label}{Label for X-axis.  Default is \code{"Time"}}

\item{this_y_label}{Label for Y-axis.  Default is \code{" "}}

\item{length_ori}{Integer scalar; number of years of the original series
to show with forecasts. #'        Default is 2 years.}

\item{do_grid}{Logical scalar; indicates if certain plots will have grid lines.
Default is no grid lines.}

\item{do_background}{Logical scalar; indicates grey background included in plot.
Default is no grey background;}

\item{this_palette}{Array of character strings; color used for original series,
forecast, and upper and lower forecast bounds.
Default is \code{c("darkgrey", "steelblue", "darkgreen")}.}

\item{this_guide_legend}{Title for legend.  Default is \code{"Forecast"}}
}
\value{
A \code{ggplot} object of the regARIMA forecasts for two models of the same series.
}
\description{
Generates plot that compares regARIMA forecasts for two models of the same series
}
\details{
Version 1.6, 8/28/2024
}
\examples{
shoes_seas_one <- 
    seasonal::seas(shoes2008, x11="", slidingspans = "", transform.function = "log",
                   arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL, 
                   outlier = NULL, forecast.maxlead = 60, forecast.save = "fct",
                   check.print = c( 'pacf', 'pacfplot' ), series.save = "a1")
shoes_seas_two <- 
    seasonal::seas(shoes2008, x11="", slidingspans = "", transform.function = "log",
                   arima.model = "(0 1 1)(0 1 1)", regression.variables = c("td"),
                   forecast.maxlead = 60, forecast.save = "fct", 
                   check.print = c( 'pacf', 'pacfplot' ), series.save = "a1")
shoes_fcst_two_p <- 
    plot_fcst_two(shoes_seas_one, shoes_seas_two,
         main_title = "Forecast Comparison Plot", 
         name_mdl1 = 'Airline', name_mdl2 = 'Airline + reg',
         do_grid = TRUE)
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
