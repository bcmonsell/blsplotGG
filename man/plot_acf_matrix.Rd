% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_acf_matrix.r
\name{plot_acf_matrix}
\alias{plot_acf_matrix}
\title{Generate ACF plot of the regARIMA model residuals from a matrix of the ACF.}
\usage{
plot_acf_matrix(
  acf_matrix = NULL,
  acf_range = NULL,
  add_ci = TRUE,
  acflimit = 1.6,
  main_title = "ACF Plot",
  sub_title = NULL,
  this_x_label = "Lag",
  this_y_label = "ACF",
  acf_color = "steelblue"
)
}
\arguments{
\item{acf_matrix}{Numeric matrix containing the ACF, PACF, or squared ACF with
columns with SE, Ljung Box Q, lags, if associated with the file.
This is a required entry.}

\item{acf_range}{Range of values you wish the acf plot to be plotted over.
Default is range of the series.}

\item{add_ci}{Logical scalar; indicates if confidence intervals are added to the plot.
Default is confidence intervals are added.}

\item{acflimit}{Numeric scalar; the multiplier for the confidence interval usually read
from the \code{udg}. Default: \code{1.6}.}

\item{main_title}{Title for the plot.
Default is character string \code{'ACF Plot'}.}

\item{sub_title}{Subtitle for the plot. Default is \code{NULL}, or no subtitle.}

\item{this_x_label}{Label for X axis. Default is \code{"Lags"}.}

\item{this_y_label}{Label for Y axis. Default is \code{"ACF"}.}

\item{acf_color}{Color used for lines in ACF plot.
Default is \code{"steelblue"}.}
}
\value{
A \code{ggplot} object that produces an ACF, PACF, or squared ACF plot
of the regARIMA residuals.
}
\description{
Generate ACF, PACF, or squared ACF plot of the regARIMA model residuals
from a matrix of the ACF.
}
\details{
Version 1.3, 10/23/2024
}
\examples{
ukgas_x11_seas <- 
   seasonal::seas(UKgas, series.period = 4, 
                  arima.model = "(0 1 1)(0 1 1)", 
                  transform.function = "log", 
                  forecast.maxlead = 20,
                  x11.seasonalma = "s3x5", 
                  check.print = c( 'pacf', 'pacfplot' ),
                  check.maxlag = 12,
                  check.save = c("acf", "pcf", "ac2"))
ukgas_acf_matrix   <- 
   seasonal::series(ukgas_x11_seas, "acf")
p_ukgas_acf   <- 
   plot_acf_matrix(ukgas_acf_matrix, 
            main_title = "UK Gas Model Squared ACF",
            acf_color = "darkblue")
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
