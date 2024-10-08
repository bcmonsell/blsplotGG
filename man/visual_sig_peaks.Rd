% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visual_sig_peaks.R
\name{visual_sig_peaks}
\alias{visual_sig_peaks}
\title{Flag visual significant peaks in spectra}
\usage{
visual_sig_peaks(seas_obj = NULL, spec_type = "sa", spec_freq_code = "seas")
}
\arguments{
\item{seas_obj}{\code{seas} object generated from a call of \code{seas} on a single time series
This entry is required.}

\item{spec_type}{Character string; type of spectrum. Possible values are \code{'ori'}, \code{'irr'}, \code{'rsd'},
\code{'sa'}, \code{'comp'}, \code{'indsa'}, \code{'indirr'}, \code{'extrsd'}. Default is \code{'sa'}.}

\item{spec_freq_code}{Character string; type of frequency being tested. Possible values are \code{'seas'} or \code{'td'}.
Default is \code{'seas'}.}
}
\value{
If visually significant peaks found, a numveric vector of the position of the peak frequecies. If no peaks found, 0.
}
\description{
Determine positions of visual significant peaks in spectra
}
\details{
Version 3.4, 5/14/2024
}
\examples{
air_seas <- seasonal::seas(AirPassengers, arima.model = '(0 1 1)(0 1 1)', x11='')
vp_ori_seas <- visual_sig_peaks(air_seas, spec_type = 'ori')
vp_ori_td   <- visual_sig_peaks(air_seas, spec_type = 'sa',
                                spec_freq_code = 'td')
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
