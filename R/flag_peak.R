#' Flag visual significant peaks in spectra
#'
#' Determine positions of visual significant peaks in spectra
#'
#' Version 3.0, 6/21/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series
#'        This is a required argument.
#' @param spec_type Character string; type of spectrum. Possible values are 'ori','irr','rsd','sa'. 
#' @param spec_freq_code Character string; type of frequency being tested. Possible values are 's' or 't'. 
#' @return If visually significant peaks are found, a numeric vector of the position of the peak frequencies are returned. If no peaks found, the function returns 0.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' air_seas <- seasonal::seas(AirPassengers, arima.model = '(0 1 1)(0 1 1)', x11='')
#' this_flagged_peak_seas <- flag_peak(air_seas,'ori','s')
#' this_flagged_peak_td <- flag_peak(air_seas,'ori','t')
#' @export
flag_peak <- function(seas_obj = NULL, spec_type = NULL, spec_freq_code = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 3.0, 6/21/2024
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
        }
    }
    
    if (is.null(spec_freq_code)) {
        stop("must specify either s (for seasonal) or t (for trading day)")
    }
    
    if (is.null(spec_type)) {
        stop("must specify a type of spectrum")
    } 

    if (spec_freq_code == "s") { 
        max_freq <- 5 
    } else {
        if (spec_freq_code == "t") { 
           max_freq <- 2 
        } else {
            stop("must specify either s (for seasonal) or t (for trading day) for spec_freq_code")
        }
    }
    
    # Initialize number of significant peaks (sigPeak), vector of significant peaks (sigvec)
    sigPeak <- 0
    sigvec <- array(0, max_freq)
    
    # extract significance level from udg output
    siglevel <- seasonal::udg(seas_obj, "siglevel")
    
    # process all frequencies
    for (i in 1:max_freq) {
        # construct key and extracing peak info from udg output
        thisKey <- paste("spc", spec_type, ".", spec_freq_code, i, sep = "")
        thisPeak <- seasonal::udg(seas_obj, thisKey)[1]
        
        # if a peak is found see if it is significant
        if (!thisPeak == "nopeak") {
            if (as.numeric(thisPeak) > siglevel) {
                if (length(seasonal::udg(seas_obj, thisKey)) > 1) {
                  sigPeak <- sigPeak + 1
                  sigvec[sigPeak] <- seasonal::udg(seas_obj, paste(spec_freq_code, i, ".index", sep = ""))
                }
            }
        }
    }
    
    # return either 0 if no peaks found or vector of peak indices
    if (sigPeak == 0) {
        return(0)
    } else {
        return(sigvec[1:sigPeak])
    }
}
