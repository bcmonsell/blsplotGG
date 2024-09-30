#' Flag visual significant peaks in spectra
#'
#' Determine positions of visual significant peaks in spectra
#'
#' Version 3.4, 5/14/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series
#'                 This entry is required.
#' @param spec_type Character string; type of spectrum. Possible values are \code{'ori'}, \code{'irr'}, \code{'rsd'},
#'                  \code{'sa'}, \code{'comp'}, \code{'indsa'}, \code{'indirr'}, \code{'extrsd'}. Default is \code{'sa'}.
#' @param spec_freq_code Character string; type of frequency being tested. Possible values are \code{'seas'} or \code{'td'}. 
#'                       Default is \code{'seas'}.
#' @return If visually significant peaks found, a numveric vector of the position of the peak frequecies. If no peaks found, 0.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <- seasonal::seas(AirPassengers, arima.model = '(0 1 1)(0 1 1)', x11='')
#' vp_ori_seas <- visual_sig_peaks(air_seas, spec_type = 'ori')
#' vp_ori_td   <- visual_sig_peaks(air_seas, spec_type = 'sa',
#'                                 spec_freq_code = 'td')
#' @export
visual_sig_peaks <- function(seas_obj = NULL, spec_type = "sa", spec_freq_code = "seas") {
    # Author: Brian C. Monsell (OEUS) Version 3.4, 5/14/2024
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
        }
    }

    # check to see if the spectrum type if valid
    spec_type_all <- c("ori", "irr", "rsd", "sa", "comp", "indsa", "indirr", "extrsd")
    if (sum(match(spec_type_all, spec_type), na.rm = TRUE) == 0) {
        print(paste("Spectrum type not valid:", spec_type))
        return(0)
    }
    
    # check to see if the frequency type if valid
    spec_freq_all <- c("seas", "td")
    if (sum(match(spec_freq_all, spec_freq_code), na.rm = TRUE) == 0) {
        print(paste("Frequency type not valid:", spec_freq_code))
        return(0)
    }
    
    if (spec_type == "ori" & spec_freq_code == "seas") {
        # test original series for seasonal frequencies
        this_sig_peak <- flag_peak(seas_obj, "ori", "s")
    } else {
        # construct key for peaks output in UDG file
        this_key <- paste("peaks.", spec_freq_code, sep = "")
        # get tables with seasonal peaks
        this_peak <- seasonal::udg(seas_obj, this_key)
        # see if spec_type among the spectra with peaks if not, return 0
        this_peak_vec <- unlist(stringr::str_split(this_peak, " "))
        if (sum(match(this_peak_vec, spec_type), na.rm = TRUE) == 0) {
            return(0)
        }
        # get this_freq_code, flag peaks
        this_freq_code <- substr(spec_freq_code, 1, 1)
        if (this_freq_code == "s") {
            this_sig_peak <- flag_peak(seas_obj, spec_type, this_freq_code)
        } else {
            this_sig_peak <- flag_peak(seas_obj, spec_type, this_freq_code)
        }
    }
    return(this_sig_peak)
}
