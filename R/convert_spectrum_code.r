#' Convert spectrum code to visual peak code
#'
#' Convert the three character code used by the \code{plot_spectrum} function so that it can be used with the 
#' \code{visual_sig_peaks} function from the \code{blsplot} package.
#'
#' Version 1.4, 5/6/2024
#'
#' @param this_spectrum Character string; three character code for the X-13 spectrum to be generated.
#'             Allowed entries are \code{"sp0"} (modified original series), 
#'             \code{"sp1"} (modified X-11 seasonally adjusted series), 
#'             \code{"sp2"} (modified X-11 irregular), 
#'             \code{"s1s"} (modified SEATS seasonally adjusted series), 
#'             \code{"s2s"} (modified SEATS irregular),
#'             \code{"is0"} (modified composite series), 
#'             \code{"is1"} (modified indirect seasonally adjusted series),
#'             \code{"is2"} (modified indirect irregular), \code{spr} (model residuals), or
#'             \code{"ser"} (extended residuals).  Default: \code{"sp0"}.
#' @return Text for spectrum associated with code used in \code{plot_spectrum} function. 
#'         If improper \code{this_spectrum} specified, function will return \code{NULL}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' this_code <- convert_spectrum_code("sp2")
#' @export
convert_spectrum_code <- function(this_spectrum = "sp0") {
    # Author: Brian C. Monsell (OEUS) Version 1.4, 5/6/2024

	this_spectrum_text <- 
	    dplyr::case_when(
            this_spectrum == "sp0" ~ "ori",
            this_spectrum == "sp1" ~ "sa",
            this_spectrum == "sp2" ~ "irr",
            this_spectrum == "s1s" ~ "sa",
            this_spectrum == "s2s" ~ "irr",
            this_spectrum == "is0" ~ "compori",
            this_spectrum == "is1" ~ "indsa",
            this_spectrum == "is2" ~ "indirr",
            this_spectrum == "spr" ~ "rsd",
            this_spectrum == "ser" ~ "extrsd",
            TRUE ~ NA)
	
	if(is.na(this_spectrum_text)) {
             cat("Must enter sp0, sp1, sp2, s1s, s2s, is0, is1, is2, spr, or ser for the this_spectrum argument")
             this_spectrum_text <- NULL
	}
	
	return(this_spectrum_text)
}
