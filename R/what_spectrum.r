#' What spectrum is plotted
#'
#' Return which spectrum plot is generated based on the three character code used by the \code{plot_spectrum} function.
#'
#' Version 1.5, 5/6/2024
#'
#' @param this_spectrum Character string; three character code for the X-13 spectrum to be generated.
#'        Allowed entries are \code{"sp0"} (modified original series), 
#'        \code{"sp1"} (modified X-11 seasonally adjusted series), 
#'        \code{"sp2"} (modified X-11 irregular), 
#'        \code{"s1s"} (modified SEATS seasonally adjusted series), 
#'        \code{"s2s"} (modified SEATS irregular),
#'        \code{"is0"} (modified composite series), 
#'        \code{"is1"} (modified indirect seasonally adjusted series),
#'        \code{"is2"} (modified indirect irregular), \code{spr} (model residuals), or
#'        \code{"ser"} (extended residuals).  Default: \code{"sp0"}.
#' @param use_title_case Logical scalar; convert string to title case. Default is FALSE.
#' @return Text for spectrum associated with code used in \code{plot_spectrum} function.
#'         If improper value set for \code{this_spectrum}, function will return \code{NULL}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' sub_title <- what_spectrum("sp2", use_title_case = TRUE)
#' @export
what_spectrum <- function(this_spectrum = "sp0", use_title_case = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 1.5, 5/6/2024

	# generate text describing spectrum plot for specific codes
	this_spectrum_text <- 
	    dplyr::case_when(
            this_spectrum == "sp0" ~ "modified original series",
            this_spectrum == "sp1" ~ "modified X-11 seasonally adjusted series",
            this_spectrum == "sp2" ~ "modified X-11 irregular",
            this_spectrum == "s1s" ~ "modified SEATS seasonally adjusted series",
            this_spectrum == "s2s" ~ "modified SEATS irregular",
            this_spectrum == "is0" ~ "modified composite series",
            this_spectrum == "is1" ~ "modified indirect seasonally adjusted series",
            this_spectrum == "is2" ~ "modified indirect irregular",
            this_spectrum == "spr" ~ "model residuals",
            this_spectrum == "ser" ~ "extended resisuals",
            TRUE ~ NA)
	
	
    if(is.na(this_spectrum_text)) {
        print(paste("Spectrum type not valid:", this_spectrum))
		this_spectrum_text <- NULL
    } else {
	if (use_title_case) {
	    this_spectrum_text <- stringr::str_to_title(this_spectrum_text)
        }
    }
	
    return(this_spectrum_text)
    
}
