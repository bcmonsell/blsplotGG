#' Generate index of month abbreviation
#'
#' Process string of month abbreviation to return a numeric index 
#' (1 for "Jan", 2 for "Feb", etc.).
#'
#' Version 3.1, 2026-06-15
#'
#' @param this_month_string Character string; 3 character abbreviation of month.
#' @param full_name Logical scalar; indicates if this is the full name of the month
#' @return Index of month - 1 for \code{'Jan'}, 2 for \code{'Feb'}, etc.
#'         If results do not match, this function will return \code{0}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' thisOtl <- 'AO2015.Jan'
#' thisCode <- 'AO'
#' thisPerChar <- substr(thisOtl,nchar(thisCode)+6,nchar(thisOtl))
#' thisPerIndex <- get_month_index(thisPerChar)
#' @export
get_month_index <- function(this_month_string, full_name = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 3.1, 2026-06-15
    
    # Initialize monthly abbreviations, index
	if (full_name) {
		mVec <- month.name
	} else {
		mVec <- month.abb
	}
    mInd <- 1:12
    
    # return index of month abbreviation
	this_m <- mInd[!is.na(match(tolower(mVec), tolower(this_month_string)))]
	
	if (length(this_m) > 0) {
	  return(this_m)
	} else {
	  return(0)
	}

}
