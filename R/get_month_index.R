#' Generate index of month abbreviation
#'
#' Process string of month abbrev to return a numeric index.
#'
#' Version 2.3, 9/18/2020
#'
#' @param this_month_string Character string; 3 character abbreviation of month
#' @return Index of month - 1 for 'Jan', 2 for 'Feb', etc.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' thisOtl <- 'AO2015.Jan'
#' thisCode <- 'AO'
#' thisPerChar <- substr(thisOtl,nchar(thisCode)+6,nchar(thisOtl))
#' thisPerIndex <- get_month_index(thisPerChar)
#' @export
get_month_index <- function(this_month_string) {
    # Author: Brian C. Monsell (OEUS) Version 2.3, September 18, 2020
    
    # Initialize monthly abbreviations, index
    mAbb <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    mInd <- 1:12
    
    # return index of month abbreviation
    return(mInd[!is.na(match(tolower(mAbb), tolower(this_month_string)))])
}
