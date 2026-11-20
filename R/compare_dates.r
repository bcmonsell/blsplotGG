#' Date Match
#'
#' Compare two dates to see if they match. The dates are integer strings, where 
#' the first element is the year and the second element is the month or quarter.
#'
#' Version 3.3, 2026-02-24
#'
#' @param this_date Integer array of length 2, a date where the first element 
#'        is the year and the second element is the month or quarter. 
#'        This is a required entry.
#' @param comp_date Integer array of length 2, a date to comapare to \code{this_date}.
#'        This is a required entry.
#' @return Logical scalar; \code{TRUE} if the dates match, 
#'         \code{FALSE} if they don't.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' match_start <- compare_dates(start(shoes2007), c(1990,1))
#' @export
compare_dates <- function(this_date = NULL, comp_date = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 3.3, 2026-02-24
    
    # check if a value is specified for \code{this_date}
    if (is.null(this_date)) {
        stop("no date specified for this_date")
    }
    
    # check if a value is specified for \code{comp_date}
    if (is.null(comp_date)) {
        stop("no date specified for comp_date")
    }
    
    # check if length of the arguments are the same.
    if (length(this_date) != length(comp_date)) {
        stop(paste("Lengths of arguments not compatable - ", length(this_date), 
                   length(comp_date), sep = " "))
    }
    
    # get number of elements that match
    x_date <- sum(this_date == comp_date)
    
    # If both elements match, return \code{TRUE}; else, return \code{FALSE}.
    if (x_date == length(this_date)) {
        return(TRUE)
    }
    return(FALSE)
}
