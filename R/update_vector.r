#' Update vector.
#'
#' Fill unspecified elements of a vector with the first element of the input series
#'
#' Version 2.3, 5/25/2023
#'
#' @param this_series Original time series. This is a required entry.
#' @param this_num Length of updated series. Must be more than the length of \code{this_series}.
#'        This is a required entry.
#' @return An updated vector of length \code{this_num} augmented with the first value of the 
#'         input series.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' this_vector <- c(1,2)
#' updated_vector <- update_vector(this_vector, 4)
#' @export
update_vector <- function(this_series = NULL, this_num = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 2.3, 5/25/2023
    
    if (is.null(this_series)) {
        stop("Argument this_series must be specified.")
    }
    if (is.null(this_num)) {
        stop("Argument this_num must be specified.")
    }
 
    if (length(this_series) > this_num) {
        stop("value of this_num must be greater than the length of the input series")
    }

    # intialize vector to hold the updates
    rvec <- array(NA, this_num)

    # set the first \code{length(this_series)} to be equal to this_series
    for (i in 1:length(this_series)) {
         rvec[i] <- this_series[i]
    }

    # fill rest of the vector with x[1]
    this_series_1ong <- length(this_series) + 1
    for (i in this_series_1ong:this_num) {
         rvec[i] <- this_series[1]
    }
    return(rvec)

}
