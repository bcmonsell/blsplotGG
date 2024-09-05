#' Maximum absolute value of a vector
#'
#' Generates the maximum of the absolute value of a numeric vector.
#'
#' Version 1.1, 3/29/2021
#'
#' @param x vector of numbers
#' @return Maximum of the absolute value of a vector
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' r50 <- rnorm(50)
#' r50.absmax <- absmax(r50)
#' @export
absmax <- function(x) {
    # Author: Brian C. Monsell (OEUS) Version 1.3, 3/29/2021
    return(max(abs(x)))
}
