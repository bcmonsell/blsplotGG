#' Get automatic outlier names
#'
#' Get the names of outliers identified in the \code{seas} object for a single series.
#'
#' Version 3.0, 5/14/2024
#'
#' @param seas_obj A \code{seas} object for a single series generated from the 
#'        \code{seasonal} package. This is a required entry.
#' @return Character string containing a summary of the outliers identified in the 
#'         regARIMA model. If no regressors or automatic outliers in the model, the routine 
#'         will return a blank character.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <- seasonal::seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)", x11="")
#' this_auto_outlier <- get_auto_outlier_string(air_seas)
#' @export
get_auto_outlier_string <- function(seas_obj = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 3.0, 5/14/2024
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
           stop("First argument must be a seas object")
        }
    }
    
    # Extract number of regressors, number of automatic outliers
    this_nreg <- seasonal::udg(seas_obj, "nreg")
    this_auto_outlier <- seasonal::udg(seas_obj, "autoout")
    
    # Initialize string of names
    this_string <- " "
    
    # If number of regressors equals number of automatic outliers, extract names of outliers
    if (this_nreg == this_auto_outlier) {
        this_string <- get_reg_string(seas_obj)
    } else {
        # If number of automatic outliers greater than zero, get the position of the 
        # \code{nreg} code in UDG data
        if (this_auto_outlier > 0) {
            this_reg_index <- get_udg_index(seasonal::udg(seas_obj), "nreg")
            
            # get UDG keywords for individual regressors
            this_reg_names <- names(seas_obj$udg)[seq(1, this_nreg) + this_reg_index]
            
            # split keywords into regressor names and types
            reg_vec <- unlist(strsplit(this_reg_names, "[$]"))[seq(2, 2 * this_nreg, 2)]
            reg_type_vec <- unlist(strsplit(this_reg_names, "[$]"))[seq(1, 2 * this_nreg, 2)]
            
            # extract names of automatic outliers into \code{this_string}
            this_string <- paste(reg_vec[reg_type_vec == "AutoOutlier"], collapse = " ")
        }
    }
    return(this_string)
}
