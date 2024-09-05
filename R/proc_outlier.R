#' Extract dates from outlier text
#'
#' Process name of outlier regressor to extract the dates associated with the outlier
#'
#' Version 2.1, 5/2/2024
#'
#' @param this_outlier Character string; outlier regressor.
#'        This is a required entry.
#' @param this_freq integer scalar; time series frequency. Default is \code{12}.
#' @param add_type logical scalar; determines if type of outlier is added to the output. 
#'        Default is \code{TRUE}.
#' @return List of either year and month/quarter of outlier, or year and month/quarter of 
#'         start and end of outlier
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' air_seas <- 
#'    seasonal::seas(AirPassengers, x11="", slidingspans = "", 
#'                      transform.function = "log", arima.model = "(0 1 1)(0 1 1)", 
#'                      regression.aictest = 'td', forecast.maxlead=36, 
#'                      check.print = c( "pacf", "pacfplot" ))
#' this_auto_outlier <- get_auto_outlier_string(air_seas)
#' this_outlier      <- proc_outlier(this_auto_outlier)
#' @export
proc_outlier <- function(this_outlier = NULL, this_freq = 12, add_type = TRUE) {
    # Author: Brian C. Monsell (OEUS) Version 2.1, 5/2/2024
    
    # check if a value is specified for \code{this_outlier}
    if (is.null(this_outlier)) {
        stop("must specify an outlier")
    }
    
    # generate the code for the outlier
    first_string <- tolower(substr(this_outlier,1,3))
    if (first_string == "aos" | first_string == "lss") {
        this_code <- first_string
    } else {
        first_string <- tolower(substr(this_outlier,1,2))
        if (first_string == "ao" | first_string == "ls" | first_string == "tc" | first_string == "rp" | 
            first_string == "so" | first_string == "tl" | first_string == "qi" | first_string == "qd") {
            this_code <- first_string
        } else {
            stop("must specify an outlier - string must start with ao, ls, tc, so, rp, tl, aos, lss, qi, qd")
        }
    }
    
    # get length of outlier name, outlier code
    this_otl_length <- nchar(this_outlier)
    this_code_length <- nchar(this_code)
        
    # extract outlier year from outlier name
    this_year <- as.numeric(substr(this_outlier, this_code_length + 1, this_code_length + 4))
    
    # if outlier is a ramp or temporary level shift, extract string for the period of the start  
    # and end of outlier as well as the end year
    if (this_code == "tl" | this_code == "rp" | this_code == "aos" | this_code == "lss" | 
        this_code == "qi" | this_code == "qd") {
        this_dash <- regexpr("-", this_outlier)
        this_period_string <- substr(this_outlier, this_code_length + 6, this_dash - 1)
        this_year_2 <- as.numeric(substr(this_outlier, this_dash + 1, this_dash + 4))
        this_period_string_2 <- substr(this_outlier, this_dash + 6, this_otl_length)
    } else {
        # else extract string for the period
        this_period_string <- substr(this_outlier, this_code_length + 6, this_otl_length)
    }
    
    # Convert periods to numeric depending on the period of the time series
    if (this_freq == 4) {
        if (substr(this_period_string, 1, 1) == "q") {
            this_period <- as.numeric(substr(this_period_string, 2, this_otl_length))
        } else {
            this_period <- as.numeric(this_period_string)
        }
        if (this_code == "tl" | this_code == "rp") {
            if (substr(this_period_string, 1, 1) == "q") {
                this_period_2 <- as.numeric(substr(this_period_string_2, 2, this_otl_length))
            } else {
                this_period_2 <- as.numeric(this_period_string_2)
            }
        }
    } else {
        if (nchar(this_period_string) == 3) {
            this_period <- get_month_index(this_period_string)
        } else {
            this_period <- as.numeric(this_period_string)
        }
        if (this_code == "tls" | this_code == "rp") {
            if (nchar(this_period_string_2) == 3) {
                this_period_2 <- get_month_index(this_period_string_2)
            } else {
                this_period_2 <- as.numeric(this_period_string_2)
            }
        }
    }
    
    # return list of year, period of outliers
    if (add_type) {
        if (this_code == "tls" | this_code == "rp"| this_code == "aos" | this_code == "lss" | 
            this_code == "qi" | this_code == "qd") {
            otl_list <- list(year = this_year, period = this_period, year2 = this_year_2, 
                             period2 = this_period_2, type = this_code)
        } else {
            otl_list <- list(year = this_year, period = this_period, type = this_code)
        }
    } else {
        if (this_code == "tls" | this_code == "rp"| this_code == "aos" | this_code == "lss" | 
            this_code == "qi" | this_code == "qd") {
            otl_list <- list(year = this_year, period = this_period, year2 = this_year_2, 
                             period2 = this_period_2)
        } else {
            otl_list <- list(year = this_year, period = this_period)
        }
    }
    
    # if this_freq = 1, check if period = NA and reset to 1
    if (this_freq == 1) {
        if(is.na(otl_list$period)) { otl_list$period = 1 }
    }
    
    return(otl_list)
}
