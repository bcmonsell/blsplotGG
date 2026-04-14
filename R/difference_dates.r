#' Date difference
#'
#' Generate the difference between two dates.
#'
#' Version 3.2, 1/7/2026
#'
#' @param start_date Integer array of length 2, a date where the first element is the year and 
#'        the second element is the month or quarter. This is a required entry.
#' @param end_date Integer array of length 2, a date to comapare to \code{this_date}.
#'        This is a required entry.
#' @param this_frequency Integer scalar, frequency of target time series.
#'        Entries limited to \code{1} (yearly), \code{2} (biannual), \code{3} (triannual),  
#'        \code{4} (quarterly), \code{6} (Bimonthly), \code{365} (daily) and \code{12}. 
#'        Default is \code{12} (monthly). 
#' @param include_start Logical scalar; if \code{TRUE}, include the starting date in the difference,
#'        otherwise, the difference is left as computed. Default is \code{TRUE}.
#' @param return_absolute Logical scalar; if \code{TRUE}, returns the absolute value of the 
#'        number of days if the difference is negative
#' @return Integer scalar; difference between first date and second date.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' diff_start <- difference_dates(c(1990,1), c(1995,1))
#' @export
difference_dates <- 
	function(start_date = NULL, 
			 end_date = NULL, 
			 this_frequency = 12,
			 include_start = TRUE,
			 return_absolute = FALSE) {
    # Author: Brian C. Monsell (OEUS), Version 3.2, 1/7/2026
	
    # check if a value is specified for \code{start_date}
    if (is.null(start_date)) {
        stop("no date specified for start_date")
    }
    
    # check if a value is specified for \code{end_date}
    if (is.null(end_date)) {
        stop("no date specified for end_date")
    }
    
    # check if length of the arguments are the same.
    if (length(start_date) != length(end_date)) {
        stop(paste("Lengths of arguments not compatable - ", length(start_date), 
                   length(end_date), sep = " "))
    }
	
    # check if frequency is valid.
	freq_set <- c(1,2,3,4,6,12,365)
	this_match <- this_frequency %in% freq_set
	if (!this_match) {
		cat(paste0("Improper entry for this_frequency: ", this_frequency))
		stop(paste("Valid entries are ", freq_set))
	}
	
    # Generate starting and ending date objects.
	if (this_frequency == 365) {
		start_date_object <- set_day_and_month(start_date[2], start_date[1])
		end_date_object   <- set_day_and_month(end_date[2], end_date[1])
	} else {
		if (this_frequency == 12) {
			start_date_string  <- stringr::str_pad(start_date[2], 2, pad = "0")
			end_date_string <- stringr::str_pad(end_date[2], 2, pad = "0")
		} else {
			start_period       <- ((12 / this_frequency) * (start_date[2] - 1)) + 1
			start_date_string  <- stringr::str_pad(start_period, 2, pad = "0")
			end_period      <- ((12 / this_frequency) * (end_date[2] - 1)) + 1
			end_date_string <- stringr::str_pad(end_period, 2, pad = "0")
		}
		start_date_object  <- 
			lubridate::ymd(paste0(start_date[1],   "-", start_date_string,  "-01"))
		end_date_object    <- 
			lubridate::ymd(paste0(end_date[1],  "-", end_date_string, "-01"))
	}
	
    # Generate difference between date objects.
	if (this_frequency == 365) {
		nd <- as.integer(end_date_object - start_date_object)
		if (nd < 0) {
			if (return_absolute) {
				nd <- abs(nd)
			}
		}
		if (include_start) { 
			nd <- nd + 1
		}
		return(as.integer(nd))
	} else {
		months_diff <- lubridate::interval(start_date_object, end_date_object) %/% months(1)
		if (months_diff < 0) {
			if (include_start) { 
				months_diff <- months_diff - 1
			}
			if (return_absolute) {
				months_diff <- abs(months_diff)
			}
		} else {
			if (include_start) { 
				months_diff <- months_diff + 1
			}
		}
		if (this_frequency == 12) {
			return(months_diff)
		} else {
			this_div <- 12 / this_frequency
			return(months_diff / this_div)
		}
	}
}