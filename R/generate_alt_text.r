#' Generate alt text for ggplot graphs
#'
#' Generates alt text for ggplot graph objects using the \code{BrailleR} package and adding
#' text suggested by Amy Casale in her article "Writing Alt Text for Data Visualization".
#'
#' Version 3.1, 9/9/2024
#'
#' @param gg_object \code{ggplot} object from which alt text will be generated. 
#'                  Required entry if \code{short_alt = FALSE}.
#' @param chart_type character scalar telling what type of plot is used in \code{gg_object}. 
#'                  This is a required entry.
#' @param data_type character scalar detailing what data is used in \code{gg_object}. 
#'                  This is a required entry.
#' @param reason_text character scalar detailing the reason \code{gg_object} is plotted. 
#'                  This is a required entry.
#' @param short_alt logical scalar if TRUE BrailleR text will not be appended to the alt text. 
#'                  Default is \code{FALSE}.
#' @param BrailleR_only logical scalar if TRUE only BrailleR text will returned. 
#'                  Default is \code{FALSE}.
#' @return generate alt text for plot produced by \code{gg_object}
#'
#' 
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#' @references \url{https://CRAN.R-project.org/package=BrailleR} and   \url{https://medium.com/nightingale/writing-alt-text-for-data-visualization-2a218ef43f81}
#'
#' @examples
#' air_seas <- seasonal::seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)", x11="")
#' air_df   <- 
#'    data.frame(date = tsbox::ts_df(AirPassengers)$time, 
#'               ori = as.double(seasonal::original(air_seas)), 
#'               sa = as.double(seasonal::final(air_seas)), 
#'               trend = as.double(seasonal::trend(air_seas)))
#' air_p <- ggplot2::ggplot(air_df, ggplot2::aes(x=date)) +
#'   ggplot2::geom_line(ggplot2::aes(y = ori), color = "grey") + 
#'   ggplot2::geom_line(ggplot2::aes(y = sa), 
#'                      color="steelblue", linetype="twodash") + 
#'   ggplot2::geom_line(ggplot2::aes(y = trend), 
#'                      color="darkred", linetype="dotdash") + 
#'   ggplot2::ggtitle("Airline Passenger X-11 Seasonal Adjustment")
#' air_alt_text <- 
#'    generate_alt_text(air_p, 
#'                      "Time series plot", 
#'                      "International Airline Passengers time series",
#'                      "compare seasonal adjustment and trend to original series")
#' @export
generate_alt_text <- function(gg_object = NULL, chart_type = NULL, data_type = NULL, reason_text = NULL, 
                              short_alt = FALSE, BrailleR_only = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 3.1, 9/9/2024
    
    if (!short_alt) {
        if (is.null(gg_object)) {
            cat("must specify ggplot graph object")
            return(NULL)
        } else {
            if (!ggplot2::is.ggplot(gg_object)) {
                cat("must specify ggplot graph object")
                return(NULL)
            }
        }
    } 
	
	if (!BrailleR_only) {
   
		if (is.null(chart_type)) {
			cat("must specify type of chart")
			return(NULL)
		}
    
		if (is.null(data_type)) {
			cat("must specify description of data used in plot")
			return(NULL)
		}
   
		if (is.null(reason_text)) {
			cat("must specify reason for chart")
			return(NULL)
		}
	}
    
    # create short text based on Amy Casale's article
    short_alt_text <- 
      paste0(chart_type, " of ", data_type, " where ", reason_text)
    if (short_alt) {
        return(short_alt_text)
    }
	
	if (BrailleR_only) {
		long_alt_text <- stringr::str_c(BrailleR::VI(gg_object)$text, collapse=" ")
	} else {
    # append BrailleR text to short text 
		long_alt_text <- 
			paste0(short_alt_text, " ", 
                   stringr::str_c(BrailleR::VI(gg_object)$text, collapse=" "))
	}

    #return long alt text
    return(long_alt_text)
}
