#' generate x-axis label for outliers
#'
#' Generate an x-axis label when adding lines for outliers to a \code{ggplot} plot object
#'
#' Version 1.1, 5/15/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series. 
#'        This is a required entry.
#' @param this_color Character array of length 6; color used for different outliers, with the order being 
#'        \code{"ao", "ls", "tc", "so", "rp", "tls"}. 
#'        Default is \code{c("red", "blue", "green", "brown", "grey", "yellow")}. 
#' @return Character string with description of outliers for x-axis label of a \code{ggplot}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <- 
#'    seasonal::seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)", x11 = "")
#' air_df   <- 
#'    data.frame(date = tsbox::ts_df(AirPassengers)$time, 
#'               ori = as.double(seasonal::original(air_seas)),  
#'               sa = as.double(seasonal::final(air_seas)), 
#'               trend = as.double(seasonal::trend(air_seas)))
#' this_p <- ggplot2::ggplot(air_df) +
#'   ggplot2::geom_line(ggplot2::aes(x=date, y = ori), color = "grey") + 
#'   ggplot2::geom_line(ggplot2::aes(x=date, y = sa), color="steelblue", linetype="twodash") + 
#'   ggplot2::geom_line(ggplot2::aes(x=date, y = trend), color="darkred", linetype="twodash") + 
#'   ggplot2::labs(
#'     title = "Airline Passenger X-11 Seasonal Adjustment",
#'     subtitle = NULL,
#'     y = "Airline Passengers")
#' this_p_with_outlier_lines <- add_outlier_lines(this_p, air_seas)
#' outlier_lines_label <- gen_outlier_label(air_seas)
#' this_p_with_outlier_lines <- 
#'     this_p_with_outlier_lines + ggplot2::xlab(outlier_lines_label)
#' @importFrom rlang .data
#' @import stats
#' @export
gen_outlier_label <- function(seas_obj = NULL, 
                              this_color = c("red", "blue", "green", "brown", "grey", "yellow")) {
  # Author: Brian C. Monsell (OEUS) Version 1.1, 5/15/2024
  
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
        }
    }
    
    # number of AO, LS, TC, SO, RP, and TLS outliers.
    nAO <- seasonal::udg(seas_obj, "outlier.ao")
    nLS <- seasonal::udg(seas_obj, "outlier.ls")
    nTC <- seasonal::udg(seas_obj, "outlier.tc")
    nSO <- seasonal::udg(seas_obj, "outlier.so")
    nRP <- seasonal::udg(seas_obj, "outlier.rp")
    nTLS <- seasonal::udg(seas_obj, "outlier.tls")
		
    # create sub header for outliers included in plot
    sub_header <- ""
    if (nAO > 0) {
        sub_header <- paste(sub_header, " AO = ", this_color[1], sep = "")
        if (nLS + nTC + nSO + nRP + nTLS > 0) {
            sub_header <- paste(sub_header, ",", sep = "")
        }
    }
    if (seasonal::udg(seas_obj, "outlier.ls") > 0) {
        sub_header <- paste(sub_header, " LS = ", this_color[2], sep = "")
        if (nTC + nSO + nRP + nTLS > 0) {
            sub_header <- paste(sub_header, ",", sep = "")
        }
    }
    if (seasonal::udg(seas_obj, "outlier.tc") > 0) {
        sub_header <- paste(sub_header, " TC = ", this_color[3], sep = "")
        if (nSO + nRP + nTLS > 0) {
            sub_header <- paste(sub_header, ",", sep = "")
        }
    }
    if (seasonal::udg(seas_obj, "outlier.so") > 0) {
        sub_header <- paste(sub_header, " SO = ", this_color[4], sep = "")
        if (nRP + nTLS > 0) {
            sub_header <- paste(sub_header, ",", sep = "")
        }
    }
    if (seasonal::udg(seas_obj, "outlier.rp") > 0) {
        sub_header <- paste(sub_header, " RP = ", this_color[5], sep = "")
        if (nTLS > 0) {
            sub_header <- paste(sub_header, ",", sep = "")
        }
    }
    if (seasonal::udg(seas_obj, "outlier.tls") > 0) {
        sub_header <- paste(sub_header, " TLS = ", this_color[6], sep = "")
    }
        
    return(sub_header)
	
}
