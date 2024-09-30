#' Extract range of data from ggplot object
#'
#' Computes the range of all data plotted in given ggplot object
#'
#' Version 1.2, 10/19/2023
#'
#' @param this_p \code{ggplot} object of a time series plot. 
#'        This is a required entry.
#' @return Vector of length 2 with the range of the data used to generate a given 
#'         \code{ggplot} object of a time series plot.
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
#' air_p <- ggplot2::ggplot(air_df, ggplot2::aes(x=date)) +
#'   ggplot2::geom_line(ggplot2::aes(y = ori), color = "grey") + 
#'   ggplot2::geom_line(ggplot2::aes(y = trend), color="darkred", 
#'                      linetype="twodash") + 
#'   ggplot2::labs(
#'     title = "Airline Passenger X-11 Trend Component",
#'     subtitle = NULL,
#'     x = "Time",
#'     y = "Airline Passengers")
#' air_short_p <- plot_date_span(air_p, "1-1-1956", "1-1-1961")
#' air_short_range <- extract_range_from_ggplot(air_short_p)
#' @export
extract_range_from_ggplot <- function(this_p = NULL) {
    # Author: Brian C. Monsell (OEUS) 

#   check if \code{this_p} is specified and is a list object
    if (is.null(this_p)) {
        cat("must specify a ggplot object")
        return(NULL)
    } else {
        if (!is.list(this_p)) {
            cat("this is not a list object; must specify a ggplot object")
            return(NULL)
        }
    }
    
#   format ggplot argument to make it easier to extract the underlieing data
    data_from_p <- ggplot2::ggplot_build(this_p)
    data_length <- length(data_from_p$data)

#   generate an index for data not plotted.
    this_index  <- !is.na(data_from_p$data[[1]]$x)

#   Generate range for the first data set in ggplot object
    this_range <- range(data_from_p$data[[1]]$y[this_index])

#   if more than one data source in plot, update range of data
    if (data_length > 1) {
        for (i in 2:data_length) {
            this_index  <- !is.na(data_from_p$data[[i]]$x)
            this_range <- range(data_from_p$data[[i]]$y[this_index], this_range)
        }
    }
    
#   return range of the data from this plot
    return(this_range)
}