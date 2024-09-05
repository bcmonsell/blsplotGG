#' Plot a span of data
#'
#' Shortens the time span of an existing time series \code{ggplot} object by limiting the 
#' X axis to user specified dates.
#' 
#' Version 2.2, 8/29/2024
#'
#' @param this_p A \code{ggplot} object of a time series plot. 
#'               This is a required entry.
#' @param this_start_span Character scalar with the date of the start of the span to be plotted. 
#'                        This is a required entry.
#' @param this_end_span Character scalar with the date of the end of the span to be plotted. 
#'                      This is a required entry.
#' @param this_date_breaks Character scalar with the interval for tic marks on the x-axis. 
#'                         Default is \code{"1 year"}.
#' @param this_date_format Character scalar with the format used for the date label on the x-axis. 
#' @param reset_y_limit Logical scalar that if TRUE, the range of the y-axis is reset. Default is \code{FALSE}.
#' @return A \code{ggplot} object that produces a subplot of the submitted plot.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
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
#'   ggplot2::geom_line(ggplot2::aes(y = trend), 
#'                      color="darkred", 
#'                      linetype="twodash") + 
#'   ggplot2::labs(
#'     title = "Airline Passenger X-11 Trend Component",
#'     subtitle = NULL,
#'     x = "Time",
#'     y = "Airline Passengers")
#' air_short_p <- 
#'    plot_date_span(air_p, "1-1-1956", "1-1-1962", reset_y_limit = TRUE)
#' @export
plot_date_span <- 
  function(this_p = NULL, 
           this_start_span = NULL, 
           this_end_span = NULL, 
           this_date_breaks = "1 year",
           this_date_format = "%Y", 
           reset_y_limit = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 2.2, 8/29/2024
 
#  check if \code{this_p} is specified and is a list object
    if (is.null(this_p)) {
        cat("must specify ggplot graph object")
        return(NULL)
    } else {
        if (!ggplot2::is.ggplot(this_p)) {
            cat("must specify ggplot graph object")
            return(NULL)
        }
    }

#  check if \code{this_start_span} and \code{this_end_span} is specified 
#  and is a character object
    if (is.null(this_start_span)) {
        cat("must specify the start to the plotting span")
        return(NULL)
    } else {
        if (!is.character(this_start_span)) {
            cat("Date for start of span must be a character object")
            return(NULL)
        }
    }

    if (is.null(this_end_span)) {
        cat("must specify the start to the plotting span")
        return(NULL)
    } else {
        if (!is.character(this_end_span)) {
            cat("Date for end of span must be a character object")
            return(NULL)
        }
    }
 
#   apply user-defined limits to x-axis
    this_short_p <- 
        this_p + ggplot2::scale_x_date(date_breaks = this_date_breaks, 
               limits = c(lubridate::mdy(this_start_span), 
                          lubridate::mdy(this_end_span)),
               date_labels = this_date_format)
               
#   reset the y-limit of the plot to fit the reduced span better
    if (reset_y_limit) {
        short_range  <- extract_range_from_ggplot(this_short_p)
        this_short_p <- this_short_p + ggplot2::ylim(short_range)
    } 
    
    return(this_short_p)
}