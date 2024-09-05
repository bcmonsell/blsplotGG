#' Add shading for NBER recession dates
#'
#' Add shading for US NBER recession dates ro ggplot plot object.
#' 
#' Version 3.2, 5/6/2024
#' 
#' @param this_p \code{ggplot} object of a time series plot. 
#'        This is a required entry.
#' @param shade_color Character scalar; shading for recession region. 
#'        Default is \code{"pink"}.
#' @param shade_alpha numeric scalar; controls the intensity of the shading. Default is \code{0.2}.
#' @return \code{ggplot} object with shading for recession added.
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
#' this_p <- ggplot2::ggplot(air_df) +
#'   ggplot2::geom_line(ggplot2::aes(x=date, y = ori), color = "grey") + 
#'   ggplot2::geom_line(ggplot2::aes(x=date, y = sa), color="steelblue", linetype="twodash") + 
#'   ggplot2::geom_line(ggplot2::aes(x=date, y = trend), color="darkred", linetype="twodash") + 
#'   ggplot2::labs(
#'     title = "Airline Passenger X-11 Seasonal Adjustment",
#'     subtitle = NULL,
#'     x = "Time",
#'     y = "Airline Passengers")
#' this_p_with_recession_shading <-
#'     add_recession_shade(this_p, shade_color = "steelblue")
#' @importFrom rlang .data
#' @import stats
#' @export
add_recession_shade <- function(this_p = NULL, shade_color = "lightblue1", 
                                shade_alpha = 0.2) {
# Author: Brian C. Monsell (OEUS) Version 3.2, 5/6/2024
  
#   check if \code{this_p} is specified and is a list object
    if (is.null(this_p)) {
        cat("must specify a ggplot object")
        return(NULL)
    } else {
        if (!ggplot2::is.ggplot(this_p)) {
            cat("this is not a ggplot object")
            return(NULL)
        }
    }
    
#   get recessions from NBER, put in date value format
    recessions_vec = tis::nberDates()
    recessions_df  = 
        data.frame(Start = tis::as.Date.ti(recessions_vec[,1]), 
                   End = tis::as.Date.ti(recessions_vec[,2]))
        
#   get date range of plot converted to date format
    this_plot_date_range <-
        as.Date(range(ggplot2::ggplot_build(this_p)$data[[1]]$x, na.rm = TRUE), 
                      origin = as.Date("1970-01-01"))
        
#   trim recessions to get just those within the range of data being plotted
    recessions_trim = dplyr::filter(recessions_df,   .data$End >= this_plot_date_range[1])
    recessions_trim = dplyr::filter(recessions_trim, .data$Start <= this_plot_date_range[2])
    
#   return ggplot object augmented with recession shading
    return(this_p + 
           ggplot2::geom_rect(data=recessions_trim, 
                              ggplot2::aes(xmin=.data$Start, 
                                           xmax=.data$End, 
                                           ymin=-Inf, 
                                           ymax=+Inf), 
                              fill = shade_color, 
                              alpha = shade_alpha))    
}