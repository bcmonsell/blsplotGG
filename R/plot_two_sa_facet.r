#' Compare two seasonal adjustments in a facet plot
#'
#' Generates a \code{ggplot} object with a time series facet plot that compares two s
#' easonal adjustments of the same series, optionally including the original series.
#'
#' Version 2.1, 5/6/2024
#'
#' @param this_sa_one Time series of the first seasonal adjustment. 
#'                    This is a required entry.
#' @param this_sa_two Time series of the second seasonal adjustment. 
#'                    This is a required entry.
#' @param this_ori Time series of the original series. Optional entry.
#' @param main_title Title for the plot. Default is character string \code{'Comparison of Seasonal Adjustments'}.
#' @param sub_title Subtitle for the plot. Optional entry.
#' @param this_x_label Label for X-axis.  Default is \code{"Time"}
#' @param this_y_label Label for Y-axis.  Default is \code{" "}
#' @param this_sa_text Labels for different seasonal adjustments.  Default is \code{c('X-11', 'SEATS')}
#' @param line_color Color used for lines in plot.  Default is \code{"steelblue"}.
#' @return A \code{ggplot} object that generates a facet plot comparing two seasonal adjustments,
#'         trends, or factors.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' ukgas_x11_seas   <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", forecast.maxlead = 20,
#'                   x11.seasonalma = "s3x5", 
#'                   check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_seats_seas <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", forecast.maxlead = 20,
#'                   check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_x11_sa     <- seasonal::final(ukgas_x11_seas)
#' ukgas_seats_sa   <- seasonal::final(ukgas_seats_seas)
#' ukgas_two_sa_facet_p <- 
#'     plot_two_sa_facet(this_sa_one = ukgas_x11_sa, this_sa_two = ukgas_seats_sa,
#'                       main_title = "UK Gas Seasonal Adjustments",
#'                       line_color = "forestgreen")
#' @importFrom magrittr %>%
#' @importFrom rlang .data
##' @export
plot_two_sa_facet <- 
    function(this_sa_one = NULL, this_sa_two = NULL, this_ori = NULL, 
             main_title = "Compare X-11 and SEATS",  sub_title = NULL, 
             this_x_label = "Time", this_y_label = " ", this_sa_text = c("X-11", "SEATS"),
             line_color = "steelblue") {
    # Author: Brian C. Monsell (OEUS) Version 2.1, 5/6/2024

    if (is.null(this_sa_one)) {
        cat("must specify the first seasonally adjusted series")
        return(NULL)
    } else {
        # check if \code{this_sa_one} is a ts object
	if (!is.ts(this_sa_one)) {
	    cat("must specify a ts object")
            return(NULL)
	}
    }
    if (is.null(this_sa_two)) {
        cat("must specify the second seasonally adjusted series")
        return(NULL)
    } else {
        # check if \code{this_sa_two} is a ts object
	if (!is.ts(this_sa_two)) {
	    cat("must specify a ts object")
            return(NULL)
	}
    }
    
    if (is.null(this_ori)) {
        this_facet_df <- 
            data.frame(date = tsbox::ts_df(this_sa_one)$time, 
	               one.sa = as.double(this_sa_one), 
	               two.sa = as.double(this_sa_two)) %>%
		dplyr::select(.data$date, .data$one.sa, .data$two.sa) %>%
		tidyr::gather(key = "series", value = "value", -date)
		
	p_two_sa_facet <- 
	   ggplot2::ggplot(this_facet_df) +  
           ggplot2::geom_line(ggplot2::aes(y = .data$value, x = .data$date), 
                              color = line_color) + 
           ggplot2::facet_grid(dplyr::case_when(.data$series == "one.sa" ~ this_sa_text[1],
                                                .data$series == "two.sa" ~ this_sa_text[2]) ~ .) + 
           ggplot2::labs(title = main_title,
                         subtitle = sub_title,
                         x = this_x_label,
                         y = this_y_label)
        
    } else {
        this_facet_df <- 
            data.frame(date = tsbox::ts_df(this_sa_one)$time, 
	               ori = as.double(this_ori), 
	               one.sa = as.double(this_sa_one), 
	               two.sa = as.double(this_sa_two)) %>%
	        dplyr::select(.data$date, .data$ori, .data$one.sa, .data$two.sa) %>%
	        tidyr::gather(key = "series", value = "value", -date)
	        
	p_two_sa_facet <- 
	   ggplot2::ggplot(this_facet_df) +  
           ggplot2::geom_line(ggplot2::aes(y = .data$value, x = .data$date), 
                              color = line_color) + 
           ggplot2::facet_grid(dplyr::case_when(.data$series == "ori" ~ "Ori",
                                                .data$series == "one.sa" ~ this_sa_text[1],
                                                .data$series == "two.sa" ~ this_sa_text[1]) ~ .) + 
           ggplot2::labs(title = main_title,
                         subtitle = sub_title,
                         x = this_x_label,
                         y = this_y_label)
    }
    

    return(p_two_sa_facet)
}