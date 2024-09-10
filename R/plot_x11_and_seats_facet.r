#' Compare X-11 and SEATS seasonal adjustments in a facet plot
#'
#' Generates a \code{ggplot} object with a time series facet plot that compares an X-11 and SEATS 
#' seasonal adjustment, optionally including the original series.
#'
#' Version 6.3, 9/9/2024
#'
#' @param this_x11 Time series of the X-11 seasonal adjustment. 
#'        This is a required entry.
#' @param this_seats Time series of the SEATS seasonal adjustment. 
#'        This is a required entry.
#' @param this_ori Time series of the original series. Optional entry.
#' @param main_title Title for the plot. Default is character string \code{'Comparison of X-11 and SEATS Seasonal Adjustments'}.
#' @param sub_title Subtitle for the plot. Optional entry.
#' @param this_x_label Label for X-axis.  Default is \code{"Time"}
#' @param this_y_label Label for Y-axis.  Default is \code{" "}
#' @param line_color Color used for lines in plot.  Default is \code{"steelblue"}.
#' @param remove_legend Logical scalar; if TRUE, plot legend will be removed.
#'        Default is FALSE.
#' @return A \code{ggplot} object that generates a facet plot comparing an X-11 and SEATS 
#'         seasonal adjustment, trend, or factor.
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
#' ukgas_seats_v_x11_facet_p <- 
#'     plot_x11_and_seats_facet(this_x11 = ukgas_x11_sa, this_seats = ukgas_seats_sa,
#'                        main_title = "UK Gas Seasonal Adjustments",
#'                        line_color = "forestgreen")
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
plot_x11_and_seats_facet <- 
    function(this_x11 = NULL, 
             this_seats = NULL, 
             this_ori = NULL, 
             main_title = "Compare X-11 and SEATS",  
             sub_title = NULL, 
             this_x_label = "Time", 
             this_y_label = " ", 
             line_color = "steelblue",
			 remove_legend = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 6.3, 9/9/2024

    if (is.null(this_x11)) {
        cat("must specify the X-11 seasonally adjusted series")
        return(NULL)
    } else {
        # check if \code{this_x11} is a ts object
	if (!is.ts(this_x11)) {
	    cat("must specify a ts object")
            return(NULL)
	}
    }

    if (is.null(this_seats)) {
        cat("must specify the SEATS seasonally adjusted series")
        return(NULL)
    } else {
        # check if \code{this_seats} is a ts object
	if (!is.ts(this_seats)) {
	    cat("must specify a ts object")
            return(NULL)
	}
    }
    
    if (is.null(this_ori)) {
        this_facet_df <- 
            data.frame(date = tsbox::ts_df(this_x11)$time, 
	               x11.sa = as.double(this_x11), 
	               seats.sa = as.double(this_seats)) %>%
        dplyr::select(.data$date, .data$x11.sa, .data$seats.sa) %>%
        tidyr::gather(key = "series", value = "value", -date)
		
		p_facet <- 
			ggplot2::ggplot(this_facet_df, 
			                ggplot2::aes(y = .data$value, x = .data$date, 
			                             color = .data$series)) +  
				ggplot2::geom_line() + 
			  ggplot2::scale_color_manual(values = c("x11.sa" = line_color[1], 
				                                       "seats.sa" = line_color[2]),
                                            labels = c("X-11", "SEATS")) +
				ggplot2::facet_grid(dplyr::case_when(.data$series == "x11.sa" ~ "X-11",
                                                .data$series == "seats.sa" ~ "SEATS") ~ .) + 
				ggplot2::labs(title = main_title,
							  subtitle = sub_title,
                              x = this_x_label,
                              y = this_y_label)
        
    } else {
        this_facet_df <- 
            data.frame(date = tsbox::ts_df(this_x11)$time, 
	               ori = as.double(this_ori), 
	               x11.sa = as.double(this_x11), 
	               seats.sa = as.double(this_seats)) %>%
	        dplyr::select(.data$date, .data$ori, .data$x11.sa, .data$seats.sa) %>%
	        tidyr::gather(key = "series", value = "value", -date)
	        
	p_facet <- 
	   ggplot2::ggplot(this_facet_df, 
	                   ggplot2::aes(y = .data$value, x = .data$date, 
	                                color = .data$series)) +  
           ggplot2::geom_line() + 
		       ggplot2::scale_color_manual(values = c("ori" = line_color[1],
		                                          "x11.sa" = line_color[2], 
				                                  "seats.sa" = line_color[3]),
                                            labels = c("Ori", "X-11", "SEATS")) +
           ggplot2::facet_grid(dplyr::case_when(.data$series == "ori" ~ "Ori",
                                                .data$series == "x11.sa" ~ "X-11",
                                                .data$series == "seats.sa" ~ "SEATS") ~ .) + 
           ggplot2::labs(title = main_title,
                         subtitle = sub_title,
                         x = this_x_label,
                         y = this_y_label)
    }
    

 	if (remove_legend) {
 	  p_facet <- 
 	    p_facet + ggplot2::theme(legend.position = "none")
	}
	
   return(p_facet)
}