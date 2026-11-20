#' Compare two seasonal adjustments in a facet plot
#'
#' Generates a \code{ggplot} object with a time series facet plot that compares two 
#' seasonal adjustments of the same series, optionally including the original series.
#'
#' Version 8.1, 2026-09-30
#'
#' @param this_sa_one Time series of the first seasonal adjustment. 
#'        This is a required entry.
#' @param this_sa_two Time series of the second seasonal adjustment. 
#'        This is a required entry.
#' @param this_ori Time series of the original series. Optional entry.
#' @param main_title Title for the plot. 
#'        Default is character string \code{"Comparison of Seasonal Adjustments"}.
#' @param sub_title Subtitle for the plot. Optional entry.
#' @param this_x_label Label for X-axis.  Default is \code{"Time"}
#' @param this_y_label Label for Y-axis.  Default is \code{" "}
#' @param line_color Color used for lines in plot.  Default is \code{"steelblue"}.
#'        For a listing of R color codes, 
#'        see \insertCite{rchartsColors;textual}{blsplotGG}.
#' @param this_sa_text Caracter vector of length 2;  
#'        labels for different seasonal adjustments.  
#'        Note that if \code{this_ori} is set, the label for that  
#'        line will be set as \code{"Ori"}.
#'        Default is \code{c("sadj1", "sadj2")}.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param facet_scale Character scalar; set the scale for the y-axis of the facet plots. 
#'        Default is \code{"fixed"}.  To set axis to be different for the different plots,
#'        set \code{facet_scale = "free_y"}.
#' @param facet_text_size Numeric scalar; size of text in facet labels. 
#'        Default is set by the \code{ggplot2} package.
#' @return A \code{ggplot} object that generates a facet plot comparing  
#'         two seasonal adjustments, trends, or factors.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' ukgas_x11_seas   <- 
#'    seasonal::seas(UKgas, series.period = 4, 
#'       arima.model = "(0 1 1)(0 1 1)", 
#'       transform.function = "log", forecast.maxlead = 20,
#'       x11.seasonalma = "s3x5", 
#'       check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_seats_seas <- 
#'    seasonal::seas(UKgas, series.period = 4,  
#'       arima.model = "(0 1 1)(0 1 1)", 
#'       transform.function = "log", forecast.maxlead = 20,
#'       check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_x11_sa     <- seasonal::final(ukgas_x11_seas)
#' ukgas_seats_sa   <- seasonal::final(ukgas_seats_seas)
#' ukgas_two_sa_facet_p <- 
#'     plot_two_sa_facet(this_sa_one = ukgas_x11_sa, 
#'         this_sa_two = ukgas_seats_sa,
#'         main_title = "UK Gas Seasonal Adjustments",
#'         line_color = "forestgreen",
#'         this_sa_text = c("X-11", "Seats"))
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom Rdpack reprompt
#' @export
plot_two_sa_facet <- 
    function(this_sa_one = NULL, 
	         this_sa_two = NULL, 
			 this_ori = NULL, 
             main_title = "Compare X-11 and SEATS",  
			 sub_title = NULL, 
             this_x_label = "Time", 
			 this_y_label = " ", 
             line_color = "steelblue", 
			 this_sa_text = c("sadj1", "sadj2"),
			 do_grid = FALSE, 
			 do_background = FALSE, 
			 facet_scale = "fixed",
			 facet_text_size = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 8.1, 2026-09-30

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
			ggplot2::ggplot(this_facet_df, 
				ggplot2::aes(y = .data$value, x = .data$date, 
		                             color = .data$series)) +  
			ggplot2::geom_line(color = line_color) + 
			ggplot2::facet_grid(factor(.data$series, 
	                            levels = c("one.sa", "two.sa"), 
								labels = this_sa_text) ~ ., 
								scale = facet_scale) +
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
			ggplot2::ggplot(this_facet_df, 
				ggplot2::aes(y = .data$value, x = .data$date, 
	                     colour = .data$series)) +  
			ggplot2::geom_line(color = line_color) + 
			ggplot2::facet_grid(factor(.data$series, 
	                            levels = c("ori", "one.sa", "two.sa"), 
								labels = c("Ori", this_sa_text)) ~ ., 
								scale = facet_scale) +
			ggplot2::labs(title = main_title,
                         subtitle = sub_title,
                         x = this_x_label,
                         y = this_y_label) 
    }
        
      # remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_two_sa_facet <- p_two_sa_facet + ggplot2::theme_bw()
    }
      
      # remove grid lines if \code{do_grid = FALSE}
    if (!do_grid) {
        p_two_sa_facet <- p_two_sa_facet + 
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                         panel.grid.minor = ggplot2::element_blank())
    }
	
	p_two_sa_facet <- p_two_sa_facet + ggplot2::theme(legend.position = "none")
	
	if (!is.null(facet_text_size)) {
		p_two_sa_facet <- p_two_sa_facet + 
			ggplot2::theme(strip.text = ggplot2::element_text(size = facet_text_size),
				legend.position = "none")
	} else {
		p_two_sa_facet <- p_two_sa_facet + 
			ggplot2::theme(legend.position = "none")
	}

      
    return(p_two_sa_facet)
}