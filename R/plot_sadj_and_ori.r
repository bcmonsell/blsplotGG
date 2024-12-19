#' Plot X-13 seasonal adjustment, original series on same axis
#'
#' Generates a \code{ggplot} object with a time series plot that compares an X-13 
#' seasonal adjustment and trend, optionally including the original series.
#'
#' Version 2.2, 12/17/2024
#'
#' @param this_sa Time series of a seasonal adjustment. 
#'        This is a required entry.
#' @param this_ori Time series of the original series. Optional entry.
#' @param this_sa_type Character string; type of seasonal adjustment.
#'        Default is \code{"SEATS"}.
#' @param main_title Title for the plot. 
#'        By default, the routine will generate a title based on the 
#'        type of adjustment (X-11 and SEATS) done.
#' @param sub_title Subtitle for the plot. Optional entry.
#' @param this_x_label Label for X-axis.  Default is \code{"Time"}
#' @param this_y_label Label for Y-axis.  Default is \code{" "}
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param line_color Character vector of length 2; color used for lines in the plot,
#'        in the order of original series, seasonally adjusted series. 
#'        Default is \code{c("grey", "darkblue")}.
#' @param this_guide_legend Title for legend.  Default is \code{"Series"}
#' @return A \code{ggplot} object that generates a plot comparing a 
#'         seasonally adjusted series with the original series.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' ukgas_x11_seas   <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", forecast.maxlead = 20,
#'                   x11.seasonalma = "s3x5", 
#'                   check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_x11_sa     <- seasonal::final(ukgas_x11_seas)
#' ukgas_sa_ori_p <- 
#'     plot_sadj_and_ori(this_sa = ukgas_x11_sa, 
#'                       this_ori = UKgas,
#'						 this_sa_type = "X-11",
#'                       main_title = "UK Gas Seasonal Adjustment",
#'                       line_color = c("lightgrey", "forestgreen"))
#' @importFrom rlang .data
#' @export
#' @importFrom magrittr %>%
plot_sadj_and_ori <- 
    function(this_sa = NULL, 
	         this_ori = NULL,
			 this_sa_type = "SEATS",
             main_title = NULL, 
             sub_title = NULL, 
             this_x_label = "Time", 
             this_y_label = " ", 
             do_grid = FALSE,
             do_background = FALSE,
             line_color = c("grey", "darkblue"), 
             this_guide_legend = "Series") {
    # Author: Brian C. Monsell (OEUS) Version 2.2, 12/17/2024

    # check if a value is specified for \code{this_sa}
    if (is.null(this_sa)) {
        cat("must specify the seasonally adjusted series")
        return(NULL)
    } else {
        # check if \code{this_sa} is a ts object
		if (!is.ts(this_sa)) {
			cat("must specify a ts object")
				return(NULL)
		}
    }
	
    if (is.null(this_ori)) {
        cat("must specify the original series")
        return(NULL)
    } else {
        # check if \code{this_ori} is a ts object
		if (!is.ts(this_ori)) {
			cat("must specify a ts object")
            return(NULL)
		}
    }


	if (is.null(main_title)) {
		main_title <- 
			paste0(this_sa_type, " Seasonal Adjutment and Oringal Series")
	}
    
    this_df <- 
		data.frame(date = tsbox::ts_df(this_sa)$time, 
				sadj = as.double(this_sa), 
				ori = as.double(this_ori)) %>%
				dplyr::select(.data$date, .data$sadj, .data$ori) %>%
				tidyr::gather(key = "comp", value = "value", -date)
				
    p_sadj_ori <- 
            ggplot2::ggplot(this_df) + 
            ggplot2::geom_line(ggplot2::aes(x = .data$date, 
                                            y = .data$value, 
                                            color = .data$comp)) + 
            ggplot2::scale_color_manual(labels = c("Ori", "SA"), 
                                        values = line_color) +
            ggplot2::labs(title = main_title,
                          subtitle = sub_title,
                          x = this_x_label,
                          y = this_y_label)
    
    p_sadj_ori <- 
      p_sadj_ori + 
        ggplot2::guides(color = ggplot2::guide_legend(this_guide_legend))
	
	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_sadj_ori <- p_sadj_ori + ggplot2::theme_bw()
    }

	# remove grid lines if \code{do_grid = FALSE}
	if (!do_grid) {
	    p_sadj_ori <- p_sadj_ori + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
	}
       
	return(p_sadj_ori)
}
