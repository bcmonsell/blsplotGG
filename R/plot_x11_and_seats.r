#' Compare X-11 and SEATS seasonal adjustment
#'
#' Generates a \code{ggplot} object with a time series plot that compares an X-11 and SEATS 
#' seasonal adjustment, optionally including the original series.
#'
#' Version 4.3, 11/6/2024
#'
#' @param this_x11 Time series of the X-11 seasonal adjustment. 
#'        This is a required entry.
#' @param this_seats Time series of the SEATS seasonal adjustment. 
#'        This is a required entry.
#' @param this_ori Time series of the original series. Optional entry.
#' @param main_title Title for the plot. 
#'        Default is character string \code{'Comparison of X-11 and SEATS Seasonal Adjustments'}.
#' @param sub_title Subtitle for the plot. Optional entry.
#' @param this_x_label Label for X-axis.  Default is \code{"Time"}
#' @param this_y_label Label for Y-axis.  Default is \code{" "}
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param line_color Character vector of length 2 (if \code{this_ori} is not specified)
#'        or 3 (if \code{plot_ori} is specified); color used for lines in the plot,
#'        in the order of seasonally adjusted series, trend, original series. 
#'        Default is generated from the \code{RColorBrewer} palette \code{"Dark2"}.
#' @param this_palette Character string; default \code{RColorBrewer} palette.
#'        Deault is \code{"Dark2"}.
#' @param this_guide_legend Title for legend.  Default is \code{"Series"}
#' @return A \code{ggplot} object that generates a plot comparing an X-11 and SEATS 
#'         seasonal adjustment, trend, or factors.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
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
#' ukgas_seats_v_x11_p <- 
#'     plot_x11_and_seats(this_x11 = ukgas_x11_sa, this_seats = ukgas_seats_sa,
#'                        main_title = "UK Gas Seasonal Adjustments",
#'                        sub_title = "X-11 - Blue, SEATS - Green",
#'                        line_color = c("steelblue", "forestgreen"))
#' @importFrom rlang .data
#' @export
#' @importFrom magrittr %>%
plot_x11_and_seats <- 
    function(this_x11 = NULL, 
             this_seats = NULL, 
             this_ori = NULL, 
             main_title = "Compare X-11 and SEATS", 
             sub_title = NULL, 
             this_x_label = "Time", 
             this_y_label = " ", 
             do_grid = FALSE,
             do_background = FALSE,
             line_color = NULL, 
             this_palette = "Dark2",
             this_guide_legend = "Series") {
    # Author: Brian C. Monsell (OEUS) Version 4.3, 11/6/2024

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

    num_color <- 3
    if (is.null(this_ori)) {
		num_color <- 2
    }
	
    if (is.null(line_color)) {
        line_color <- RColorBrewer::brewer.pal(3, this_palette)[1:num_color]
    } else {
        if (length(line_color) < num_color) {
	    if (length(line_color) > 1) {
                warning("Number of line colors specified less than series plotted.")
                warning("Will use colorRampPalette to increase number of colors.")
                line_color <- grDevices::colorRampPalette(line_color)(num_color)
            }
        }
    }

    if (is.null(this_ori)) {
        this_df <- 
	    data.frame(date = tsbox::ts_df(this_x11)$time, 
	               x11.sa = as.double(this_x11), 
		       seats.sa = as.double(this_seats)) %>%
          dplyr::select(.data$date, .data$x11.sa, .data$seats.sa) %>%
          tidyr::gather(key = "sa", value = "value", -date)
        p_x11_seats <- 
            ggplot2::ggplot(this_df) + 
            ggplot2::geom_line(ggplot2::aes(x = .data$date, 
                                            y = .data$value, 
                                            color = .data$sa)) + 
            ggplot2::scale_color_manual(labels = c("X-11", "SEATS"), 
                                        values = line_color[1:2]) +
            ggplot2::labs(title = main_title,
                          subtitle = sub_title,
                          x = this_x_label,
                          y = this_y_label) +
            ggplot2::guides(color = ggplot2::guide_legend(this_guide_legend))
    } else {
        this_df <- 
            data.frame(date = tsbox::ts_df(this_x11)$time, 
	               ori = as.double(this_ori),  
	               x11.sa = as.double(this_x11), 
	               seats.sa = as.double(this_seats)) %>%
	        dplyr::select(.data$date, .data$ori, .data$x11.sa, .data$seats.sa) %>%
			tidyr::gather(key = "sa", value = "value", -date)
        p_x11_seats <- 
            ggplot2::ggplot(this_df) + 
            ggplot2::geom_line(ggplot2::aes(x = .data$date, 
                                            y = .data$value, 
                                            color = .data$sa)) + 
            ggplot2::scale_color_manual(labels = c("Ori", "X-11", "SEATS"), 
                                        values = line_color[c(3,1,2)]) +
            ggplot2::labs(title = main_title,
                          subtitle = sub_title,
                          x = this_x_label,
                          y = this_y_label) +
            ggplot2::guides(color = ggplot2::guide_legend(this_guide_legend))
    }
	
	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_x11_seats <- p_x11_seats + ggplot2::theme_bw()
    }

	# remove grid lines if \code{do_grid = FALSE}
	if (!do_grid) {
	    p_x11_seats <- p_x11_seats + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
	}
       
	return(p_x11_seats)
}
