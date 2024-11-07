#' Compare two seasonal adjustments
#'
#' Generates a \code{ggplot} object with a time series plot that compares two seasonal adjustments
#' of the same series, optionally including the original series.
#'
#' Version 2.4, 11/6/2024
#'
#' @param this_sa_one Time series of the X-11 seasonal adjustment.
#'        This is a required entry.
#' @param this_sa_two Time series of the SEATS seasonal adjustment.
#'        This is a required entry.
#' @param this_ori Time series of the original series. Optional entry.
#' @param main_title Title for the plot. 
#'        Default is character string \code{'Comparison of Seasonal Adjustments'}.
#' @param sub_title Subtitle for the plot. Optional entry.
#' @param this_x_label Label for X-axis.  Default is \code{"Time"}
#' @param this_y_label Label for Y-axis.  Default is \code{" "}
#' @param this_sa_text Labels for different seasonal adjustments.  
#'        Default is \code{c('X-11', 'SEATS')}
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param line_color Character scalar; color used for plot. 
#'        User should specify one color for each column of the matrix specified.
#'        Default is the \code{RColorBrewer} palette \code{"Dark2"}.
#' @param this_palette Character string; default \code{RColorBrewer} palette.
#'        Deault is \code{"Dark2"}.
#' @param this_guide_legend Title for legend.  Default is \code{"Series"}
#' @return A \code{ggplot} object that generates a plot comparing two seasonal adjustments, trend, or factors.
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
#'     plot_two_sa(this_sa_one = ukgas_x11_sa, this_sa_two = ukgas_seats_sa,
#'                        main_title = "UK Gas Seasonal Adjustments",
#'                        sub_title = "X-11 - Blue, SEATS - Green",
#'                        line_color = c("steelblue", "forestgreen"))
#' @importFrom rlang .data
#' @export
#' @importFrom magrittr %>%
plot_two_sa <- 
    function(this_sa_one = NULL, 
	         this_sa_two = NULL, this_ori = NULL, 
             main_title = "Compare X-11 and SEATS", sub_title = NULL, 
             this_x_label = "Time", 
			 this_y_label = " ", 
             this_sa_text = c("X-11", "SEATS"), 
			 do_grid = FALSE,
             do_background = FALSE,
             line_color = NULL, 
			 this_palette = "Dark2",
             this_guide_legend = "Series") {
    # Author: Brian C. Monsell (OEUS) Version 2.4, 11/6/2024

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
	
    num_color <- 3
    if (is.null(this_ori)) {
		num_color <- 2
    }
	
    if (is.null(line_color)) {
		line_color <- RColorBrewer::brewer.pal(3, this_palette)[1:num_color]
    } else {
	if (length(line_color) < num_color) {
	    if (length(line_color) > 1) {
			warning("Number of line colors specified less than number of years.")
			warning("Will use colorRampPalette to increase number of colors.")
			line_color <- grDevices::colorRampPalette(line_color)(num_color)
	    }
	}
    }

    if (is.null(this_ori)) {
        this_df <- 
			data.frame(
				date = tsbox::ts_df(this_sa_one)$time, 
				one.sa = as.double(this_sa_one), 
				two.sa = as.double(this_sa_two)) %>%
            dplyr::select(.data$date, .data$one.sa, .data$two.sa) %>%
            tidyr::gather(key = "sa", value = "value", -date)
        p_two_sa <- 
            ggplot2::ggplot(this_df) + 
            ggplot2::geom_line(ggplot2::aes(x = .data$date, 
                                            y = .data$value, 
                                            color = .data$sa)) + 
            ggplot2::scale_color_manual(labels = this_sa_text, 
                                        values = line_color[1:2]) +
            ggplot2::labs(title = main_title,
                          subtitle = sub_title,
                          x = this_x_label,
                          y = this_y_label) +
            ggplot2::guides(color = ggplot2::guide_legend(this_guide_legend))
    } else {
        this_df <- 
            data.frame(date = tsbox::ts_df(this_sa_one)$time, 
	               ori = as.double(this_ori),  
	               one.sa = as.double(this_sa_one), 
	               two.sa = as.double(this_sa_two)) %>%
	    dplyr::select(.data$date, .data$ori, .data$one.sa, .data$two.sa) %>%
	    tidyr::gather(key = "sa", value = "value", -date)
        p_two_sa <- 
            ggplot2::ggplot(this_df) + 
            ggplot2::geom_line(ggplot2::aes(x = .data$date, 
                                            y = .data$value, 
                                            color = .data$sa)) + 
            ggplot2::scale_color_manual(labels = c("Ori", this_sa_text), 
                                        values = line_color) +
            ggplot2::labs(title = main_title,
                          subtitle = sub_title,
                          x = this_x_label,
                          y = this_y_label) +
            ggplot2::guides(color = ggplot2::guide_legend(this_guide_legend))
    }
	
    # remove grid lines if \code{do_grid = FALSE}
    if (!do_grid) {
		p_two_sa <- p_two_sa + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			           panel.grid.minor = ggplot2::element_blank())
    }
       
	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_two_sa <- p_two_sa + ggplot2::theme_bw()
    }

     # remove grid lines if \code{do_grid = FALSE}
    if (!do_grid) {
		p_two_sa <- p_two_sa + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			           panel.grid.minor = ggplot2::element_blank())
    }
       
   return(p_two_sa)
}