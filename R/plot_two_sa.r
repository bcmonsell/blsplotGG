#' Compare two seasonal adjustments
#'
#' Generates a \code{ggplot} object with a time series plot that compares two seasonal adjustments
#' of the same series, optionally including the original series.
#'
#' Version 3.7, 2/17/2026
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
#' @param this_sa_text Caracter vector of length 2; labels for different seasonal adjustments.  
#'        Note that if \code{this_ori} is set, the label for that line will be set as \code{"Ori"}.
#'        Default is \code{c('X-11', 'SEATS')}. 
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param line_color Character scalar; color used for plot. 
#'        User should specify colors in the order of original series, first SA, and second SA
#'        if \code{this_ori} is set, first SA and second SA otherwise.
#'        Default is the \code{RColorBrewer} palette \code{"Dark2"}.
#'        For a listing of R color codes, 
#'        see \insertCite{rchartsColors;textual}{blsplotGG}.
#' @param this_palette Character string; default \code{RColorBrewer} palette.
#'        Default is the \code{RColorBrewer} palette \code{"Dark2"} \insertCite{RColorBrewer}{blsplotGG}. 
#' @param this_legend_title Title for legend.  Default is \code{"Series"}
#' @param this_legend_title_size integer scalar; Size of the legend title.
#'        Default is \code{12}.
#' @param this_legend_text_size integer scalar; Size of the legend title.
#'        Default is \code{10}.
#' @param this_legend_position character String; The position of the legend, which can be one of
#'        \code{top}, \code{bottom}, \code{left}, \code{right}.
#'        Default is \code{right}.
#' @param this_legend_space integer scalar; Space between legend and plot in points.
#'        Default is \code{10}.
#' @return A \code{ggplot} object that generates a plot comparing two seasonal adjustments, trend, or factors.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
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
#' @importFrom Rdpack reprompt
#' @importFrom magrittr %>%
#' @export
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
             this_legend_title = "Series",
             this_legend_title_size = 12,
             this_legend_text_size = 10,
			 this_legend_position = "right",
			 this_legend_space = 10) {
    # Author: Brian C. Monsell (OEUS) Version 3.7, 2/17/2026

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
	
	# check legend options 
	position_all <- c("top", "bottom", "left", "right")
	if(sum(match(position_all, this_legend_position), na.rm = TRUE) == 0) {
        cat(paste("Legend position choice not valid:", this_legend_position))
		return(NULL)
	}
	
	if(this_legend_text_size <= 0) {
        cat(paste("Legend text size not valid (must be greater than zero):", 
		           this_legend_text_size))
		return(NULL)
	}
	
	if(this_legend_title_size <= 0) {
        cat(paste("Legend title text size not valid (must be greater than zero):", 
		           this_legend_title_size))
		return(NULL)
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
            ggplot2::guides(color = ggplot2::guide_legend(this_legend_title))
    } else {
        this_df <- 
            data.frame(date = tsbox::ts_df(this_sa_one)$time, 
	               one.ori = as.double(this_ori),  
	               one.sa = as.double(this_sa_one), 
	               two.sa = as.double(this_sa_two)) %>%
	    dplyr::select(.data$date, .data$one.ori, .data$one.sa, .data$two.sa) %>%
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
            ggplot2::guides(color = ggplot2::guide_legend(this_legend_title))
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
       
	p_two_sa <- p_two_sa +
		ggplot2::theme(legend.text = ggplot2::element_text(size = this_legend_text_size),
					   legend.title = ggplot2::element_text(size = this_legend_title_size),
				       legend.position = this_legend_position,
				       legend.box.spacing = ggplot2::unit(this_legend_space, "pt"))
	
   return(p_two_sa)
}