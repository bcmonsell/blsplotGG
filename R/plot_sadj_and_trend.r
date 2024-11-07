#' Plot X-13 seasonal adjustment, trend on same axis
#'
#' Generates a \code{ggplot} object with a time series plot that compares an X-13 
#' seasonal adjustment and trend, optionally including the original series.
#'
#' Version 1.7, 11/6/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series
#'        This is a required entry.
#' @param plot_ori Logical scalar, indicates if original series is included with plot. 
#'        Default is \code{TRUE}.
#' @param main_title Title for the plot. 
#'        By default, the routine will generate a trend based on the 
#'        type of adjustment (X-11 and SEATS) done.
#' @param sub_title Subtitle for the plot. Optional entry.
#' @param this_x_label Label for X-axis.  Default is \code{"Time"}
#' @param this_y_label Label for Y-axis.  Default is \code{" "}
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param line_color Character vector of length 2 (if \code{plot_ori} is \code{FALSE})
#'        or 3 (if \code{plot_ori} is \code{TRUE}); color used for lines in the plot,
#'        in the order of seasonally adjusted series, trend, original series. 
#'        Default is generated from the \code{RColorBrewer} palette \code{"Dark2"}.
#' @param this_palette Character string; default \code{RColorBrewer} palette.
#'        Deault is \code{"Dark2"}.
#' @param this_guide_legend Title for legend.  Default is \code{"Series"}
#' @return A \code{ggplot} object that generates a plot comparing a 
#'         seasonally adjusted series with the trend generated from the same
#'         X-13ARIMA-SEATS seasonal adjustment.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' ukgas_x11_seas   <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", forecast.maxlead = 20,
#'                   x11.seasonalma = "s3x5", x11.save = "d12",
#'                   check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_x11_sadj_and_trend_p <- 
#'     plot_sadj_and_trend(ukgas_x11_seas, plot_ori = TRUE,
#'                         main_title = "UK Gas",
#'                         sub_title = "X-11 Seasonal Adjustment",
#'                         line_color = c("steelblue", "forestgreen", "lightgrey"))
#' ukgas_seats_seas <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", forecast.maxlead = 20,
#'                   seats.save = "s12",
#'                   check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_seats_sadj_and_trend_p <- 
#'     plot_sadj_and_trend(ukgas_seats_seas, plot_ori = FALSE, 
#'                         main_title = "UK Gas Series",
#'                         sub_title = "SEATS Seasonal Adjustment",
#'                         line_color = c("steelblue", "forestgreen"))
#' @importFrom rlang .data
#' @export
#' @importFrom magrittr %>%
plot_sadj_and_trend <- 
    function(seas_obj = NULL, 
             plot_ori = TRUE, 
             main_title = NULL, 
             sub_title = NULL, 
             this_x_label = "Time", 
             this_y_label = " ", 
             do_grid = FALSE,
             do_background = FALSE,
             line_color = NULL, 
             this_palette = "Dark2",
             this_guide_legend = "Series") {
    # Author: Brian C. Monsell (OEUS) Version 1.7, 11/6/2024

    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        cat("must specify a seas object")
		return(NULL)
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            cat("First argument must be a seas object")
			return(NULL)
        }
    }

	this_sadj  <- seasonal::final(seas_obj)
	this_trend <- seasonal::trend(seas_obj)
	if (plot_ori) {
		this_ori <- seasonal::original(seas_obj)
	}
	
	if ("seatsadj" %in% names(seasonal::udg(seas_obj))) {
		this_sa_type <- "SEATS"
	} else {
		this_sa_type <- "X-11"
	}
	
	if (is.null(main_title)) {
		main_title <- 
			paste0(this_sa_type, " Seasonal Adjutment and Trend")
	}
    
    num_color <- 3
    if (!plot_ori) {
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

    if (!plot_ori) {
        this_df <- 
			data.frame(date = tsbox::ts_df(this_sadj)$time, 
				sadj = as.double(this_sadj), 
				trend = as.double(this_trend)) %>%
				dplyr::select(.data$date, .data$sadj, .data$trend) %>%
				tidyr::gather(key = "comp", value = "value", -date)
        p_sadj_trend <- 
            ggplot2::ggplot(this_df) + 
            ggplot2::geom_line(ggplot2::aes(x = .data$date, 
                                            y = .data$value, 
                                            color = .data$comp)) + 
            ggplot2::scale_color_manual(labels = c("SA", "Trend"), 
                                        values = line_color[1:2]) +
            ggplot2::labs(title = main_title,
                          subtitle = sub_title,
                          x = this_x_label,
                          y = this_y_label)
    } else {
        this_df <- 
            data.frame(date = tsbox::ts_df(this_sadj)$time, 
	            ori = as.double(this_ori),  
				sadj = as.double(this_sadj), 
				trend = as.double(this_trend)) %>%
	        dplyr::select(.data$date, .data$ori, .data$sadj, .data$trend) %>%
			tidyr::gather(key = "comp", value = "value", -date)
        
        p_sadj_trend <- 
            ggplot2::ggplot(this_df) + 
            ggplot2::geom_line(ggplot2::aes(x = .data$date, 
                                            y = .data$value, 
                                            color = .data$comp)) + 
            ggplot2::scale_color_manual(labels = c("Ori", "SA", "Trend"), 
                                        values = line_color[c(3,1,2)]) +
            ggplot2::labs(title = main_title,
                          subtitle = sub_title,
                          x = this_x_label,
                          y = this_y_label)
    }
    
    p_sadj_trend <- 
      p_sadj_trend + 
        ggplot2::guides(color = ggplot2::guide_legend(this_guide_legend))
	
	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_sadj_trend <- p_sadj_trend + ggplot2::theme_bw()
    }

	# remove grid lines if \code{do_grid = FALSE}
	if (!do_grid) {
	    p_sadj_trend <- p_sadj_trend + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
	}
       
	return(p_sadj_trend)
}
