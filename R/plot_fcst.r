#' Forecast plot
#'
#' Generates a \code{ggplot} plot of regARIMA forecasts with confidence bounds.
#'
#' Version 4.3, 1/7/2026
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series
#'        This is a required entry.
#' @param main_title Character string; main title of plot.  
#'        Default is \code{'ARIMA forecasts'}.
#' @param sub_title Subtitle for the plot. 
#'        Default is to generate the subtitle with the value of the average absolute percentage error 
#'        in within-sample forecasts (AAPE) for the last three years. 
#' @param this_x_label Label for X-axis.  Default is \code{"Time"}
#' @param this_y_label Label for Y-axis.  Default is \code{" "}
#' @param length_ori Integer scalar; number of years of the original series 
#'        to show with forecasts. 
#'        Default is 2 years. 
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param this_palette Array of character strings; color used for original series, 
#'        forecast, and upper and lower forecast bounds. 
#'        Default is \code{c("steelblue", "blue", "darkgrey", "darkgrey")}. 
#' @param this_legend_title Title for legend.  Default is \code{"Forecast"}
#' @param this_legend_title_size integer scalar; Size of the legend title.
#'        Default is \code{12}.
#' @param this_legend_text_size integer scalar; Size of the legend title.
#'        Default is \code{10}.
#' @param this_legend_position character String; The position of the legend, which can be one of
#'        \code{top}, \code{bottom}, \code{left}, \code{right}.
#'        Default is \code{right}.
#' @param this_legend_space integer scalar; Space between legend and plot in points.
#'        Default is \code{10}.
#' @return A \code{ggplot} object of the regARIMA forecasts with confidence bounds.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <- seasonal::seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)",
#'                            forecast.maxlead = 60, forecast.save = "fct",
#'                            series.save = "a1")
#' air_fcst_p <- 
#'     plot_fcst(air_seas, 
#'               main_title = "Forecasts for Airline Passengers", do_grid = TRUE)
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_fcst <- 
    function(seas_obj = NULL, 
	         main_title = "ARIMA forecasts", 
			 sub_title = NULL, 
             this_x_label = "Time", 
			 this_y_label = " ", 
			 length_ori = 2, 
			 do_grid = FALSE, 
             do_background = FALSE, 
             this_palette = c("steelblue", "blue", "darkgrey", "darkgrey"), 
			 this_legend_title = "Forecast",
			 this_legend_title_size = 12,
			 this_legend_text_size = 10,
			 this_legend_position = "right",
			 this_legend_space = 10) {
    # Author: Brian C. Monsell (OEUS) Version 4.3, 1/7/2026
    
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
	
    if (is.null(main_title)) {
       sub_title <- NULL
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

    # extract forecasts, original series
    fcst <- seasonal::series(seas_obj, "fct")
    a1 <- seasonal::series(seas_obj, "a1")
    
    # get date for end of series
    end_a1 <- end(a1)
    
    # get series to be pottted without forecasts
    srs <- window(a1, start = c(end_a1[1] - length_ori, end_a1[2]))
    # get series to be pottted with forecasts
    ext <- ts(c(srs, fcst[, 1]), start = start(srs), frequency = frequency(srs))
	
	length_fcst <- length(fcst[, 1])
	length_srs  <- length(srs)
	
    # generate data frame for plot
	fcst_df <- 
	    data.frame(date  = tsbox::ts_df(ext)$time,
		           ori   = c(srs, rep(NA, length_fcst)),
				   fcst  = c(rep(NA, length_srs), fcst[, 1]),
				   lowerci  = c(rep(NA, length_srs), fcst[, 2]),
				   upperci = c(rep(NA, length_srs), fcst[, 3])) %>%
	    dplyr::select(.data$date, .data$ori, .data$fcst, .data$lowerci, .data$upperci) %>%
        tidyr::gather(key = "fcst", value = "value", -date)
    
    # generate subtitle
    if (is.null(sub_title)) {
        aape <- tryCatch(seasonal::udg(seas_obj, "aape.0"), error = function(e) {
            NULL
        })
		sub_title <- NULL
        if (!is.null(aape)) {
            sub_title <- paste("AAPE Last 3 Years = ", format(aape, digits = 4, nsmall = 2), sep = "")
        }
    }
    
    p_fcst <- 
        ggplot2::ggplot(fcst_df) + 
        ggplot2::geom_line(ggplot2::aes(x = .data$date, 
                                        y = .data$value, 
                                        color = .data$fcst)) + 
        ggplot2::scale_color_manual(labels = c("Ori", "Fcst", "lowCI", "upCI"), 
                                    values = c("ori"     = this_palette[1],
                                               "fcst"    = this_palette[2],
                                               "lowerci" = this_palette[3],
                                               "upperci" = this_palette[4]),
                                    breaks=c("ori", "fcst", "lowerci", 
                                             "upperci")) +
        ggplot2::labs(title = main_title,
                      subtitle = sub_title,
                      x = this_x_label,
                      y = this_y_label) +
        ggplot2::guides(color = ggplot2::guide_legend(this_legend_title))

    
	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_fcst <- p_fcst + ggplot2::theme_bw()
    }

	# remove grid lines if \code{do_grid = FALSE}
	if (!do_grid) {
	    p_fcst <- p_fcst + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
	}
    
    p_fcst <- p_fcst +
		ggplot2::theme(legend.text = ggplot2::element_text(size = this_legend_text_size),
					   legend.title = ggplot2::element_text(size = this_legend_title_size),
					   legend.position = this_legend_position,
					   legend.box.spacing = ggplot2::unit(this_legend_space, "pt"))

	return(p_fcst)
}
