#' Plot time series object. 
#'
#' Generate plot of user-specified time series (ts) object. 
#'
#' Version 2.3, 8/26/2024
#'
#' @param this_series Time series object; 
#'        This is a required entry.
#' @param this_series_name Character string; name of time series. 
#'        No default.
#' @param main_title Character string; main title of plot. 
#'        A title will be generated if no title is specified.
#' @param sub_title Character string; subtitle of plot. 
#'        There is no default subtitle.
#' @param this_y_label Character string; y-axis label for plot. 
#'        If not specified, set to \code{this_series_name}, if specified.
#' @param y_limit Numeric vector of length 2; 
#'        Range of values you wish the plot to be plotted over. 
#'        Default is range of the series specified.
#' @param this_x_label Label for X axis. Default is \code{"Time"}.
#' @param start_plot Integer vector of length 2; 
#'        Starting date for plot. Default is starting date for the time series. 
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines.
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param draw_recess Logical scalar; indicates if certain plots will have 
#'        shaded areas for NBER recession dates. 
#'        Default is no recession shading. 
#' @param recess_color Character string; color used for shading of recession region.
#'        Default is \code{'lightgrey'}.
#' @param recess_sub Logical scalar; indicates if x-axis label for recession is 
#'        produced for this plot. 
#'        Default is x-axis label.
#' @param this_line_type Character string; indicates line type of each plot produced. 
#'        Default is "solid".
#' @param line_color Character string; color used for series in the plot.  
#'        Default is \code{'grey'}. 
#' @return Generate \\code{ggplot} plot of user-specified series. 
#'         If series not specified, print out error message and return NULL.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' air_seas <- 
#'   seasonal::seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)", x11="",
#'                  transform.function = "log")
#' air_seas_d11 <- seasonal::final(air_seas)
#' air_d11_p <- plot_series(air_seas_d11, this_series_name = "AirPassengers", 
#'          main_title = 'X-11 Seasonal Adjustment of Airline Passengers',
#'          sub_title = 'Box-Jenkins Airline series',
#'          do_grid = TRUE, draw_recess = TRUE, line_color = "darkblue")
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_series <- 
    function(this_series = NULL, 
             this_series_name = NULL, 
             main_title = NULL, 
             sub_title = NULL, 
             this_y_label = NULL, 
             y_limit = NULL, 
             this_x_label = "Time", 
             start_plot = NULL, 
             do_grid = FALSE,
			 do_background = FALSE,
             draw_recess = FALSE, 
             recess_color = 'lightgrey', 
             recess_sub = TRUE,  
             this_line_type = "solid", 
             line_color = "grey") {
    # Author: Brian C. Monsell (OEUS) Version 2.3, 8/26/2024
    
    # check if a value is specified for \code{this_series}
    if (is.null(this_series)) {
        cat("must specify a ts object")
        return(this_series)
    }
	
    # check if \code{this_series} is a ts object
    if (!is.ts(this_series)) {
        cat("must specify a ts object")
		return(NULL)
    }
    
    # If start_plot specified, shorten series
    if (!is.null(start_plot)) {
        this_series <- window(this_series, start = start_plot)
    }
	
    if (is.null(y_limit)) {
		y_limit <- range(this_series)
    }
 
    if (is.null(this_y_label)) {
		if (!is.null(this_series_name)) {
			this_y_label <- this_series_name
		}
    }

    this_title <- main_title
    if (is.null(this_title)) {
		if (!is.null(this_series_name)) {
			this_title <- paste("Plot of ", this_series_name)
		}
    }

    this_df <- 
        data.frame(date = tsbox::ts_df(this_series)$time, 
				   value = as.double(this_series))
    this_plot <- 
        ggplot2::ggplot(this_df) + 
        ggplot2::geom_line(ggplot2::aes(x = .data$date, 
                                        y = .data$value),
                                        color = line_color) + 
        ggplot2::labs(title = main_title,
                      subtitle = sub_title,
                      y = this_y_label)
					  
    # remove legend from plot
    this_plot <- this_plot + ggplot2::theme(legend.position="none")
	
    # remove grid lines if \code{do_grid = FALSE} 
    if (!do_grid) {
		this_plot <- this_plot + 
			ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
		                   panel.grid.minor = ggplot2::element_blank())
    }
	
	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		this_plot <- 
			this_plot + ggplot2::theme_bw()
    }

    # add recession regions to plot
    if (draw_recess) {
	if (is.null(recess_color)) {
	    this_plot <- add_recession_shade(this_plot)
	} else {
	    this_plot <- add_recession_shade(this_plot, shade_color = recess_color)		    
	}
	if (recess_sub) {
            this_x_label <- 
                stringr::str_to_title(paste0("NBER Recessions in ", recess_color))
	}
    }
	
    this_plot <- this_plot + ggplot2::xlab(this_x_label)
	
    return(this_plot)
	
}
