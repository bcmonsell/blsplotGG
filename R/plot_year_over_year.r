#' Plot year over year plot 
#'
#' Generate year over year plot of a user-specified ts object.
#'
#' Version 3.2, 11/6/2024
#'
#' @param this_series Numeric matrix; columns of time series object to be plotted.
#' @param main_title Character string; main title of plot. 
#'        The default title is the name of the series passed to this function.
#' @param sub_title Character string; subtitle of plot. There is no default subtitle.
#' @param this_y_label Character string; y-axis label for plot, if specified.
#' @param y_limit Numeric vector of length 2; Range of values you wish the plot to be plotted over. 
#'        Default is range of the series specified.
#' @param this_x_label Label for X axis. Default is \code{"Month"} or \code{"Quarter"}.
#' @param start_plot Integer vector of length 2; Starting date for plot. 
#'        Default is starting date for the time series.
#' @param do_grid Logical scalar; indicates if plots will have grid lines. 
#'        Default is no grid lines.
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background.
#' @param line_color Character scalar; color used for plot. 
#'        User should specify one color for each column of the matrix specified.
#'        Default is the \code{RColorBrewer} palette \code{"Paired"}.
#' @param this_palette Character string; default \code{RColorBrewer} palette.
#'        Default is \code{"Paired"}.
#' @param detrend_series Logical scalar; indicates if the series plotted is to be detrended. 
#'        Default is the original series is plotted.
#' @param detrend_lowess Logical scalar; indicates lowess is used to generate the trend 
#'        used to detrend the series. Default is loess is not used.
#' @return Generate \code{ggplot} object generating a year to year plot of a time series object. 
#'        If time series object not specified, print out error message and return NULL.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' this_yyplot <- 
#'     plot_year_over_year(AirPassengers, this_y_label = "Air", this_palette = "Dark2")
#' this_yyplot_detrend <- 
#'     plot_year_over_year(AirPassengers, this_y_label = "Air", this_palette = "Dark2",
#'                         detrend_series = TRUE, detrend_lowess = TRUE)
#' @importFrom rlang .data
#' @export
plot_year_over_year <- 
    function(this_series = NULL, 
	         main_title = NULL, 
			 sub_title = NULL, 
			 this_y_label = NULL, 
	         y_limit = NULL, 
			 this_x_label = NULL, 
			 start_plot = NULL, 
			 do_grid = FALSE, 
			 do_background = FALSE, 
			 line_color = NULL, 
			 this_palette = "Paired",
			 detrend_series = FALSE,
			 detrend_lowess = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 3.2, 11/6/2024

    if (is.null(this_series)) {
        stop("Argument this_series must be specified.")
    } else {
		if (!is.ts(this_series)) {
			stop("Argument this_series must be a ts object.")
		}
	}
	
	if (is.null(main_title)) {
	    main_title <- "Year-Over-Year Plot of "
		if (detrend_lowess) {
			main_title <- paste0(main_title, "Lowess ")
		}
		if (detrend_series) {
			main_title <- paste0(main_title, "Detrended ")
		}
	    main_title <- paste0(main_title, deparse(substitute(this_series)))
	}
	
	if (detrend_series) {
		if (detrend_lowess) {
			this_series <- astsa::detrend(this_series, lowess = TRUE)
		} else {
			this_series <- astsa::detrend(this_series)
		}
	}

	this_period <- cycle(this_series)
	this_year  <- time(this_series) %/% 1
	
	if (is.null(this_x_label)) {
		if (max(this_period) == 12) {
			this_x_label <- "Month"
		} else {
			if (max(this_period) == 4) {
				this_x_label <- "Quarter"
			} else {
				this_x_label <- "Period"
			}
		}
	} 
	
	num_years <- length(unique(this_year))
	
	if (is.null(line_color)) {
	    this_palette_max <- RColorBrewer::brewer.pal.info[this_palette, "maxcolors"]
	    if (num_years < 3) {
			line_color <- RColorBrewer::brewer.pal(3, this_palette)[1:num_years]
		} else {
			if (num_years <= this_palette_max) {
				line_color <- RColorBrewer::brewer.pal(num_years, this_palette)
			} else {
				this_color <- RColorBrewer::brewer.pal(this_palette_max, this_palette)
				line_color <- grDevices::colorRampPalette(this_color)(num_years)
			}
		}
	} else {
		if (length(line_color) < num_years) {
			warning("Number of line colors specified less than number of years.")
			warning("Will use colorRampPalette to increase number of colors.")
			line_color <- grDevices::colorRampPalette(line_color)(num_years)
		}
	}
	

    # If start_plot specified, shorten series
    if (!is.null(start_plot)) {
        this_series <- window(this_series, start = start_plot)
    }
    
	if (is.null(y_limit)) {
		y_limit <- range(this_series)
	}
 
	this_data <-
		data.frame(value = as.double(this_series), 
		           period = as.double(this_period), 
				   year = as.factor(as.double(this_year)))

	this_plot <- ggplot2::ggplot(data = this_data, 
                          ggplot2::aes(x = as.factor(.data$period), 
                                       y = .data$value, 
                                       group = .data$year, 
                                       colour = .data$year)) + 
		ggplot2::geom_line() +
		ggplot2::geom_point() +
		ggplot2::scale_colour_manual(values = line_color) +
		ggplot2::labs(title = main_title, 
		              subtitle = sub_title, 
					  y = this_y_label)
	
	this_freq <- max(this_period)
	if (this_freq == 12) {
		this_plot <- this_plot + 
			ggplot2::scale_x_discrete(name = this_x_label, 
                                      breaks = 1:12, 
                                      labels = month.abb)
	} else {
		if (this_freq == 4) {
			this_q_label <- paste0("Q", 1:4)
			this_plot <- this_plot + 
				ggplot2::scale_x_discrete(name = this_x_label, 
									      breaks = 1:4, 
                                          labels = this_q_label)
		} else {
			this_p_label <- paste0("P", 1:this_freq)
			this_plot <- this_plot + 
				ggplot2::scale_x_discrete(name = this_x_label, 
									      breaks = 1:this_freq, 
                                          labels = this_p_label)
		}
	}

	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		this_plot <- this_plot + ggplot2::theme_bw()
    }

	# remove grid lines if \code{do_grid = FALSE}
    if (!do_grid) {
	    this_plot <- this_plot + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
    }

	return(this_plot)
	
}