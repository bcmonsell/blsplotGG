#' Plot year over year plot 
#'
#' Generate year over year plot of a user-specified \code{ts} object.
#'
#' Version 7.6, 2026-10-28
#'
#' @param this_series A \code{ts} object; 
#'        This is a required entry.
#' @param main_title Character string; main title of plot. 
#'        The default title is the name of the series passed to this function.
#' @param sub_title Character string; subtitle of plot. There is no default subtitle.
#' @param this_y_label Character string; y-axis label for plot, if specified.
#' @param y_limit Numeric vector of length 2; Range of values you wish the plot 
#'        to be plotted over. Default is range of the series specified.
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
#'        For a listing of R color codes, 
#'        see \insertCite{rchartsColors;textual}{blsplotGG}.
#' @param this_palette Character string; \code{RColorBrewer} palette used for this plot.
#'        Default is the \code{RColorBrewer} palette \code{"Paired"} 
#'        \insertCite{RColorBrewer}{blsplotGG}. 
#' @param detrend_series Logical scalar; indicates if the series plotted is to be detrended. 
#'        Default is the original series is plotted.
#' @param detrend_lowess Logical scalar; indicates lowess is used to generate the trend 
#'        used to detrend the series. Default setting is loess is not used to detrend 
#'        the series; detrending doesn't happen unless \code{detrend_series = TRUE}.
#' @param this_legend_title Character string; sets the title associated with the plot legend.
#'        Default title is \code{"Years"}.
#' @param this_legend_position Character string; indicates the position of the plot legend.
#'        Possible values are \code{"right"}, \code{"left"}, \code{"top"}, 
#'        \code{"bottom"}, and \code{"none"}
#'        Default is \code{"right"}.
#' @param this_legend_size Numeric scalar; indicates the size of several attributes 
#'        of the plot legend, in centimeters. 
#'        Default is determined by \code{ggplot2}.
#' @param this_legend_order Character vector; indicates the order of entries in the legend.
#'        Default order is determined by \code{ggplot2}.
#' @param this_legend_space integer scalar; Space between legend and plot in points.
#'        Must be greater than or equal to zero.
#'        Default is determined by \code{ggplot2}.
#' @return Generate \code{ggplot} object generating a year to year plot of a time series object. 
#'         If time series object not specified, print out error message and return \code{NULL}.
#'         Note that this plot will likely produce warning messages - to suppress them, 
#'         use \code{suppressWarnings} when printing out the plot.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' this_yyplot <- 
#'     plot_year_over_year(AirPassengers, this_y_label = "Air", 
#'         this_palette = "Dark2")
#' legend_order <- 
#'     as.character(as.vector(matrix(1949:1960, ncol = 4, byrow = TRUE)))
#' this_yyplot_detrend <- 
#'     plot_year_over_year(AirPassengers, this_y_label = "Air", 
#'         this_palette = "Dark2",
#'         main_title = "YY Plot of Loess Detrended Air Passenger Series",
#'         detrend_series = TRUE, detrend_lowess = TRUE, 
#'         this_legend_position = "bottom",
#'         this_legend_size = 0.25,
#'         this_legend_order = legend_order,
#'         this_legend_space = 0)
#' @importFrom rlang .data
#' @importFrom Rdpack reprompt
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
			 detrend_lowess = FALSE,
			 this_legend_title = "Years",
			 this_legend_position = "right",
			 this_legend_size = NULL,
			 this_legend_order = NULL,
			 this_legend_space = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 7.6, 2026-10-28

    # check if \code{this_series} is specified
	if (is.null(this_series)) {
		cat("Argument this_series must be specified.")
		return(NULL)
	} 
	
    # check if \code{this_series} is a ts object 
	if (tsbox::ts_boxable(this_series)) {
		if (!is.ts(this_series)) {
		# attempt to convert to \code{ts} format
			this_series <- tsbox::ts_ts(this_series)
		}
	} else {
		cat("must specify a ts object for the this_series argument.")
		return(NULL)
	}

    # check if value of \code{this_legend_size} is valid, return \code{NULL} if not.
	if (!is.null(this_legend_size)) {
		if (this_legend_size <= 0) {
			cat(paste("this_legend_size not valid (must be greater than zero):", 
				this_legend_size))
			return(NULL)
		}
	}
	
    # check if value of \code{this_legend_space} is valid, return \code{NULL} if not.
	if (!is.null(this_legend_space)) {
		if (this_legend_space < 0) {
			cat(paste("this_legend_space not valid (must be greater or than equal to zero):", 
				this_legend_space))
			return(NULL)
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
  
	# check legend options 
	position_all <- c("top", "bottom", "left", "right")
	if (sum(match(position_all, this_legend_position), na.rm = TRUE) == 0) {
		cat(paste("Legend position choice not valid:", this_legend_position))
		return(NULL)
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
      
	# If start_plot specified, shorten series
	if (!is.null(start_plot)) {
		this_series <- window(this_series, start = start_plot)
		this_period <- window(this_period, start = start_plot)
		this_year   <- window(this_year, start = start_plot)
	}
      
	num_years <- length(unique(this_year))

	if (is.null(line_color)) {
		this_palette_max <- 
		RColorBrewer::brewer.pal.info[this_palette, "maxcolors"]
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
      
	if (is.null(y_limit)) {
		y_limit <- range(this_series)
	}
      
	this_freq <- max(this_period)
	this_length <- length(this_series)
      
	this_start <- start(this_series)
	this_end   <- end(this_series)
      
	this_freq_two   <- this_freq + 1
	this_length_two <- num_years * this_freq_two
	this_years_two <- unique(this_year)
      
	if (this_period[this_length] == 1) {
		this_length_two <- this_length_two - this_freq_two
		this_years_two <- this_years_two[1:(length(this_years_two)-1)]
	}
      
	this_series_two <- rep(NA, this_length_two)
      
	i0 <- 1
	i1 <- this_period[1]
	i2 <- this_freq_two
	i3 <- (this_freq_two - i1 + 1)
      
	this_series_two[i1:i2] <- this_series[i0:i3]
      
	for (i in 2:length(this_years_two)) {
		i0 <- i3
		i1 <- i2 + 1
		i2 <- i1 + this_freq
		i3 <- i3 + this_freq
		if (i3 > this_length) {
			n <- i3 - this_length
			i3 <- this_length
			i2 <- i2 - n
		}
		this_series_two[i1:i2] <- this_series[i0:i3]
	}
      
	this_start_two <- c(this_start[1], 1)
	this_series_two <- 
		ts(this_series_two, 
			start = this_start_two, 
			frequency = this_freq_two)
	this_period_two <- cycle(this_series_two)
	this_year_two <- time(this_series_two) %/% 1
      
	this_data <-
		data.frame(value = as.double(this_series_two), 
			period = as.double(this_period_two), 
			year = as.double(this_year_two))
  
	if (is.null(this_legend_order)) {
		this_data$year <- factor(this_data$year)
	} else {
		this_data$year <- 
			factor(this_data$year, levels = this_legend_order)
	}
      
	this_plot <- 
		ggplot2::ggplot(data = this_data, 
			ggplot2::aes(x = as.factor(.data$period), 
				y = .data$value, 
				group = .data$year, 
				colour = .data$year)) + 
		ggplot2::geom_line() +
		ggplot2::geom_point() +
		ggplot2::scale_colour_manual(values = line_color) +
		ggplot2::labs(title = main_title, 
			subtitle = sub_title, 
			y = this_y_label) +
		ggplot2::theme(legend.title = this_legend_title)				
      
	if (this_freq == 12) {
		this_plot <- this_plot + 
			ggplot2::scale_x_discrete(name = this_x_label, 
				breaks = 1:13, 
				labels = c(month.abb, month.abb[1]))
	} else {
		if (this_freq == 4) {
			this_q_label <- c(paste0("Q", 1:4), "Q1")
			this_plot <- this_plot + 
				ggplot2::scale_x_discrete(name = this_x_label, 
					breaks = 1:5, 
					labels = this_q_label)
		} else {
			this_p_label <- c(paste0("P", 1:this_freq), "P1")
			this_plot <- this_plot + 
				ggplot2::scale_x_discrete(name = this_x_label, 
					breaks = 1:this_freq_two, 
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
  
	if (!is.null(this_legend_position)) {
		this_plot <- this_plot +
			ggplot2::theme(legend.position = this_legend_position)
	}
  
	if (!is.null(this_legend_space)) {
		this_plot <- this_plot +
			ggplot2::theme(legend.box.spacing = ggplot2::unit(this_legend_space, "pt"))
	}
  
	if (!is.null(this_legend_size)) {
		this_plot <- this_plot + 
			ggplot2::theme(legend.key.size = ggplot2::unit(this_legend_size, 'cm'), 
				legend.key.height = ggplot2::unit(this_legend_size, 'cm'), 
				legend.key.width = ggplot2::unit(this_legend_size, 'cm'))
	}
        
	return(this_plot)
      
}