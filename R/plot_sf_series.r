#' Seasonal factor plot grouped by month/quarter
#'
#' Generates a special plot of the seasonal factors grouped by month/quarter. 
#'
#' Version 2.1, 9/2/2024
#'
#' @param this_sf array of seasonal factors stored as a time series
#' @param y_limit Numeric vector of length 2; Range of values you wish the plot to be plotted over. 
#'        Default is range of the seasonal factors.
#' @param this_trans Logical scalar; indicates if the adjustment was done with a log transform. 
#'        Default is TRUE.
#' @param main_title Character string; main title of plot.  Default is  \code{'Seasonal Sub-Plots'}.
#' @param sub_title Character string; subtitle of plot.  Subtitle not produced if not specified.
#' @param this_xlab Character string; label for x-axis of plot.  Default is a blank x-axis.
#' @param do_grid Logical scalar; indicates if plots will have grid lines. 
#'        Default is no grid lines.
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param this_color_sf Character string; color used for seasonal factors. 
#'        Default is \code{"darkblue"}.
#' @param this_color_mean Character string; color used for means of the seasonal factors. 
#'        Default is \code{"darkgrey"}.
#' @param first_year Integer scalar; First year used in plot. Default is start of the series.
#' @param add_mean_line Logical scalar; indicates if seasonal factor plots will include lines for seasonal means. 
#'        Default includes lines for seasonal means.
#' @param this_legend_title Character string; indicates title of legend. Default is \code{'Series'}.
#' @param this_legend_text Array of character strings; indicates text for each seasonal factor in plot. 
#'        Default is \code{c("SF", "SF Mean")}.
#' @return A \code{ggplot} object which generates a plot of the seasonal factors 
#'        (and the SI-ratios) grouped by month/quarter.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' air_seas    <- 
#'    seasonal::seas(AirPassengers, arima.model = '(0 1 1)(0 1 1)', 
#'                   x11 = '')
#' air_sf      <- seasonal::series(air_seas, 'd10')
#' plot_air_sf <- plot_sf_series(air_sf,
#'    add_mean_line = TRUE, 
#'    main_title = 'Air Passengers Seasonal Sub-Plots',
#'    this_color_sf = 'darkgreen', 
#'    this_color_mean = 'lightgreen',
#'    this_legend_title = 'X-11 Seasonal',
#'    this_legend_text = c('sf', 'mean'))
#' @import graphics
#' @import stats
#' @export
plot_sf_series <- 
    function(this_sf = NULL, 
	         y_limit = NULL,
			 this_trans = TRUE, 
			 main_title = "Seasonal Sub-Plots", 
			 sub_title = NULL, 
			 this_xlab = NULL, 
             do_grid = FALSE,
			 do_background = FALSE,
			 this_color_sf = "darkblue", 
			 this_color_mean = "darkgrey", 
			 first_year = NULL, 
			 add_mean_line = TRUE,
			 this_legend_title = "SF Plot", 
			 this_legend_text = c("SF", "SF Mean")) {
    # Author: Brian C. Monsell (OEUS) Version 2.1, 9/2/2024

    # check if a value is specified for \code{this_sf}
    if (is.null(this_sf)) {
        stop("must specify a set of seasonal factors")
    }

    # Extract cycle and frequency
    sf_period <- cycle(this_sf)
    freq <- frequency(this_sf)
    years <- time(this_sf) %/% 1

    # Set up range of x and y axis.
    if (is.null(y_limit)) {
        y_limit <- range(this_sf)
    }
    x_limit <- seq(0, freq + 1)

    # Set value of factor mean.
    if (this_trans) {
        h_bar <- 1
    } else {
        h_bar <- 0
    }

    # Generate monthly labels
    if (freq == 12) {
        this_label <-
           c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
		     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        if (is.null(this_xlab)) { this_xlab <- "Months" }
    } else {
    # Generate quarterly labels
        if (freq == 4) {
            this_label <- paste0("q", 1:4)
            if (is.null(this_xlab)) { this_xlab <- "Quarters" }
       } else {
            this_label <- paste0("p", 1:freq)
            if (is.null(this_xlab)) { this_xlab <- "Periods" }
       }
    }
    
	# set up variables for ggplot database
    if (!is.null(first_year)) {
        this_sf <- window(this_sf, start = c(first_year, 1))
    }
	length_sf <- length(this_sf)
	this_period_x  <- vector("numeric", length = length_sf)
	sf_vec         <- vector("numeric", length = length_sf)
	cycle_vec      <- vector("integer", length = length_sf)
    if (add_mean_line) {
        mean_vec   <- vector("numeric", length = length_sf)
	}
	
    # loop through each month or quarter.
	i0 <- 0
    for (i in 1:freq) {

        # produce limits for period plot.
        s1 <- (i - 1) + 0.6
        s2 <- i + 0.4

        # save seasonal factors for period i, and generate number of factors.
        sf <- this_sf[sf_period == i]
        this_year <- years[sf_period == i]
        number_sf <- length(sf)
        this_mu <- mean(sf)

        # Generate X values for period i, mean of SF for period 
		i1 <- i0 + 1
		i2 <- i0 + number_sf
        this_period_x[i1:i2] <- seq(s1, s2, length.out = number_sf)

        # save sf, cycle, and mean, if necessary 
        sf_vec[i1:i2] <- sf
		cycle_vec[i1:i2] <- rep(i, length = number_sf)
        if (add_mean_line) {
            mean_vec[i1:i2] <- rep(this_mu, length = number_sf)
        }
		# update i0 for next loop
		i0 <- i2
    }

    this_df <- 
	    data.frame(this_x = this_period_x, 
	               sf = as.double(sf_vec), 
		           cycle = as.double(cycle_vec),
		           mean = as.double(mean_vec))

    p_sf <- 
        ggplot2::ggplot(this_df) + 
        ggplot2::geom_line(ggplot2::aes(x = .data$this_x, 
                                        y = .data$sf, 
                                        group = .data$cycle,
                                        color = this_legend_text[1])) + 
        ggplot2::geom_line(ggplot2::aes(x = .data$this_x, 
                                        y = .data$mean, 
                                        group = .data$cycle,
                                        color = this_legend_text[2])) + 
        ggplot2::labs(title = main_title,
                      subtitle = sub_title) +
        ggplot2::scale_x_continuous(name = this_xlab, 
                                    breaks = 1:freq, 
                                    labels = this_label) +
        ggplot2::scale_color_manual(values = c(this_color_mean, this_color_sf)) +
		ggplot2::geom_hline(yintercept = h_bar)
    
    p_sf$labels$colour <- this_legend_title

	# remove grid lines if \code{do_grid = FALSE}
    if (!do_grid) {
	    p_sf <- p_sf + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
    }

	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_sf <- p_sf + ggplot2::theme_bw()
    }
	
	return(p_sf)
}
