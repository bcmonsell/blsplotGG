#'
#' Seasonal factor plot grouped by month/quarter for two series
#'
#' Generates a plot of the seasonal factors grouped by month/quarter
#' for two adjustments. 
#'
#' Version 1.2, 12/11/2024
#'
#' @param this_sf_one array of seasonal factors stored as a time series
#' @param this_sf_two array of seasonal factors stored as a time series
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
#' @param this_color_sf Vector of character strings; colors used for seasonal factors. Should be of length two. 
#'        Default is \code{NULL}, which indicates that the palette specified 
#'        in \code{this_palette} is used to generate colors for this plot.
#' @param this_color_mean Vector of character strings; color used for means of the seasonal factors. Should be of length two.
#'        Default is \code{NULL}, which indicates that the palette specified 
#'        in \code{this_palette} is used to generate colors for this plot.
#' @param this_palette Color used for lines in plot. Default is \code{"Paired"}
#' @param first_year Integer scalar; First year used in plot. Default is start of the series.
#' @param this_legend_title Character string; indicates title of legend. Default is \code{'Series'}.
#' @param this_legend_text Array of character strings; indicates text for each seasonal factor in plot. 
#'        Default is \code{c("SF One", "SF Mean One", "SF Two", "SF Mean Two"))}.
#' @param legend_title_size integer scalar; Size of the legend title.
#'        Default is \code{12}.
#' @param legend_text_size integer scalar; Size of the legend title.
#'        Default is \code{10}.
#' @return A \code{ggplot} object which generates a plot of the seasonal factors 
#'        (and the SI-ratios) grouped by month/quarter.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas_x11    <- 
#'    seasonal::seas(AirPassengers, arima.model = '(0 1 1)(0 1 1)', 
#'                   x11 = '')
#' air_sf_x11      <- seasonal::series(air_seas_x11, 'd10')
#' air_seas_seats  <- 
#'    seasonal::seas(AirPassengers, arima.model = '(0 1 1)(0 1 1)', 
#'                   seats.save = "s10")
#' air_sf_seats    <- seasonal::series(air_seas_seats, 's10')
#' plot_air_sf_two <- plot_sf_two(air_sf_x11, air_sf_seats,
#'    main_title = 'Air Passengers Seasonal Sub-Plots',
#'    this_legend_text = c('X-11 sf', 'X-11 mean', 'SEATS sf', 'SEATS mean'),
#'    legend_title_size = 10,
#'    legend_text_size = 8)
#' @import graphics
#' @import stats
#' @export
plot_sf_two <- 
    function(this_sf_one = NULL, 
	         this_sf_two = NULL,
	         y_limit = NULL,
			 this_trans = TRUE, 
			 main_title = "Seasonal Sub-Plots", 
			 sub_title = NULL, 
			 this_xlab = NULL, 
             do_grid = FALSE,
			 do_background = FALSE,
			 this_color_sf = NULL, 
			 this_color_mean = NULL, 
			 this_palette = "Paired", 
			 first_year = NULL, 
			 this_legend_title = "SF Plot", 
			 this_legend_text = c("SF One", "SF Mean One", "SF Two", "SF Mean Two"),
			 legend_title_size = 12,
			 legend_text_size = 10) {
    # Author: Brian C. Monsell (OEUS) Version 1.2, 12/11/2024

    # check if a value is specified for \code{this_sf}
    if (is.null(this_sf_one)) {
        stop("must specify a set of seasonal factors for this_sf_one")
		if (!is.ts(this_sf_one)) {
			stop("must specify a ts object for this_sf_one")
		}
    }
    if (is.null(this_sf_two)) {
        stop("must specify a set of seasonal factors for this_sf_two")
		if (!is.ts(this_sf_two)) {
			stop("must specify a ts object for this_sf_two")
		}
    }

	freq_one <- frequency(this_sf_one)
	freq_two <- frequency(this_sf_two)
	if (freq_one != freq_two) {
		stop("Seasonal factors must have the same frequency")
	} else {
		freq <- freq_one
	}

    # Set up range of x and y axis.
    if (is.null(y_limit)) {
        y_limit <- range(this_sf_one, this_sf_two)
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
        this_label <- month.abb
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
        this_sf_one <- window(this_sf_one, start = c(first_year, 1))
        this_sf_two <- window(this_sf_two, start = c(first_year, 1))
	  } else {
		  if (sum(start(this_sf_one) == start(this_sf_one)) < 2) {
			stop("Seasonal factors must start on the same date")
		}
  }
	length_sf_one <- length(this_sf_one)
	length_sf_two <- length(this_sf_two)
	
	if (length_sf_one != length_sf_two) {
		stop("Seasonal factors should be the same length")
	} else {
		length_sf <- length_sf_one
	}
	
	# Extract cycle and frequency
    sf_period_one <- cycle(this_sf_one)
    years_one <- time(this_sf_one) %/% 1
	
    sf_period_two <- cycle(this_sf_two)
    years_two <- time(this_sf_two) %/% 1
	
	this_period_x  <- vector("numeric", length = length_sf)
	sf_vec_one     <- vector("numeric", length = length_sf)
	cycle_vec_one  <- vector("integer", length = length_sf)
    mean_vec_one   <- vector("numeric", length = length_sf)
	sf_vec_two     <- vector("numeric", length = length_sf)
	cycle_vec_two  <- vector("integer", length = length_sf)
    mean_vec_two   <- vector("numeric", length = length_sf)
	
    # loop through each month or quarter.
	i0 <- 0
    for (i in 1:freq) {

        # produce limits for period plot.
        s1 <- (i - 1) + 0.6
        s2 <- i + 0.4

        # save seasonal factors for period i, and generate number of factors.
        sf1 <- this_sf_one[sf_period_one == i]
        this_year_one <- years_one[sf_period_one == i]
        number_sf1 <- length(sf1)
        this_mu_one <- mean(sf1)

        # save seasonal factors for period i, and generate number of factors.
        sf2 <- this_sf_two[sf_period_two == i]
        this_year_two <- years_two[sf_period_two == i]
        number_sf <- length(sf2)
        this_mu_two <- mean(sf2)

        # Generate X values for period i, mean of SF for period 
		i1 <- i0 + 1
		i2 <- i0 + number_sf
        this_period_x[i1:i2] <- seq(s1, s2, length.out = number_sf)

        # save sf, cycle, and mean, if necessary 
        sf_vec_one[i1:i2] <- sf1
		cycle_vec_one[i1:i2] <- rep(i, length = number_sf)
        mean_vec_one[i1:i2] <- rep(this_mu_one, length = number_sf)

        sf_vec_two[i1:i2] <- sf2
		cycle_vec_two[i1:i2] <- rep(i, length = number_sf)
        mean_vec_two[i1:i2] <- rep(this_mu_two, length = number_sf)

		# update i0 for next loop
		i0 <- i2
    }
	
	this_color <- NULL
	if (is.null(this_color_sf)) {
		this_color <- RColorBrewer::brewer.pal(4, this_palette)
		this_color_sf <- c(this_color[2], this_color[4])
	}
	if (is.null(this_color_mean)) {
		if (is.null(this_color)) {
			this_color <- RColorBrewer::brewer.pal(4, this_palette)
		}
		this_color_mean <- c(this_color[1], this_color[3])
	}

    this_df <- 
	    data.frame(this_x = this_period_x, 
	               sf1 = as.double(sf_vec_one), 
		           cycle1 = as.double(cycle_vec_one),
		           mean1 = as.double(mean_vec_one), 
	               sf2 = as.double(sf_vec_two), 
		           cycle2 = as.double(cycle_vec_two),
		           mean2 = as.double(mean_vec_two))

    p_sf <- 
        ggplot2::ggplot(this_df) + 
        ggplot2::geom_line(ggplot2::aes(x = .data$this_x, 
                                        y = .data$sf1, 
                                        group = .data$cycle1,
                                        color = this_legend_text[1])) + 
        ggplot2::geom_line(ggplot2::aes(x = .data$this_x, 
                                        y = .data$mean1, 
                                        group = .data$cycle1,
                                        color = this_legend_text[2]))+ 
        ggplot2::geom_line(ggplot2::aes(x = .data$this_x, 
                                        y = .data$sf2, 
                                        group = .data$cycle2,
                                        color = this_legend_text[3])) + 
        ggplot2::geom_line(ggplot2::aes(x = .data$this_x, 
                                        y = .data$mean2, 
                                        group = .data$cycle2,
                                        color = this_legend_text[4])) + 
        ggplot2::labs(title = main_title,
                      subtitle = sub_title) +
        ggplot2::scale_x_continuous(name = this_xlab, 
                                    breaks = 1:freq, 
                                    labels = this_label) +
        ggplot2::scale_color_manual(values = c(this_color_mean, this_color_sf)) +
		ggplot2::geom_hline(yintercept = h_bar) +
		ggplot2::theme(legend.text = ggplot2::element_text(size = legend_text_size),
					   legend.title = ggplot2::element_text(size = legend_title_size))
    
    p_sf$labels$colour <- this_legend_title

	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_sf <- p_sf + ggplot2::theme_bw()
    }
	
	# remove grid lines if \code{do_grid = FALSE}
    if (!do_grid) {
	    p_sf <- p_sf + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
    }

	return(p_sf)
}
