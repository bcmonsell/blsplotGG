#' Maximum percent difference for sliding spans by period
#'
#' Generate a plot of the maximum percent difference from a sliding spans analysis by 
#' month or quarter.
#'
#' Version 1.4, 11/7/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series
#'        This is a required entry.
#' @param this_series Character string; three character code for the type of series to be generated.
#'        Allowed entries are \code{"sfs"} (seasonal factors, default), 
#'             \code{"chs"} (period-to-period changes), 
#'             \code{"sis"} (indirect seasonal factors),
#'             \code{"cis"} (indirect period-to-period changes).
#' @param main_title Character string; main title of plot. 
#'        Default is character string \code{'Maximum Percent Difference Plot by Period'},
#'        where Period is replaced by Month or Quarter.
#' @param sub_title Character string; subtitle of plot. There is no default subtitle.
#' @param this_y_label Character string; y-axis label for plot, if specified.
#' @param this_x_label Label for X axis. Default is \code{"Month"} or \code{"Quarter"}.
#' @param do_grid Logical scalar; indicates if plots will have grid lines. 
#'        Default is no grid lines.
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background.
#' @param line_color Character scalar; color used for plot. 
#'        User should specify one color for each column of the matrix specified.
#'        Default is the \code{RColorBrewer} palette \code{"Set3"}.
#' @param this_palette Character string; default \code{RColorBrewer} palette.
#'        Default is \code{"Set3"}.
#' @param cut_color Color use to show the slidings spans cut off for this type of series.  
#'        Default is \code{"red"}.
#' @return Generate \code{ggplot} object generating a plot of the maximum percent 
#'         difference from a sliding spans analysis by month or quarter 
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' shoes_sspan_seas <- 
#'    seasonal::seas(blsplotGG::shoes2008, 
#'                   arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", 
#'                   forecast.maxlead = 36,
#'                   slidingspans.save = c('sfs', 'chs'))
#' p_shoes_maxpct_sf <- 
#'     plot_maximum_percent_difference_by_period(shoes_sspan_seas, "sfs",
#'                        main_title = "Maximum Percent Difference Plot of Seasonal Factors",
#'                        sub_title = "US Shoe Sales",
#'                        this_palette = "Paired")
#' @importFrom rlang .data
#' @export
plot_maximum_percent_difference_by_period <- 
    function(seas_obj = NULL, 
             this_series = "sfs", 
             main_title = NULL, 
			 sub_title = NULL, 
			 this_y_label = NULL, 
			 this_x_label = NULL, 
			 do_grid = FALSE, 
			 do_background = FALSE, 
			 line_color = NULL, 
			 this_palette = "Set3",
			 cut_color = "red") {
    # Author: Brian C. Monsell (OEUS) Version 1.4, 11/7/2024

    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
        }
    }
	
    maxpct_type_all <- c("sfs", "chs", "sis", "cis")
	if(sum(match(maxpct_type_all, this_series), na.rm = TRUE) == 0) {
        stop(paste("Sliding spans series type not valid:", this_plot))
	}
	
	this_freq    <- seasonal::udg(seas_obj, "freq")

	if (is.null(main_title)) {
	    main_title <- "Maximum Percent Difference Plot by "
		if (this_freq == 12) {
			main_title <- paste0(main_title, "Month")
		} else {
			main_title <- paste0(main_title, "Month")
		}
	}
	
	# extract maximum difference
	this_matrix  <- seasonal::series(seas_obj, this_series)
	num_col      <- ncol(this_matrix)
	max_pct_diff <- this_matrix[!is.na(this_matrix[,num_col]),num_col]
	
	# make \code{max_pct_diff} a ts object
	this_year    <- time(this_matrix[,num_col])[!is.na(this_matrix[,num_col])] %/% 1
	this_period  <- cycle(this_matrix[,5])[!is.na(this_matrix[,5])]
	this_start   <- c(this_year[1], this_period[1])
	base_line    <- 0.0
	
	max_pct_diff <- ts(max_pct_diff, start = this_start, frequency = this_freq)
	
	this_period <- cycle(max_pct_diff)
	this_year  <- time(max_pct_diff) %/% 1
	
	if (is.null(this_x_label)) {
		if (max(this_period) == 12) {
			this_x_label <- "Month"
		} else {
			if (max(this_period) == 4) {
				this_x_label <- "Quarter"
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
	
    # Set range of pct differences so the cutoff value is included in the plot	
	this_range   <- c(0, max(max_pct_diff))
	# extract cutoff value if values are percentages, include in range of Y-axis
	this_diff    <- seasonal::udg(seas_obj, "ssdiff")
	this_cut     <- NULL
	if (this_diff == "no") {
		cut_vec <- seasonal::udg(seas_obj, "sscut")
		if (this_series == "sfs" | this_series == "sis") {
			this_cut <- as.double(cut_vec[1,1])
		} else {
			this_cut <- as.double(cut_vec[4,1])
		}
		this_range <- range(this_range, this_cut)
	}

	this_data <-
		data.frame(value = as.double(max_pct_diff), 
		           period = as.double(this_period), 
				   year = as.factor(as.double(this_year)))

	this_plot <- ggplot2::ggplot(data = this_data, 
                          ggplot2::aes(x = as.factor(.data$period), 
                                       y = .data$value, 
                                       group = .data$year, 
                                       colour = .data$year)) + 
		ggplot2::geom_point() +
		ggplot2::ylim(this_range) +
		ggplot2::scale_colour_manual(values = line_color) +
		ggplot2::labs(title = main_title, 
		              subtitle = sub_title, 
					  y = this_y_label)
	
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
	
	# add cut line
	if (!is.null(this_cut)) {
        this_plot <- this_plot +
		    ggplot2::geom_hline(yintercept = this_cut, 
                                linetype = "solid", 
                                color = cut_color)
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