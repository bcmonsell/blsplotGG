#' Maximum percent difference for sliding spans by period
#'
#' Generate a plot of the maximum percent difference from a sliding spans analysis by 
#' month or quarter.  For more information on sliding spans analysis, 
#' \insertCite{@see Sections 6.2 and 7.13 of @Census2025;textual}{blsplotGG}, with more
#' details on sliding spans analysis in \insertCite{Findley90;textual}{blsplotGG}.
#'
#' Version 4.6, 2/18/2026
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
#' @param point_color Character scalar; color used for points in this plot. 
#'        User should specify one color for each year of the sliding spans.
#'        Default is the \code{RColorBrewer} palette \code{"Set3"}. For a listing of R color codes, 
#'        see \insertCite{rchartsColors;textual}{blsplotGG}.
#' @param this_palette Character string; default \code{RColorBrewer} palette \insertCite{RColorBrewer}{blsplotGG}.
#'        Default is \code{"Set3"}.
#' @param cut_color Color use to show the slidings spans cut off for this type of series.  
#'        Default is \code{"red"}.
#' @param this_legend_title Title for legend.  Default is \code{"Year"}.
#' @param this_legend_position character String; The position of the legend, which can be one of
#'        \code{top}, \code{bottom}, \code{left}, \code{right}.
#'        Default is \code{right}.
#' @param this_legend_size Numeric scalar; indicates the size of several attributes of the plot legend,
#'        in centimeters. Default value is \code{1}.
#' @param this_legend_order Character vector; indicates the order of entries in the legend.
#'        Default order is determined by \code{ggplot2}.
#' @param this_legend_space integer scalar; Space between legend and plot in points.
#'        Default is \code{10}.
#' @return Generate \code{ggplot} object generating a plot of the maximum percent 
#'         difference from a sliding spans analysis by month or quarter 
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
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
#' @importFrom Rdpack reprompt
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
			 point_color = NULL, 
			 this_palette = "Set3",
			 cut_color = "red", 
			 this_legend_title = "Year",
			 this_legend_position = "right",
			 this_legend_size = NULL,
			 this_legend_order = NULL,
			 this_legend_space = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 4.6, 2/18/2026

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
	
    maxpct_type_all <- c("sfs", "chs", "sis", "cis")
	if(sum(match(maxpct_type_all, this_series), na.rm = TRUE) == 0) {
        cat(paste("Sliding spans series type not valid:", this_plot))
		return(NULL)
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
	
	if (is.null(point_color)) {
	    this_palette_max <- RColorBrewer::brewer.pal.info[this_palette, "maxcolors"]
	    if (num_years < 3) {
			point_color <- RColorBrewer::brewer.pal(3, this_palette)[1:num_years]
		} else {
			if (num_years <= this_palette_max) {
				point_color <- RColorBrewer::brewer.pal(num_years, this_palette)
			} else {
				this_color <- RColorBrewer::brewer.pal(this_palette_max, this_palette)
				point_color <- grDevices::colorRampPalette(this_color)(num_years)
			}
		}
	} else {
		if (length(point_color) < num_years) {
			warning("Number of line colors specified less than number of years.")
			warning("Will use colorRampPalette to increase number of colors.")
			point_color <- grDevices::colorRampPalette(point_color)(num_years)
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

	if (is.null(this_legend_order)) {
		this_data$year <- factor(this_data$year)
	} else {
		this_data$year <- 
			factor(this_data$year, levels = this_legend_order)
	}
	
	this_plot <- ggplot2::ggplot(data = this_data, 
                          ggplot2::aes(x = as.factor(.data$period), 
                                       y = .data$value, 
                                       group = .data$year, 
                                       colour = .data$year)) + 
		ggplot2::geom_point() +
		ggplot2::ylim(this_range) +
		ggplot2::scale_colour_manual(values = point_color, 
		                             name = this_legend_title) +
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