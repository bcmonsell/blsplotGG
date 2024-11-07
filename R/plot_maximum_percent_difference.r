#' Maximum percent difference plot
#'
#' Generates a \code{ggplot} object with a time series of the maximum percent difference from
#' a sliding spans analysis of seasonal factors or changes.
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
#' @param main_title Title for the plot. 
#'        Default is character string \code{'Maximum Percent Difference Plot'}.
#' @param sub_title Subtitle for the plot. Optional entry.
#' @param this_x_label Label for X-axis.  Default is \code{"Time"}
#' @param this_y_label Label for Y-axis.  Default is \code{" "}
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param line_color Color used for lines in the maximum percentage difference plot.  
#'        Default is \code{"steelblue"}.
#' @param cut_color Color use to show the slidings spans cut off for this type of series.  
#'        Default is \code{"red"}.
#' @return A \code{ggplot} object that generates a plot of the maximum percent difference
#'         from a sliding spans analysis of seasonal factors or changes.
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
#'     plot_maximum_percent_difference(shoes_sspan_seas, "sfs",
#'                        main_title = "Maximum Percent Difference Plot of Seasonal Factors",
#'                        sub_title = "US Shoe Sales",
#'                        line_color = "blue", cut_color = "purple")
#' @importFrom rlang .data
#' @export
plot_maximum_percent_difference <- 
    function(seas_obj = NULL, 
             this_series = "sfs", 
             main_title = "Maximum Percent Difference Plot", 
             sub_title = NULL, 
             this_x_label = "Time", 
             this_y_label = " ", 
             do_grid = FALSE,
             do_background = FALSE,
             line_color = "steelblue",
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
        stop(paste("Sliding spans series type not valid:", this_series))
	}
	
	# extract maximum difference
	this_matrix  <- seasonal::series(seas_obj, this_series)
	num_col      <- ncol(this_matrix)
	max_pct_diff <- this_matrix[!is.na(this_matrix[,num_col]),num_col]
	
	# make \code{max_pct_diff} a ts object
	this_year    <- time(this_matrix[,num_col])[!is.na(this_matrix[,num_col])] %/% 1
	this_period  <- cycle(this_matrix[,5])[!is.na(this_matrix[,5])]
	this_start   <- c(this_year[1], this_period[1])
	this_freq    <- seasonal::udg(seas_obj, "freq")
	base_line    <- 0.0
	
	max_pct_diff <- ts(max_pct_diff, start = this_start, frequency = this_freq)

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
	
	# create data frame for plot
	max_df <- 
		data.frame(date = tsbox::ts_df(max_pct_diff)$time,
	               maxdiff = as.double(max_pct_diff))
		  
	# generate plot of maximum pct differences
	p_max <- ggplot2::ggplot(max_df) + 
        ggplot2::geom_segment(mapping=ggplot2::aes(x=.data$date, 
                                        y=base_line, 
                                        xend=.data$date, 
                                        yend=.data$maxdiff), 
                            color = line_color) +
        ggplot2::labs(title = main_title, 
                      subtitle = sub_title, 
                      y = this_y_label,
					  x = this_x_label)	+
		ggplot2::ylim(this_range)
	
	# add cut line
	if (!is.null(this_cut)) {
        p_max <- p_max +
		    ggplot2::geom_hline(yintercept = this_cut, 
                                linetype = "solid", 
                                color = cut_color)
	}
						  
	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_max <- p_max + ggplot2::theme_bw()
    }
	
	# remove grid lines if \code{do_grid = FALSE}
	if (!do_grid) {
	    p_max <- p_max + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
	}
       
	return(p_max)
	
}
