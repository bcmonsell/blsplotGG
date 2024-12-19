#' Maximum percent difference plot for two sliding spans runs
#'
#' Generates a \code{ggplot} object comparing the time series of the maximum percent difference from
#' the sliding spans analysis of seasonal factors or changes for two sliding spans runs.
#' The sliding spans analysis for both series should have the same length of spans.
#'
#' Version 1.5, 12/11/2024
#'
#' @param seas_obj_one \code{seas} object generated from a call of \code{seas} on a single time series
#'        This is a required entry.
#' @param seas_obj_two \code{seas} object generated from a call of \code{seas} on a single time series
#'        This is a required entry.
#' @param this_series Character string; three character code for the type of series to be generated.
#'        Allowed entries are \code{"sfs"} (seasonal factors, default), 
#'             \code{"chs"} (period-to-period changes), 
#'             \code{"sis"} (indirect seasonal factors),
#'             \code{"cis"} (indirect period-to-period changes).
#' @param main_title Title for the plot. 
#'        Default is character string \code{'Maximum Percent Difference Comparison Plot'}.
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
#' @param this_legend_title Character string; indicates title of legend. 
#'        Default is \code{'Sliding Spans'}.
#' @param this_legend_entry Character array; entries for the legend. 
#'        Default is \code{c("ss1", "ss2")}.
#' @param legend_title_size integer scalar; Size of the legend title.
#'        Default is \code{12}.
#' @param legend_text_size integer scalar; Size of the legend title.
#'        Default is \code{10}.
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
#'
#' shoes_sspan_x11_seas <- 
#'    seasonal::seas(blsplotGG::shoes2008, 
#'                   arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", 
#'                   forecast.maxlead = 36, 
#'                   x11.seasonalma = "s3x3",
#'                   slidingspans.length = 76,
#'                   slidingspans.numspans = 4,
#'                   slidingspans.print = 'sfs',
#'                   slidingspans.save = c('sfs', 'chs'))
#' p_shoes_maxpct_sf <- 
#'     plot_sliding_spans_two(shoes_sspan_seas, shoes_sspan_x11_seas, "sfs",
#'     		main_title = "Maximum Difference of the Seasonal Factors",
#'          sub_title = "US Retail Shoe Sales",
#'          line_color = "black", 
#'          cut_color = "purple",
#'          this_legend_entry = c("Seats", "X-11"))
#' @importFrom rlang .data
#' @export
plot_sliding_spans_two <- 
    function(seas_obj_one = NULL, seas_obj_two = NULL,
             this_series = "sfs", 
             main_title = "Maximum Percent Difference Comparison Plot", 
             sub_title = NULL, 
             this_x_label = "Time", 
             this_y_label = " ", 
             do_grid = FALSE,
             do_background = FALSE,
             line_color = "grey",
			 cut_color = "red",
			 this_legend_title = "Sliding Spans",
			 this_legend_entry = c("ss1", "ss2"),
			 legend_title_size = 12,
			 legend_text_size = 10) {
    # Author: Brian C. Monsell (OEUS) Version 1.5, 12/11/2024
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj_one)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj_one, "seas")) {
            stop("First argument must be a seas object")
        }
    }
	
    if (is.null(seas_obj_two)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj_two, "seas")) {
            stop("Second argument must be a seas object")
        }
    }
	
	if(seasonal::udg(seas_obj_one, "sspans") != "yes") {
		stop("First seasonal object has no sliding spans analysis results.")
	}
	if(seasonal::udg(seas_obj_two, "sspans") != "yes") {
		stop("Second seasonal object has no sliding spans analysis results.")
	}
	
	maxpct_type_all <- c("sfs", "chs", "sis", "cis")
	if(sum(match(maxpct_type_all, this_series), na.rm = TRUE) == 0) {
        stop(paste("Sliding spans series type not valid:", this_series))
	}
	
	if(sum(match(c("chs", "cis"), this_series), na.rm = TRUE) == 0) {
        this_cut <- seasonal::udg(seas_obj_one, "sscut")[1]
	} else {
        this_cut <- seasonal::udg(seas_obj_one, "sscut")[4]
	}
	
	# check sliding spans statistics - number of spans, length of spans, 
	# start of first span
	
	if (sum(seasonal::udg(seas_obj_one, "ssa") == 
	        seasonal::udg(seas_obj_two, "ssa")) < 4) {
		stop("Number, length, and starting date for sliding spans must be identifcal")
	}
	
	# extract maximum difference
	this_matrix_one  <- seasonal::series(seas_obj_one, this_series)
	num_col          <- ncol(this_matrix_one)
	max_pct_one 	 <- 
		this_matrix_one[!is.na(this_matrix_one[,num_col]),num_col]

	this_start <- 
		c((time(this_matrix_one)[!is.na(this_matrix_one[,num_col])] %/% 1)[1],
		cycle(this_matrix_one)[!is.na(this_matrix_one[,num_col])][1])
	this_freq <-
		max(cycle(this_matrix_one)[!is.na(this_matrix_one[,num_col])])

	this_matrix_two  <- seasonal::series(seas_obj_two, this_series)
	num_col          <- ncol(this_matrix_two)
	max_pct_two 	 <- 
		this_matrix_two[!is.na(this_matrix_two[,num_col]),num_col]
	
	max_pct_one <- ts(max_pct_one, start = this_start, frequency = this_freq)
	max_pct_two <- ts(max_pct_two, start = this_start, frequency = this_freq)
	
	len_max_pct <- length(max_pct_one)

	sspan_two_df <- 
		data.frame(value1 = c(time(max_pct_one), 
	                          time(max_pct_two)),
				   value2 = c(as.double(max_pct_one), 
			                  as.double(max_pct_two)),
				   category1 = c(rep("x11", len_max_pct), 
			                     rep("seats", len_max_pct)),
				   category2 = c(seq(0, len_max_pct-1), 
			                     seq(0, len_max_pct-1)))

	p_sspan_two <- 
		ggplot2::ggplot(sspan_two_df, ggplot2::aes(.data$value1, .data$value2)) + 
		ggplot2::geom_point(ggplot2::aes(color= .data$category1)) + 
		ggplot2::geom_line(ggplot2::aes(group = .data$category2, color= line_color)) +
		ggplot2::labs(title = main_title,
					  subtitle = sub_title,
					  x = this_x_label,
                      y = this_y_label) +
		ggplot2::scale_colour_discrete(name = this_legend_title, 
		                               labels = this_legend_entry) +
		ggplot2::geom_hline(yintercept = this_cut, color = cut_color) +
		ggplot2::theme(legend.text = ggplot2::element_text(size = legend_text_size),
					   legend.title = ggplot2::element_text(size = legend_title_size))

		
	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_sspan_two <- p_sspan_two + ggplot2::theme_bw()
    }
	
	# remove grid lines if \code{do_grid = FALSE}
    if (!do_grid) {
	    p_sspan_two <- p_sspan_two + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
    }
	return(p_sspan_two)	
}