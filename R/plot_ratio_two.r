#' Ratio plots for two series
#'
#' Generates ratio plots for two series, and a third plot of the ratio/difference of 
#' the two series if the user requests it.
#'
#' Version 3.7, 2026-09-30
#'
#' @param ratio_one First time series of ratios/factors for which you want to generate 
#'        a high definition plot.
#'        This is a required entry.
#' @param ratio_two Second time series of ratios/factors for which you want to generate 
#'        a high definition plot.
#'        This is a required entry.
#' @param ratio_range_one Range of values you wish the plot to be plotted over for the first ratio series. 
#'        Default is range of the first series.
#' @param ratio_range_two Range of values you wish the plot to be plotted over for the second ratio series. 
#'        Default is range of the second series.
#' @param ratio_mean_one Assumend mean for the first ratio series. 
#'        Default is \code{1.0} if \code{ratio_range_one} 
#'        has both elements greater than zero, \code{0.0} otherwise.
#' @param ratio_mean_two Assumend mean for the second ratio series. 
#'        Default is \code{1.0} if \code{ratio_range_two} 
#'        has both elements greater than zero, \code{0.0} otherwise.
#' @param do_comparison_plot Logical scalar. If TRUE, a ratio/difference plot of the two
#'        factors will be generated from \code{ratio_one} and \code{ratio_two},
#'        if the series have the same periodicity and are the same type of factor.
#'        Default is \code{TRUE}.
#' @param overall_title Title for the combined plot. 
#'        Default is \code{NULL}.
#' @param ratio_one_title Title for the first plot. 
#'        Default is character string \code{'First Ratio Plot'}.
#' @param ratio_two_title Title for the plot. 
#'        Default is character string \code{'Second Ratio Plot'}.
#' @param title_size integer scalar; Size of the component titles. 
#'        The overall title size will be \code{title_size} + 2.
#'        Default is \code{10}.
#' @param remove_fcst_one Integer scalar; indicates forecasts to be removed from  
#'        \code{ratio_one} before plotting.
#'        Default is no forecasts removed;
#' @param remove_fcst_two Integer scalar; indicates forecasts to be removed from 
#'        \code{ratio_two} before plotting.
#'        Default is no forecasts removed;
#' @param comparison_title Title for the comparison plot, if requested. 
#'        Default is character string \code{'Comparison Plot'}.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines.
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param add_line Logical scalar; add solid line for assumed mean. 
#'        Default is \code{TRUE}. 
#' @param ratio_mean_one Assumend mean for the first ratio series. 
#'        Default is \code{1.0} if \code{ratio_range_one} has both elements 
#'        greater than zero, \code{0.0} otherwise.
#' @param ratio_mean_two Assumend mean for the second ratio series. 
#'        Default is \code{1.0} if \code{ratio_range_two} has both elements 
#'        greater than zero, \code{0.0} otherwise.
#' @param ratio_color Color used for lines in ratio plot.  
#'        Default is \code{"steelblue"}. For a listing of R color codes, 
#'        see \insertCite{rchartsColors;textual}{blsplotGG}.
#' @return A \code{ggplot} object that generates a stacked plot with the 
#         ratio plot of both series.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' ukgas_x11_seas   <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", forecast.maxlead = 20,
#'                   x11.seasonalma = "s3x5", x11.save = c("d10", "d11"),
#'                   check.print = c( 'pacf', 'pacfplot' ), series.appendfcst = "no")
#' ukgas_seats_seas <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", forecast.maxlead = 20,
#'                   seats.save = c("s10", "s11"), series.appendfcst = "no",
#'                   check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_x11_sf      <- seasonal::series(ukgas_x11_seas, "d10")
#' ukgas_seats_sf    <- seasonal::series(ukgas_seats_seas, "s10")
#' ukgas_sf_two_plot <- 
#'      plot_ratio_two(ukgas_x11_sf, ukgas_seats_sf,
#'          overall_title = "UK Gas Production",
#'			ratio_one_title = "X-11 Seasonal Factors",
#'			ratio_two_title = "SEATS Seasonal Factors",
#'			comparison_title = "Ratio of Seasonal Factors (X11 / SEATS)",
#'          ratio_color = "darkgreen")
#' 
#' ukgas_seats_add_seas <-
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)",
#'                   transform.function = "none", forecast.maxlead = 20,
#'                   seats.save = c("s10", "s11"), series.appendfcst = "no",
#'                   check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_seats_add_sf    <- seasonal::series(ukgas_seats_add_seas, "s10")
#'
#' ukgas_sf_two_plot_add_mult <-
#'     plot_ratio_two(ukgas_seats_add_sf, ukgas_seats_sf,
#'          overall_title = "UK Gas Production",
#'          ratio_one_title = "SEATS Additive Seasonal Factors",
#'          ratio_two_title = "SEATS Seasonal Factors",
#'          do_comparison_plot = FALSE,
#'          ratio_color = "darkgreen")
#'
#' @importFrom Rdpack reprompt
#' @export
plot_ratio_two <- 
    function(ratio_one = NULL,
             ratio_two = NULL,
             ratio_range_one = NULL,
             ratio_range_two = NULL,
             do_comparison_plot = TRUE,
             overall_title = NULL, 
             ratio_one_title = "First Ratio Plot", 
             ratio_two_title = "Second Ratio Plot",
             title_size = 10,
             remove_fcst_one = NULL,
             remove_fcst_two = NULL,
             comparison_title = "Comparison Plot",
             do_grid = FALSE,
             do_background = FALSE, 
             add_line = TRUE, 
             ratio_mean_one = NULL,
             ratio_mean_two = NULL,
             ratio_color = "steelblue") {
    # Author: Brian C. Monsell (OEUS) Version 3.7, 2026-09-30
    
    # check if \code{ratio_one} is specified
	if (is.null(ratio_one)) {
		cat("Argument ratio_one must be specified.")
		return(NULL)
	} else {
		if (tsbox::ts_boxable(ratio_one)) {
			if (!is.ts(ratio_one)) {
	# attempt to convert to \code{ts} format
				ratio_one <- tsbox::ts_ts(ratio_one)
			}
		} else {
			cat("must specify a ts object for the ratio_one argument.")
			return(NULL)
		}
	}
	
    # check if \code{ratio_two} is specified
	if (is.null(ratio_two)) {
		cat("Argument ratio_two must be specified.")
		return(NULL)
      } else {
		if (tsbox::ts_boxable(ratio_two)) {
			if (!is.ts(ratio_two)) {
	# attempt to convert to \code{ts} format
				ratio_two <- tsbox::ts_ts(ratio_two)
			}
		} else {
			cat("must specify a ts object for the ratio_two argument.")
			return(NULL)
		}
    }
	
    # check if value of \code{title_size} is valid, return \code{NULL} if not.
	if (title_size <= 0) {
		cat(paste("Legend text size not valid (must be greater than zero):", 
			title_size))
		return(NULL)
	}
	
	if (!is.null(remove_fcst_one)) {
		this_end <- length(ratio_one) - remove_fcst_one
		this_start <- start(ratio_one)
		this_freq  <- frequency(ratio_one)
		ratio_one <- 
			ts(ratio_one[1:this_end], start = this_start, frequency = this_freq)
	}
      
	if (!is.null(remove_fcst_two)) {
		this_end <- length(ratio_two) - remove_fcst_two
 		this_start <- start(ratio_two)
		this_freq  <- frequency(ratio_two)
		ratio_two <- 
			ts(ratio_two[1:this_end], start = this_start, frequency = this_freq)
	}
  
	if (is.null(ratio_range_one)) {
		ratio_range_one <- range(ratio_one)
	}
	
	this_index_one <- sum(ratio_range_one > 0)
	if (is.null(ratio_mean_one)) {
		if (this_index_one > 1) { 
			ratio_mean_one <- 1.0
		} else {
			ratio_mean_one <- 0.0
		}
	}

	if (is.null(ratio_range_two)) {
		ratio_range_two <- range(ratio_two)
	}
	
	this_index_two <- sum(ratio_range_two > 0)
	if (is.null(ratio_mean_two)) {
		if (this_index_two > 1) { 
			ratio_mean_two <- 1.0
		} else {
			ratio_mean_two <- 0.0
		}
	}

	all_plots_list <- list()
	
	all_plots_list[["p1"]] <-
		plot_ratio(ratio_one,
			ratio_range_one,
			ratio_one_title,
			this_x_label = NULL,
			this_y_label = NULL,
			do_grid = do_grid,
			do_background = do_background,
			draw_recess = FALSE, 
			add_line = add_line, 
			ratio_mean = ratio_mean_one,
			ratio_color = ratio_color) + 
	  ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))

	all_plots_list[["p2"]] <-
		plot_ratio(ratio_two,
			ratio_range_two,
			ratio_two_title,
			this_x_label = NULL,
			this_y_label = NULL,
			do_grid = do_grid,
			do_background = do_background,
			draw_recess = FALSE, 
			add_line = add_line, 
			ratio_mean = ratio_mean_two, 
			ratio_color = ratio_color) + 
	  ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))
			
	if (do_comparison_plot) {
		freq_agree <- (frequency(ratio_one) == frequency(ratio_two))
		start_agree <- (sum(start(ratio_one) == start(ratio_two)) == 2)
		end_agree <- (sum(end(ratio_one) == end(ratio_two)) == 2)
		ratio_mean_agree <- (ratio_mean_one == ratio_mean_two)
		
		do_plot <- freq_agree & start_agree & end_agree & ratio_mean_agree
			
		if (do_plot) {
			if (ratio_mean_one == 1.0) {
				comp_series <- ratio_one / ratio_two
			} else {
				comp_series <- ratio_one - ratio_two
			}
			all_plots_list[["pComp"]] <-
				plot_ratio(comp_series,
					NULL,
					comparison_title,
					this_x_label = NULL,
					this_y_label = NULL,
					do_grid = do_grid,
					do_background = do_background,			 
					draw_recess = FALSE, 
					add_line = add_line, 
					ratio_mean = ratio_mean_one, 
					ratio_color = ratio_color) + 
				ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))
		} else {
			this_warn_message <- "Ratio series not compatable due to "
			if (!freq_agree) {
				this_warn_message <- 
					paste0(this_warn_message, "different frequencies")
				need_comma <- sum(start_agree, end_agree, ratio_mean_agree )
				if (need_comma < 3) {
					this_warn_message <- paste0(this_warn_message, ", ")
				} else {
					this_warn_message <- paste0(this_warn_message, ".")
				}
			}
			if (!start_agree) {
				this_warn_message <- 
					paste0(this_warn_message, "different start dates")
				need_comma <- sum(end_agree, ratio_mean_agree )
				if (need_comma < 2) {
					this_warn_message <- paste0(this_warn_message, ", ")
				} else {
					this_warn_message <- paste0(this_warn_message, ".")
				}
			}
			if (!end_agree) {
				this_warn_message <- 
					paste0(this_warn_message, "different start dates")
				need_comma <- sum(end_agree, ratio_mean_agree )
				if (need_comma < 1) {
					this_warn_message <- paste0(this_warn_message, ", ")
				} else {
					this_warn_message <- paste0(this_warn_message, ".")
				}
			}
			if (!ratio_mean_agree) {
				this_warn_message <- 
					paste0(this_warn_message, "different factor types.")
			}
			cat(this_warn_message)
			cat("Plot returned will not have comparison panel")
		}
	} 
	
	plot_final <- 
		ggpubr::ggarrange(plotlist = all_plots_list, 
			nrow = length(all_plots_list), 
			ncol = 1)

	if (!is.null(overall_title)) {
		plot_final <- ggpubr::annotate_figure(plot_final, 
			top = ggpubr::text_grob(overall_title, size = title_size + 2))
	}

	return(plot_final)
}
	