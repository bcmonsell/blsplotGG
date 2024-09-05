#' Ratio plots for two series
#'
#' Generates ratio plots for two series, and a third plot of the ratio/difference of 
#' the two series if the user requests it.
#'
#' Version 1.3, 9/2/2024
#'
#' @param ratio_one First time series of ratios/factors for which you want to generate 
#'        a high definition plot.
#'        This is a required entry.
#' @param ratio_two Second time series of ratios/factors for which you want to generate 
#'        a high definition plot.
#'        This is a required entry.
#' @param ratio_range Range of values you wish the plot to be plotted over. 
#'        Default is range of the series, if they are the same type of factor.
#' @param do_comparison_plot Logical scalar. If TRUE, a ratio/difference plot of the two
#'        factors will be generated from \code{ratio_one} and \code{ratio_two},
#'        if the series have the same periodicity and are the same type of factor.
#'        Default is TRUE.
#' @param overall_title Title for the combined plot. 
#'        Default is NULL.
#' @param ratio_one_title Title for the first plot. 
#'        Default is character string \code{'First Ratio Plot'}.
#' @param ratio_two_title Title for the plot. 
#'        Default is character string \code{'Second Ratio Plot'}.
#' @param comparison_title Title for the comparison plot, if requested. 
#'        Default is character string \code{'Comparison Plot'}.
#' @param this_x_label Label for X axis. Default is no X axis label.
#' @param this_y_label Label for Y axis. Default is no Y axis label.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines.
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param add_line Logical scalar; add solid line for assumed mean. 
#'        Default is \code{TRUE}. 
#' @param ratio_mean Numeric vector of length two; the assumed mean value for the 
#'        ratio of each series.  Default is \code{c(1.0, 1.0)}
#' @param ratio_color Color used for lines in ratio plot.  
#'        Default is \code{"steelblue"}.
#' @return A \code{ggplot} object that generates a stacked plot with the 
#         ratio plot of both series.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' ukgas_x11_seas   <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", forecast.maxlead = 20,
#'                   x11.seasonalma = "s3x5", x11.save = c("d10", "d11"),
#'                   check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_seats_seas <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", forecast.maxlead = 20,
#'                   seats.save = c("s10", "s11"),
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
#' @export
plot_ratio_two <- 
    function(ratio_one = NULL,
             ratio_two = NULL,	
	         ratio_range = NULL,
			 do_comparison_plot = TRUE,
             overall_title = NULL, 
			 ratio_one_title = "First Ratio Plot", 
			 ratio_two_title = "Second Ratio Plot",
			 comparison_title = "Comparison Plot",
             this_x_label = NULL, 
			 this_y_label = NULL, 
			 do_grid = FALSE,
			 do_background = FALSE,			 
			 add_line = TRUE, 
			 ratio_mean = c(1.0, 1.0), 
			 ratio_color = "steelblue") {
    # Author: Brian C. Monsell (OEUS) Version 1.3, 9/2/2024
    
    # check if \code{ratio_one} is specified
    if (is.null(ratio_one)) {
        cat("Argument ratio_one must be specified.")
        return(NULL)
	} else {
		if (!is.ts(ratio_one)) {
			cat("Argument ratio_one must be a ts object")
			return(NULL)
		}
    }
	
    # check if \code{ratio_two} is specified
    if (is.null(ratio_two)) {
        cat("Argument ratio_two must be specified.")
        return(NULL)
	} else {
		if (!is.ts(ratio_two)) {
			cat("Argument ratio_two must be a ts object")
			return(NULL)
		}
    }
	
	if (is.null(ratio_range)) {
		ratio_range <- range(ratio_one, ratio_two)
	}

	if (is.null(this_y_label)) {
	    this_y_label <- "Ratio"
	}
	
	all_plots_list <- list()
	
	all_plots_list[["p1"]] <-
		plot_ratio(ratio_one,
			ratio_range,
			ratio_one_title,
			this_x_label = this_x_label,
			this_y_label = this_y_label,
			do_grid = do_grid,
			do_background = do_background,			 
	        draw_recess = FALSE, 
			add_line = add_line, 
			ratio_mean = ratio_mean[1], 
			ratio_color = ratio_color)

	all_plots_list[["p2"]] <-
		plot_ratio(ratio_two,
			ratio_range,
			ratio_two_title,
			this_x_label = this_x_label,
			this_y_label = this_y_label,
			do_grid = do_grid,
			do_background = do_background,			 
	        draw_recess = FALSE, 
			add_line = add_line, 
			ratio_mean = ratio_mean[2], 
			ratio_color = ratio_color)
			
	if (do_comparison_plot) {
		do_plot <- (frequency(ratio_one) == frequency(ratio_two)) &
			(sum(start(ratio_one) == start(ratio_two)) == 2) &
			(sum(end(ratio_one) == end(ratio_two)) == 2) &
			(ratio_mean[1] == ratio_mean[2])
		if (do_plot) {
			if (ratio_mean[1] == 1.0) {
				comp_series <- ratio_one / ratio_two
			} else {
				comp_series <- ratio_one - ratio_two
			}
		}
		all_plots_list[["pComp"]] <-
			plot_ratio(comp_series,
				NULL,
				comparison_title,
				this_x_label = this_x_label,
				this_y_label = NULL,
				do_grid = do_grid,
				do_background = do_background,			 
				draw_recess = FALSE, 
				add_line = add_line, 
				ratio_mean = ratio_mean[1], 
				ratio_color = ratio_color)
	}
	
	plot_final <- 
		ggpubr::ggarrange(plotlist = all_plots_list, 
			nrow = length(all_plots_list), 
			ncol = 1)

	if (!is.null(overall_title)) {
		ggpubr::annotate_figure(plot_final, 
			top = ggpubr::text_grob(overall_title))
	}

	return(plot_final)
}
	