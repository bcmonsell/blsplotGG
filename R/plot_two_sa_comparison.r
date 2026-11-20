#' Compare two seasonal adjustments
#'
#' Generates a \code{ggplot} object with a time series plot that compares two seasonal adjustments
#' of the same series, with an optional comparison panel between the two adjustments.
#'
#' Version 2.4, 2026-09-30
#'
#' @param this_sa_one Time series of the first seasonal adjustment.
#'        This is a required entry.
#' @param this_sa_two Time series of the SEATS seasonal adjustment.
#'        This is a required entry.
#' @param do_comparison_plot Logical scalar. If TRUE, a ratio/difference plot of the two
#'        seasonal adjustments will be generated from \code{this_sa_one} and \code{this_sa_two},
#'        if the series have the same periodicity.
#'        Default is \code{TRUE}.
#' @param overall_title Title for the combined plot. 
#'        Default is \code{NULL}.
#' @param sa_one_title Title for the first plot. 
#'        Default is character string \code{'First SA Plot'}.
#' @param sa_two_title Title for the plot. 
#'        Default is character string \code{'Second SA Plot'}.
#' @param comparison_title Title for the comparison plot, if requested. 
#'        Default is character string \code{'Comparison Plot'}.
#' @param title_size integer scalar; Size of the component titles. 
#'        The overall title size will be \code{title_size} + 2.
#'        Default is \code{10}.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param this_scale Character scalar; set the scale for the y-axis of the facet plots. 
#'        Default is \code{"fixed"}.  To set axis to be different for the different plots,
#'        set \code{this_scale = "free_y"}.
#' @param line_color Character string; color used for lines in plot.  
#'        Default is \code{"steelblue"}. For a listing of R color codes, 
#'        see \insertCite{rchartsColors;textual}{blsplotGG}.
#' @param ratio_color Character string; Line color for the comparison ratio plot.
#'        Deault is \code{"grey"}.
#' @return A \code{ggplot} object that generates a plot comparing two seasonal adjustments,
#'         with an optional comparison panel.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' ukgas_x11_seas   <- 
#'    seasonal::seas(UKgas, series.period = 4, 
#'        arima.model = "(0 1 1)(0 1 1)", 
#'        transform.function = "log", forecast.maxlead = 20,
#'        x11.seasonalma = "s3x5", 
#'        check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_seats_seas <- 
#'    seasonal::seas(UKgas, series.period = 4,  
#'        arima.model = "(0 1 1)(0 1 1)", 
#'        transform.function = "log", forecast.maxlead = 20,
#'        check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_x11_sa     <- seasonal::final(ukgas_x11_seas)
#' ukgas_seats_sa   <- seasonal::final(ukgas_seats_seas)
#' ukgas_seats_v_x11_p <- 
#'     plot_two_sa_comparison(
#'         this_sa_one = ukgas_x11_sa, 
#'         this_sa_two = ukgas_seats_sa,
#'         overall_title = "UK Gas Production",
#'         sa_one_title = "X-11 Seasonal Adjustment",
#'         sa_two_title = "SEATS Seasonal Adjustment",
#'         comparison_title = "Ratio of Seasonal Adjustments (X11 / SEATS)")
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom Rdpack reprompt
#' @export
plot_two_sa_comparison <- 
    function(this_sa_one = NULL, 
		this_sa_two = NULL, 
		do_comparison_plot = TRUE, 
		overall_title = "Compare X-11 and SEATS", 
		sa_one_title = "First SA Plot", 
		sa_two_title = "Second SA Plot", 
		comparison_title = "Comparison Plot",
		title_size = 10,
		do_grid = FALSE,
		do_background = FALSE,
		this_scale = "fixed",
		line_color = "steelblue", 
		ratio_color = "grey") {
    # Author: Brian C. Monsell (OEUS) Version 2.4, 2026-09-30

    if (is.null(this_sa_one)) {
        cat("must specify the first seasonally adjusted series")
        return(NULL)
    } else {
        # check if \code{this_sa_one} is a ts object
		if (!is.ts(this_sa_one)) {
			cat("must specify a ts object")
            return(NULL)
		}
    }
    if (is.null(this_sa_two)) {
        cat("must specify the second seasonally adjusted series")
        return(NULL)
    } else {
        # check if \code{this_sa_two} is a ts object
		if (!is.ts(this_sa_two)) {
			cat("must specify a ts object")
				return(NULL)
		}
    }
	
    # check if value of \code{title_size} is valid, return \code{NULL} if not.
	if (title_size <= 0) {
        cat(paste("Title size not valid (must be greater than zero):", 
		           title_size))
        return(NULL)
	}
	
	this_range <- NULL
	if (this_scale == "fixed") {
		this_range <- range(this_sa_one, this_sa_two)
	}
	
	all_plots_list <- list()
	
	all_plots_list[["p1"]] <- 
		plot_series(this_sa_one,
			plot_range = this_range,
			main_title = sa_one_title,
			this_x_label = NULL,
			this_y_label = NULL,
			do_grid = do_grid,
			do_background = do_background,
			draw_recess = FALSE,
			line_color = line_color) + 
	  ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))

	all_plots_list[["p2"]] <- 
		plot_series(this_sa_two,
			plot_range = this_range,
			main_title = sa_two_title,
			this_x_label = NULL,
			this_y_label = NULL,
			do_grid = do_grid,
			do_background = do_background,
			draw_recess = FALSE,
			line_color = line_color) + 
	  ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))
			
	if (do_comparison_plot) {
		do_plot <- (frequency(this_sa_one) == frequency(this_sa_two)) &
			(sum(start(this_sa_one) == start(this_sa_two)) == 2) &
			(sum(end(this_sa_one) == end(this_sa_two)) == 2)
			
		if (do_plot) {

			allpos_one <- sum(this_sa_one <= 0.0) > 0
			allpos_two <- sum(this_sa_two <= 0.0) > 0
			allpos <- sum(allpos_one, allpos_two)
		
			if (allpos == 0) {
				comp_series <- this_sa_one / this_sa_two
				
				all_plots_list[["pComp"]] <-
					plot_ratio(comp_series,
						NULL,
						comparison_title,
						this_x_label = NULL,
						this_y_label = NULL,
						do_grid = do_grid,
						do_background = do_background,			 
						draw_recess = FALSE, 
						add_line = TRUE, 
						ratio_mean = 1.0, 
						ratio_color = ratio_color) + 
					ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))
			} else {
				comp_series <- this_sa_one - this_sa_two
				
				all_plots_list[["pComp"]] <-
					plot_ratio(comp_series,
						NULL,
						comparison_title,
						this_x_label = NULL,
						this_y_label = NULL,
						do_grid = do_grid,
						do_background = do_background,			 
						draw_recess = FALSE, 
						add_line = TRUE, 
						ratio_mean = 0.0, 
						ratio_color = ratio_color) + 
					ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))
			}				
		}
	}
	
	plot_final <- 
		ggpubr::ggarrange(plotlist = all_plots_list, 
			nrow = length(all_plots_list), 
			ncol = 1)

	if (!is.null(overall_title)) {
		plot_final <- ggpubr::annotate_figure(plot_final, 
			top = ggpubr::text_grob(overall_title, 
			                        size = title_size + 2))
	}
	
	return(plot_final)
	
}