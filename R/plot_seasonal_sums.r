#' Plot of the seasonal period length sums of the SEATS seasonal factors from 
#' an X-13ARIMA-SEATS SEATS seasonal adjustment run. 
#'
#' Generate plot of the seasonal period length sums of the SEATS seasonal factors 
#' from a SEATS adjustment from a \code{seas} object generated by the \code{seasonal}
#'  package. 
#'
#' Version 1.5, 8/29/2024
#' 
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series
#'        This is a required entry.
#' @param main_title Character string; main title of plot. 
#'        A title will be generated if no title is specified.
#' @param sub_title Character string; subtitle of plot. There is no default subtitle.
#' @param this_y_label Character string; y-axis label for plot, if specified.
#' @param this_x_label Label for X axis. Default is \code{"Time"}.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines.
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param draw_recess Logical scalar; indicates if certain plots will have 
#'        shaded areas for NBER recession dates. 
#'        Default is no recession shading. 
#' @param recess_color Character string; color used for shading of recession region. 
#'        Default is \code{'lightgrey'}.
#' @param recess_sub Logical scalar; indicates if x-axis label for recession is 
#'        produced for this plot. 
#'        Default is x-axis label.
#' @param line_color Character string; color used for series in the plot.  
#'        Default is \code{'steelblue'}. 
#' @return A \code{ggplot} object which generates a plot of the seasonal period length sums 
#'         of the SEATS seasonal factors. 
#'         If SEATS seaonal adjustent not producted, print out error message and 
#'         return NULL.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' shoes_seats_seas <- 
#'    seasonal::seas(shoes2008, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", 
#'                   forecast.maxlead = 36,
#'                   check.print = c( 'pacf', 'pacfplot' ), 
#'                   seats.finite = "yes",
#'                   seats.save = c( 'ssm' ) )
#' p_seasonal_sum <- plot_seasonal_sums(shoes_seats_seas, 
#'		sub_title = "US Shoe Sales",
#'      do_grid = TRUE, 
#'		line_color = "darkgreen")
#' @export
plot_seasonal_sums <- function(
	seas_obj = NULL, 
	main_title = NULL, 
	sub_title = NULL, 
	this_y_label = NULL, 
	this_x_label = "Time", 
	do_grid = FALSE, 
	do_background = FALSE, 
    draw_recess = FALSE, 
    recess_color = "lightgrey", 
    recess_sub = TRUE,  
	line_color = "steelblue") {
    # Author: Brian C. Monsell (OEUS) Version 1.5, 8/29/2024
	
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
	
	if (!("seatsadj" %in% names(seasonal::udg(seas_obj)))) {
		cat("Must use SEATS to generate seasonal adjustment")
		return(NULL)
	}
    
	if (is.null(main_title)) {
		main_title <- "Seasonal Period Length Sums of the SEATS Seasonal Factors"
	}
	
	p_final <- 
		blsplotGG::plot_table(seas_obj, 
			"ssm",
			main_title, 
			sub_title,
			this_y_label,
			y_limit = NULL, 
			this_x_label,
			start_plot = NULL, 
			do_grid, 
			do_background,
			draw_recess,
			recess_color,
			recess_sub,
			add_outlier = FALSE, 
            use_ratio = FALSE, 
            this_line_type = "solid", 
            line_color = line_color)

	return(p_final)
	
}
