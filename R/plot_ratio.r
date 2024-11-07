#' Ratio plot
#'
#' Generates a high-definition plot around a reference line other than zero.
#'
#' Version 3.2, 11/6/2024
#'
#' @param ratio_series Time series of ratios/factors for which you want to generate 
#'        a high definition plot
#' @param ratio_range Range of values you wish the plot to be plotted over. 
#'        Default is range of the series.
#' @param main_title Title for the plot. 
#'        Default is character string \code{'Ratio Plot'}.
#' @param sub_title Subtitle for the plot. Default is \code{NULL}.
#' @param this_x_label Label for X axis. Default is \code{"Time"}.
#' @param this_y_label Label for Y axis. Default is \code{"Ratio"}.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines.
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param draw_recess Logical scalar; indicates if certain plots will have shaded
#'        areas for NBER recession dates. 
#'        Default is no recession shading. 
#' @param recess_color Character string; color used for shading of recession region. 
#'        Default is \code{'lightgrey'}.
#' @param recess_sub Logical scalar; indicates if x-axis label for recession is 
#'        produced for this plot. 
#'        Default is x-axis label.
#' @param add_line Logical scalar; add solid line for assumed mean. 
#'        Default is \code{TRUE}. 
#' @param ratio_mean Assumed mean value for the ratio.  Default is \code{1.0}
#' @param ratio_color Color used for lines in ratio plot.  
#'        Default is \code{"steelblue"}.
#' @return A \code{ggplot} object that produces a ratio plot of a time series.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' ukgas_x11_seas <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", forecast.maxlead = 20,
#'                   x11.seasonalma = "s3x5", check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_x11_sf     <- seasonal::series(ukgas_x11_seas, "d10")
#' ukgas_sf_ratio   <- plot_ratio(ukgas_x11_sf, main_title = "UK Gas X-11 Seasonal Factors",
#'                                sub_title = "Seasonal Filter: 3x5")
#' @export
plot_ratio <- 
    function(ratio_series = NULL, 
	         ratio_range = NULL, 
             main_title = "Ratio Plot", 
			 sub_title = NULL, 
             this_x_label = "Time", 
			 this_y_label = "Ratio", 
			 do_grid = FALSE,
			 do_background = FALSE,			 
	         draw_recess = FALSE, 
			 recess_color = 'lightgrey', 
			 recess_sub = TRUE,  
			 add_line = TRUE, 
			 ratio_mean = 1.0, 
			 ratio_color = "steelblue") {
    # Author: Brian C. Monsell (OEUS) Version 3.2, 11/6/2024
    
    # check if \code{ratio_series} is specified
    if (is.null(ratio_series)) {
        cat("Argument ratio_series must be specified.")
        return(NULL)
	} else {
		if (!is.ts(ratio_series)) {
			cat("Argument ratio_series must be a ts object")
			return(NULL)
		}
    }
	
	if (is.null(this_y_label)) {
	    this_y_label <- "Ratio"
	}
	
	if (is.null(ratio_range)) {
		ratio_range <- range(ratio_series)
	}

    # generate data frame with ratio time series to be plotted
    this_ratio_df <-  
        data.frame(date = tsbox::ts_df(ratio_series)$time, 
                   ratio_series = as.double(ratio_series))
				   
	p_ratio <- ggplot2::ggplot(this_ratio_df) + 
        ggplot2::geom_segment(mapping=ggplot2::aes(x=date, 
                                        y=ratio_mean, 
                                        xend=date, 
                                        yend=ratio_series), 
                            color = ratio_color) +
        ggplot2::labs(title = main_title, 
                      subtitle = sub_title, 
                      y = this_y_label) + 
        ggplot2::ylim(ratio_range)
	
	if (add_line) {
        p_ratio <- p_ratio +
		    ggplot2::geom_hline(yintercept = ratio_mean, 
                                linetype = "solid", 
                                color = ratio_color)
	}
						  
	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_ratio <- p_ratio + ggplot2::theme_bw()
    }

	# remove grid lines if \code{do_grid = FALSE}
	if (!do_grid) {
	    p_ratio <- p_ratio + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
	}
       
    # Generate recession region shading
	if (draw_recess) {
	    if (is.null(recess_color)) {
	        p_ratio <- add_recession_shade(p_ratio)
		} else {
	        p_ratio <- add_recession_shade(p_ratio, shade_color = recess_color)		    
		}
		if (recess_sub) {
		    this_x_label <- 
				stringr::str_to_title(paste0("NBER Recessions in ", recess_color))
		}
	} 
	
    p_ratio <- p_ratio + ggplot2::xlab(this_x_label)
    
	return(p_ratio)

}