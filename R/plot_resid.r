#' Residual plot
#'
#' Generates a plot of the regARIMA residuals with diagnostic information
#'
#' Version 2.5, 8/26/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series
#'        This is a requited entry.
#' @param main_title Character string; main title of plot.  
#'        Default is  \code{'ARIMA Residuals'}.
#' @param series_name Character scalar; name of the time series used in \code{seas_obj}.
#' @param this_x_label Label for X axis. Default is \code{"Time"}.
#' @param this_y_label Label for Y axis. Default is \code{series_name}. if specified.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is grid lines plotted.
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param draw_recess Logical scalar; indicates if certain plots will have shaded 
#'        areas for NBER recession dates. Default is recession shading not plotted.
#' @param recess_color Character string; color used for shading of recession region. 
#'        Default is \code{'lightgrey'}.
#' @param recess_sub Logical scalar; indicates if x-axis label for recession is 
#'        produced for this plot. Default is x-axis label is produced
#' @param use_ratio Logical scalar; indicates if plots of seasonal factors, irregular, 
#'        and residuals are done as ratio plots. 
#'        Default has these plots as time series line plots.
#' @param add_line Logical scalar; add solid line for assumed mean. 
#'        Default is \code{TRUE}. 
#' @param line_color Character string; color used for residuals. 
#'        Default is \code{"green"}.
#' @return Generates a \code{ggplot} object of a plot of the regARIMA residuals with 
#'         diagnostic information in the sub-headers.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' air_seas <- seasonal::seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)")
#' plot_resid_air <-
#'      plot_resid(air_seas, main_title = "ARIMA Residuals for Airline Passengers", 
#'                 use_ratio = TRUE, line_color="darkblue")
#' @import ggfortify
#' @import utils
#' @export
plot_resid <- 
    function(seas_obj = NULL, 
             main_title = "ARIMA Residuals", 
             series_name = NULL, 
             this_x_label = "Time", 
             this_y_label = NULL, 
             do_grid = TRUE, 
			 do_background = FALSE,
             draw_recess = FALSE, 
             recess_color = NULL, 
             recess_sub = TRUE, 
             use_ratio = FALSE, 
             add_line = TRUE, 
             line_color = "steelblue") {
    # Author: Brian C. Monsell (OEUS) Version 2.5, 8/26/2024

    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
        }
    }
    
    # Extract regARIMA residuals
    resid <- seasonal::series(seas_obj, "rsd")

    # Generate main plot title
    if (!is.null(series_name)) {
        if (!is.null(main_title)) {
            main_title <- paste(main_title, " for ", series_name)
        }
    }
	
	if (is.null(this_y_label)) {
	    if (!is.null(series_name)) {
		    this_y_label <- series_name
		} else {
		    this_y_label <- "Residuals"
		}
	}
	
    # Generate mean 
	this_add <- sum(resid < 0.0) > 0
	this_ratio_mean <- 1.0
	if (this_add) {
	    this_ratio_mean <- 0.0
	}

    # Generate ratio plot of residuals
    if (use_ratio) {
        this_plot <-
		    plot_ratio(resid, main_title = main_title, this_y_label = this_y_label,
			           ratio_color = line_color, ratio_mean = this_ratio_mean, 
					   do_grid = do_grid, draw_recess = draw_recess, add_line = add_line)
    } else {
        # Generate line plot of residuals
        this_plot <-
		    plot_series(resid, main_title = main_title, this_y_label = this_y_label, 
                        do_grid = do_grid, draw_recess = draw_recess, 
                        line_color = line_color)
						
        # Generate mean line
        if (add_line) {
		    this_plot <- 
	            this_plot + ggplot2::geom_hline(yintercept = this_ratio_mean)
		}

	}

	# remove grid lines if \code{do_grid = FALSE}
	if (!do_grid) {
	    this_plot <- this_plot + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
	}

	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		this_plot <- this_plot + ggplot2::theme_bw()
    }

    # Generate recession region shading
	if (draw_recess) {
	    if (is.null(recess_color)) {
	        this_plot <- add_recession_shade(this_plot)
		} else {
	        this_plot <- add_recession_shade(this_plot, shade_color = recess_color)		    
		}
		if (recess_sub) {
		    this_x_label <- 
				stringr::str_to_title(paste0("NBER Recessions in ", recess_color))
		}
	}
	
    # Generate x-axis label
	this_plot <- this_plot + ggplot2::xlab(this_x_label)

    # get residual diagnostics
    thisA <- tryCatch(seasonal::udg(seas_obj, "a"), error = function(e) {
        print("Geary's a not found")
        NULL
    })

    thisKurt <- tryCatch(seasonal::udg(seas_obj, "kurtosis"), error = function(e) {
        print("kurtosis not found")
        NULL
    })

    thisSkew <- tryCatch(seasonal::udg(seas_obj, "skewness"), error = function(e) {
        print("skewness not found")
        NULL
    })

    # generate subheader for diagnostics
    sub_title <- NULL
    
	if (!is.null(thisA)) {
        if (length(thisA) > 1) {
            sub_title <- paste("Geary's a = ", sprintf("%4.1f", as.numeric(thisA[1])), " (", thisA[2],
                ")", sep = "")
        } else {
            sub_title <- paste("Geary's a = ", sprintf("%4.1f", thisA), sep = "")
        }
    }

    if (!is.null(thisKurt)) {
        if (length(thisKurt) > 1) {
            sub2 <- paste("Excess Kurtosis = ", sprintf("%4.1f", as.numeric(thisKurt[1])), " (", thisKurt[2],
                ")", sep = "")
        } else {
            sub2 <- paste("Excess Kurtosis = ", sprintf("%4.1f", thisKurt), sep = "")
        }
		if (is.null(sub_title)) {
		    sub_title <- sub2
		} else {
		    sub_title <- paste0(sub_title, ", ", sub2)
		}
    }

    if (!is.null(thisSkew)) {
        if (length(thisSkew) > 1) {
            sub3 <- paste("Skewness = ", sprintf("%4.1f", as.numeric(thisSkew[1])), " (", thisSkew[2],
                ")", sep = "")
        } else {
            sub3 <- paste("Skewness = ", sprintf("%4.1f", thisSkew), sep = "")
        }
 		if (is.null(sub_title)) {
		    sub_title <- sub3
		} else {
		    sub_title <- paste0(sub_title, ", ", sub3)
		}
    }

    # add subheader to plot
    this_plot <- 
	    this_plot + ggplot2::labs(subtitle = sub_title)

    # return ggplot object
    return(this_plot)	

}
