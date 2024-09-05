#' Ratio facet plot
#'
#' Generates a high-definition plot for a number of X-13 factors around 
#' a reference line other than zero.
#'
#' Version 2.5, 8/28/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} 
#'        on a single time series
#'        This is a required entry.
#' @param ratio_tables Array of tables for which you want to generate a 
#'        high definition plot. Possible entries are \code{"sf"} (seasonal factor),
#'        \code{"td"} (trading day factor), \code{"hol"} (holiday factors), and 
#'        \code{"irr"} (irregular). 
#' @param main_title Title for the plot. 
#'        Default is character string \code{'Ratio Facet Plot'}.
#' @param sub_title Subtitle for the plot. Default is \code{NULL}.
#' @param this_x_label Label for X axis. Default is \code{"Time"}.
#' @param this_y_label Label for Y axis. Default is \code{"Ratio"}.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param add_line Logical scalar; add solid line for assumed mean. 
#'        Default is \code{TRUE}. 
#' @param ratio_mean Assumed mean value for the ratio.  Default is \code{1.0}
#' @param ratio_color Color used for lines in ratio plot.  
#'        Default is \code{"steelblue"}.
#' @return A \code{ggplot} object that generates a ratio facet plot for the 
#'         factors provided in the \code{ratio_tables} argument.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' shoes_x11_seas <- 
#'    seasonal::seas(shoes2008, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", forecast.maxlead = 20,
#'                   regression.aictest = c("td", "easter"),
#'		             regression.save = c("td", "hol"),
#'                   x11.seasonalma = "s3x5", check.print = c( 'pacf', 'pacfplot' ))
#' shoes_factor_ratio_one   <- 
#'    plot_ratio_facet(shoes_x11_seas, c("sf", "irr"), 
#'                     main_title = "US Retail Shoe Sales",
#'                     sub_title = "X-11 Seasonal (3x5) and Irregular Factors")
#' shoes_factor_ratio_two   <- 
#'    plot_ratio_facet(shoes_x11_seas, c("td", "hol", "irr"), 
#'                     main_title = "US Retail Shoe Sales",
#'                     sub_title = "Trading Day, Easter, and Irregular Factors")
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_ratio_facet <- 
    function(seas_obj = NULL, 
	         ratio_tables = NULL, 
             main_title = "Ratio Facet Plot", 
			 sub_title = NULL, 
             this_x_label = "Time", 
			 this_y_label = "Ratio", 
			 do_grid = FALSE, 
	         do_background = FALSE, 
	         add_line = TRUE, 
			 ratio_mean = 1.0, 
			 ratio_color = "steelblue") {
    # Author: Brian C. Monsell (OEUS) Version 2.5, 8/28/2024

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
	
    # check if a value is specified for \code{ratio_tables}
    if (is.null(ratio_tables)) {
        cat("must specify tables to plot")
 	    return(NULL)
    }
	
	this_time <- NULL
	this_len <- NULL
	if ("sf" %in% ratio_tables) {
	    if (length(seas_obj$spc$x11) > 0) {
	        this_sf <- seasonal::series(seas_obj, "d10")
		} else {
	        this_sf <- seasonal::series(seas_obj, "s10")
		}
		if (is.null(this_time)) {
		    this_time <- tsbox::ts_df(this_sf)$time
		}
		if (is.null(this_len)) {
		    this_len <- length(this_sf)
		}
	} else {
	    this_sf <- NULL
	}

	if ("td" %in% ratio_tables) {
	    this_td <- seasonal::series(seas_obj, "td")
		if (is.null(this_time)) {
		    this_time <- tsbox::ts_df(this_td)$time
		}
		if (is.null(this_len)) {
		    this_len <- length(this_td)
		}
	} else {
	    this_td <- NULL
	}

	if ("hol" %in% ratio_tables) {
	    this_hol <- seasonal::series(seas_obj, "hol")
		if (is.null(this_time)) {
		    this_time <- tsbox::ts_df(this_hol)$time
		}
		if (is.null(this_len)) {
		    this_len <- length(this_hol)
		}
	} else {
	    this_hol <- NULL
	}

	if ("irr" %in% ratio_tables) {
	    if (length(seas_obj$spc$x11) > 0) {
			this_irr <- seasonal::series(seas_obj, "d13")
		} else {
			this_irr <- seasonal::series(seas_obj, "s13")
		}
		if (is.null(this_time)) {
		    this_time <- tsbox::ts_df(this_irr)$time
		}
		if (is.null(this_len)) {
		    this_len <- length(this_irr)
		}
	} else {
	    this_irr <- NULL
	}

    if (is.null(this_len)) {
        cat("must specify either sf, td, irr, or hol in the ratio_tables argument")
	    return(NULL)
	}
	
	if (is.null(this_sf)) {
	    this_sf <- rep(NA, this_len)
	}	
	if (is.null(this_td)) {
	    this_td <- rep(NA, this_len)
	}	
	if (is.null(this_hol)) {
	    this_hol <- rep(NA, this_len)
	}	
	if (is.null(this_irr)) {
	    this_irr <- rep(NA, this_len)
	}
	
    this_factor_df <- 
        data.frame(
            date = this_time,
	        sf   = as.double(this_sf),
	        irr  = as.double(this_irr),
	        hol  = as.double(this_hol),
	        td   = as.double(this_td)) %>%
		dplyr::select(.data$date, .data$sf, .data$irr, .data$hol, .data$td) %>%
		tidyr::gather(key = "series", value = "value", -date) %>% 
        tidyr::drop_na()

    p_ratio_facet <- 
	    ggplot2::ggplot(this_factor_df) + 
        ggplot2::geom_segment(mapping=ggplot2::aes(x=.data$date, 
                                             y=ratio_mean, 
                                             xend=.data$date, 
                                             yend=.data$value), 
                                             color = ratio_color) +
	    ggplot2::facet_grid(series ~ .) + 
            ggplot2::labs(title = main_title,
                          subtitle = sub_title,
						  x = this_x_label,
                          y = this_y_label)
		
	if (add_line) {
       p_ratio_facet <- p_ratio_facet +
            ggplot2::geom_hline(yintercept = ratio_mean, 
	                            linetype = "solid", 
	                            color = ratio_color)
	}
	
	# remove grid lines if \code{do_grid = FALSE}
	if (!do_grid) {
	    p_ratio_facet <- p_ratio_facet + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
		                   panel.grid.minor = ggplot2::element_blank())
	}
	
	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_ratio_facet <- p_ratio_facet + ggplot2::theme_bw()
    }

	return(p_ratio_facet)
	
}