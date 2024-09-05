#' Forecast plot for two models
#'
#' Generates plot that compares regARIMA forecasts  for two models of the same series
#'
#' Version 1.6, 8/28/2024
#'
#' @param seas_obj_one \code{seas} object generated from a call of \code{seas} 
#'        on a single time series
#'        This is a required entry.
#' @param seas_obj_two \code{seas} object generated from a call of \code{seas} 
#'        on the same time series, but a different regARIMA model.
#'        This is a required entry.
#' @param main_title Character string; main title of plot.  
#'        Default is \code{'ARIMA Residuals'}.
#' @param sub_title Subtitle for the plot. 
#'        Default is to generate the subtitle. 
#' @param name_mdl1 Character string; Description of first model for use in the subtitle. 
#'        Default is \code{'Model 1'}.
#' @param name_mdl2 Character string; Description of second model for use in the subtitle. 
#'        Default is \code{'Model 2'}.
#' @param this_x_label Label for X-axis.  Default is \code{"Time"}
#' @param this_y_label Label for Y-axis.  Default is \code{" "}
#' @param length_ori Integer scalar; number of years of the original series 
#'        to show with forecasts. #'        Default is 2 years. 
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param this_palette Array of character strings; color used for original series, 
#'        forecast, and upper and lower forecast bounds. 
#'        Default is \code{c("darkgrey", "steelblue", "darkgreen")}. 
#' @param this_guide_legend Title for legend.  Default is \code{"Forecast"}
#' @return A \code{ggplot} object of the regARIMA forecasts for two models of the same series.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' shoes_seas_one <- 
#'     seasonal::seas(shoes2008, x11="", slidingspans = "", transform.function = "log",
#'                    arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL, 
#'                    outlier = NULL, forecast.maxlead = 60, forecast.save = "fct",
#'                    check.print = c( 'pacf', 'pacfplot' ), series.save = "a1")
#' shoes_seas_two <- 
#'     seasonal::seas(shoes2008, x11="", slidingspans = "", transform.function = "log",
#'                    arima.model = "(0 1 1)(0 1 1)", regression.variables = c("td"),
#'                    forecast.maxlead = 60, forecast.save = "fct", 
#'                    check.print = c( 'pacf', 'pacfplot' ), series.save = "a1")
#' shoes_fcst_two_p <- 
#'     plot_fcst_two(shoes_seas_one, shoes_seas_two,
#'          main_title = "Forecast Comparison Plot", 
#'          name_mdl1 = 'Airline', name_mdl2 = 'Airline + reg',
#'          do_grid = TRUE)
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_fcst_two <- 
    function(seas_obj_one = NULL, 
	         seas_obj_two = NULL, 
	         main_title = "ARIMA forecasts", 
			 sub_title = NULL, 
             name_mdl1 = "Model 1", 
			 name_mdl2 = "Model 2", 
             this_x_label = "Time", 
			 this_y_label = " ", 
			 length_ori = 2, 
			 do_grid = FALSE, 
             do_background = FALSE, 
             this_palette = c("darkgrey", "steelblue", "darkgreen"), 
			 this_guide_legend = "Forecast") {
    # Author: Brian C. Monsell (OEUS) Version 1.6, 8/28/2024
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj_one)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj_one}
        if (!inherits(seas_obj_one, "seas")) {
            stop("First argument must be a seas object")
        }
    }
	
    if (is.null(seas_obj_two)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj_two}
        if (!inherits(seas_obj_two, "seas")) {
            stop("Second argument must be a seas object")
        }
    }
	
   if (is.null(main_title)) {
       sub_title <- NULL
    }

    # extract forecasts, original series
    fcst_one <- seasonal::series(seas_obj_one, "fct")
    fcst_two <- seasonal::series(seas_obj_two, "fct")
    a1 <- seasonal::series(seas_obj_one, "a1")
    a1_two <- seasonal::series(seas_obj_two, "a1")
	
	a1_equal <- tryCatch(a1 == a1_two, error = function(e) { NULL })
	if (is.null(a1_equal)) {
        stop("Series not the same for both models")
	} else {
		if (!(sum(a1_equal) == length(a1_equal))) {
			stop("Series not the same for both models")
		}
	}
   
    # get date for end of series
    end_a1 <- end(a1)
    
    # get series to be pottted without forecasts
    srs <- window(a1, start = c(end_a1[1] - length_ori, end_a1[2]))
    # get series to be pottted with forecasts
    ext <- ts(c(srs, fcst_one[, 1]), start = start(srs), frequency = frequency(srs))
	
	length_fcst_one <- length(fcst_one[, 1])
	length_fcst_two <- length(fcst_two[, 1])
	length_srs  <- length(srs)
	
	f1 <- fcst_one[, 1]
	f2 <- fcst_two[, 1]
	if (length_fcst_one > length_fcst_two) {
		f2 <- c(f2, rep(NA, length_fcst_one - length_fcst_two))
	} else {
		if (length_fcst_one < length_fcst_two) {
			f1 <- c(f1, rep(NA, length_fcst_two - length_fcst_one))
		}
	}
	length_fcst <- max(length_fcst_one, length_fcst_two)
		
    # generate data frame for plot
	fcst_df <- 
	    data.frame(date  = tsbox::ts_df(ext)$time,
		           ori   = c(srs, rep(NA, length_fcst)),
				   fcst1 = c(rep(NA, length_srs), f1),
				   fcst2  = c(rep(NA, length_srs), f2)) %>%
	    dplyr::select(.data$date, .data$ori, .data$fcst1, .data$fcst2) %>%
        tidyr::gather(key = "fcst", value = "value", -date)
    
    p_fcst_two <- 
        ggplot2::ggplot(fcst_df) + 
        ggplot2::geom_line(ggplot2::aes(x = .data$date, 
                                        y = .data$value, 
                                        color = .data$fcst)) + 
        ggplot2::scale_color_manual(labels = c("Ori", name_mdl1, name_mdl2), 
                                    values = c("ori"   = this_palette[1],
                                               "fcst1" = this_palette[2],
                                               "fcst2" = this_palette[3]),
                                    breaks=c("ori", "fcst1", "fcst2")) +
        ggplot2::labs(title = main_title,
                      subtitle = sub_title,
                      x = this_x_label,
                      y = this_y_label) +
        ggplot2::guides(color = ggplot2::guide_legend(this_guide_legend))

    
	# remove grid lines if \code{do_grid = FALSE}
	if (!do_grid) {
	    p_fcst_two <- p_fcst_two + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
	}
    
	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_fcst_two <- p_fcst_two + ggplot2::theme_bw()
    }

	return(p_fcst_two)
}
