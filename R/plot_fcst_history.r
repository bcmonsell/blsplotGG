#' Generate forecast history plot
#'
#' Generate forecast history plot, which compares the sum of squared forecast errors for two models. 
#' This follows the theory established in \insertCite{Findley2005;textual}{blsplotGG}.
#' For more information on history analysis, \insertCite{@see Chapters 6.3 and 7.8 of @Census2025;textual}{blsplotGG}.
#'
#' Version 4.2, 2/10/2026
#'
#' @param seas_mdl1 \code{seas} object generated from a call of \code{seas} 
#'        on a single time series for the first model 
#'        This is a required entry.
#' @param seas_mdl2 \code{seas} object generated from a call of \code{seas} 
#'        on a single time series for the second model
#'        This is a required entry.
#' @param main_title Character string; main title of plot.  
#'        Default is \code{'Differences in the Sum of Squared Forecast Errors'}.
#' @param name_mdl1 Character string; Description of first model for use in the subtitle. 
#'        Default is \code{'Model 1'}.
#' @param name_mdl2 Character string; Description of second model for use in the subtitle. 
#'        Default is \code{'Model 2'}.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background.
#' @param this_x_label Label for X-axis.  Default is \code{"Time"}
#' @param this_y_label Label for Y-axis.  Default is \code{" "}
#' @param this_palette Character array of length 2; color used for each forecast lag. 
#'        Default is \code{c("steelblue", "darkgreen")}. 
#' @param this_legend_title Title for legend.  Default is \code{"Fcst History"}
#' @param this_legend_title_size integer scalar; Size of the legend title.
#'        Default is \code{12}.
#' @param this_legend_text_size integer scalar; Size of the legend title.
#'        Default is \code{10}.
#' @param this_legend_position character String; The position of the legend, which can be one of
#'        \code{top}, \code{bottom}, \code{left}, \code{right}.
#'        Default is \code{right}.
#' @param this_legend_space integer scalar; Space between legend and plot in points.
#'        Default is \code{10}.
#' @return A \code{ggplot} object that produces a forecast history plot. 
#'         If series not specified, print out error message and return NULL.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' air_seas_mdl <- 
#'     seasonal::seas(AirPassengers, x11="", slidingspans = "", transform.function = "log",
#'                    arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL, outlier = NULL,
#'                    forecast.maxlead = 36, check.print = c( 'pacf', 'pacfplot' ),
#'                    history.fstep = c(1, 12), history.estimates = 'fcst', 
#'                    history.save = 'fcsterrors')
#' air_seas_mdl2 <- 
#'     seasonal::seas(AirPassengers, x11="", slidingspans = "", transform.function = "log",
#'                    arima.model = "(0 1 1)(0 1 1)", regression.variables = c("td"),
#'                    forecast.maxlead = 36, check.print = c( 'pacf', 'pacfplot' ),
#'                    history.fstep = c(1, 12), history.estimates = 'fcst', 
#'                    history.save = 'fcsterrors')
#' plot_fcst_history_air <-
#'     plot_fcst_history(air_seas_mdl, air_seas_mdl2, 
#'          main_title = 'Differences in the Sum of Squared Forecast Errors for Airline Passengers',
#'          name_mdl1 = 'Airline model', name_mdl2 = 'Airline model + regressors')
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom Rdpack reprompt
#' @export
plot_fcst_history <- 
    function(seas_mdl1 = NULL, seas_mdl2 = NULL, 
             main_title = "Differences in the Sum of Squared Forecast Errors", 
             name_mdl1 = "Model 1", name_mdl2 = "Model 2", 
             do_grid = FALSE, 
             do_background = FALSE, 
             this_x_label = "Time", 
             this_y_label = " ",
             this_palette = c("steelblue", "darkgreen"),
             this_legend_title = "Fcst History",
			 this_legend_title_size = 12,
			 this_legend_text_size = 10,
			 this_legend_position = "right",
			 this_legend_space = 10) {
    # Author: Brian C. Monsell (OEUS) Version 4.2, 2/10/2026
    
    # check if seas objects for both models are specified if not, return NULL
    if (is.null(seas_mdl1)) {
        cat("must specify seas objects for both models")
        return(NULL)
    } else {
        # check if a seas object is specified for \code{seas_mdl1}
        if (!inherits(seas_mdl1, "seas")) {
            cat("First argument must be a seas object")
			return(NULL)
        }
    }

    if (is.null(seas_mdl2)) {
        cat("must specify seas objects for both models")
        return(NULL)
    } else {
        # check if a seas object is specified for \code{seas_mdl2}
        if (!inherits(seas_mdl2, "seas")) {
            cat("Second argument must be a seas object")
			return(NULL)
        }
    }

	# check legend options 
	position_all <- c("top", "bottom", "left", "right")
	if(sum(match(position_all, this_legend_position), na.rm = TRUE) == 0) {
        cat(paste("Legend position choice not valid:", this_legend_position))
        return(NULL)
	}
	
	if(this_legend_text_size <= 0) {
        cat(paste("Legend text size not valid (must be greater than zero):", 
		           this_legend_text_size))
        return(NULL)
	}
	
	if(this_legend_title_size <= 0) {
        cat(paste("Legend title text size not valid (must be greater than zero):", 
		           this_legend_title_size))
        return(NULL)
	}
	
    # extract udg information from seas objects
    udg_mdl1 <- seasonal::udg(seas_mdl1)
    udg_mdl2 <- seasonal::udg(seas_mdl2)
    
    # extract fstep information from udg lists
    fstep_mdl1 <- udg_mdl1$rvfcstlag
    fstep_mdl2 <- udg_mdl2$rvfcstlag
    
    # extract squared forecast errors from seas objects
    fcst_error_mdl1 <- seasonal::series(seas_mdl1, "history.fcsterrors")
    fcst_error_mdl2 <- seasonal::series(seas_mdl2, "history.fcsterrors")
	
    # extract frequency
    if (udg_mdl1$freq != udg_mdl2$freq) {
        cat("series frequency for both runs must match.")
        return(NULL)
    }
    this_freq <- udg_mdl1$freq
    
    # set and test number of rows for time series tested
    if (is.matrix(fcst_error_mdl1)) {
        # check if the number of forecast leads being tested are the same for the two models if number of
        # leads unequal, return NULL
        if (ncol(fcst_error_mdl1) != ncol(fcst_error_mdl2)) {
            cat("number of forecast lags tested must be equal for the two models")
            return(NULL)
        }
        
        # check if the forecast leads being tested are the same for the two models if forecast leads
        # tested not the same, return NULL
        nstep <- ncol(fcst_error_mdl1)
        this_nrows <- nrow(fcst_error_mdl1)
    } else {
        if (is.matrix(fcst_error_mdl1)) {
            cat("number of forecast lags tested must be equal")
            return(NULL)
        }
        nstep <- 1
        this_nrows <- length(fcst_error_mdl1)
    }
    nsum <- sum(fstep_mdl1 == fstep_mdl2)
    if (nstep > nsum) {
        cat("forecast lags tested for the two models need to be the same")
        return(NULL)
    }
	
	if (nstep > 2) {
	    step_index <- fstep_mdl1 %in% c(1, this_freq)
		fcst_error_mdl1 <- fcst_error_mdl1[,step_index]
		fcst_error_mdl2 <- fcst_error_mdl2[,step_index]
		nstep <- sum(step_index)
        fstep_mdl1 <- fstep_mdl1[step_index]
        fstep_mdl2 <- fstep_mdl2[step_index]
		cat(paste0("Warning: forecast steps plotted restricted to ", 
		    paste0(fstep_mdl1, collapse = " and ")))
	}
    
   
    # initialize list for difference in squared forecast error, column names
    fcst_err_list <- list()
    this_key <- paste("lead ", fstep_mdl1, sep = " ")
    
    this_start_history <- start(fcst_error_mdl1)
	nsum <- sum(this_start_history == start(fcst_error_mdl2))
	if (nsum < 2) {
        cat("the start of the forecast history analysis need to be the same")
        return(NULL)
	}
	
    # generate sub title
	sub_title <- ""
    if (!is.null(name_mdl1)) {
        if (!is.null(name_mdl2)) {
            sub_title <- paste0(name_mdl1, " vs ", name_mdl2)
        }
    }
	if (nstep == 1) {
	    if (length(sub_title) > 0) {
	        sub_title <- paste0(sub_title, ", Lead ", fstep_mdl1)
		} else {
	        sub_title <- paste0("Lead ", fstep_mdl1)
		}
	}
        
    # generate cumulative difference in squared forecast error, store as a time series object in the
    # list
    if (nstep > 1) {
	    fcst_error_mdl1[,2][fcst_error_mdl1[,2] == 0] <- NA
	    fcst_error_mdl2[,2][fcst_error_mdl2[,2] == 0] <- NA
        this_date <- tsbox::ts_df(fcst_error_mdl1[,1])$time
        fcst_history_df <- 
		    data.frame(date  = this_date,
		               step1 = as.vector(fcst_error_mdl1[,1] - fcst_error_mdl2[,1]),
		               step2 = as.vector(fcst_error_mdl1[,2] - fcst_error_mdl2[,2])) %>%
	    dplyr::select(.data$date, .data$step1, .data$step2) %>%
        tidyr::gather(key = "fcsthist", value = "value", -date)
		
		p_fcst_hist <- 
            ggplot2::ggplot(fcst_history_df) + 
            ggplot2::geom_line(ggplot2::aes(x = .data$date, 
                                            y = .data$value, 
                                        color = .data$fcsthist)) + 
            ggplot2::scale_color_manual(labels = c("Lead 1", paste0("Lead ", this_freq)), 
                                        values = c("step1"    = this_palette[1],
                                                   "step2"    = this_palette[2]),
                                        breaks = c("step1", "step2")) +
            ggplot2::labs(title = stringr::str_to_title(main_title),
                       subtitle = stringr::str_to_title(sub_title),
                              x = stringr::str_to_title(this_x_label),
                              y = stringr::str_to_title(this_y_label)) +
            ggplot2::guides(color = ggplot2::guide_legend(this_legend_title)) + 
		    ggplot2::geom_hline(yintercept = 0, 
                                linetype = "solid", 
                                color = "lightgrey")


    } else {
        this_date <- tsbox::ts_df(fcst_error_mdl1)$time
        fcst_history_df <- 
		    data.frame(date  = this_date,
		               value = as.vector(fcst_error_mdl1 - fcst_error_mdl2))
	    p_fcst_hist <- 
            ggplot2::ggplot(fcst_history_df) + 
            ggplot2::geom_line(ggplot2::aes(x = .data$date, 
                                            y = .data$value),
											color = this_palette[1]) + 
            ggplot2::labs(title = stringr::str_to_title(main_title),
                          subtitle = stringr::str_to_title(sub_title),
						  x = stringr::str_to_title(this_x_label),
                          y = stringr::str_to_title(this_y_label)) + 
		    ggplot2::theme(legend.position="none")+ 
		    ggplot2::geom_hline(yintercept = 0, 
                            linetype = "solid", 
                            color = "lightgrey")
    }
       
	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_fcst_hist <- p_fcst_hist + ggplot2::theme_bw()
    }

	# remove grid lines if \code{do_grid = FALSE}
	if (!do_grid) {
	    p_fcst_hist <- p_fcst_hist + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
	}
	
	p_fcst_hist <- p_fcst_hist +
		ggplot2::theme(legend.text = ggplot2::element_text(size = this_legend_text_size),
		               legend.title = ggplot2::element_text(size = this_legend_title_size),
					   legend.position = this_legend_position,
					   legend.box.spacing = ggplot2::unit(this_legend_space, "pt"))

    
	return(p_fcst_hist)
    
}
