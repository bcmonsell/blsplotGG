#' Revisions History Plot for Seasonal Adjustments
#'
#' Generates a ggplot2 object of estimates from a revisions history of a seasonal 
#' adjustment for a given series.
#'
#' Version 1.8, 9/5/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series
#'        This is a required entry.
#' @param add_ori Logical scalar; add the original series to the plot. Default is \code{TRUE}. 
#' @param main_title Character string. Title for the plot. 
#'        Default is \code{'Seasonal Adjustment History Graph'}.
#' @param sub_title Subtitle for the plot. Default is \code{NULL}.
#' @param this_x_label Label for X axis. Default is \code{"Time"}.
#' @param this_y_label Label for Y axis. Default is \code{NULL}.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param line_color Vector with colors used for lines in history plot. 
#'        Should be of length 4 (if \code{add_ori = FALSE}) or 5 (if \code{add_ori = TRUE})
#'        Default is \code{NULL}, which indicates that the palette specified 
#'        in \code{this_palette} is used to generate colors for this plot.
#' @param this_palette Color used for lines in plot. Default is \code{"Dark2"}
#' @return A \code{ggplot} object that generates a history plot of the seasonal adjustment.
#'         The seas object in the \code{seas_obj} argument must contain output for a
#'         revisions history analysis for seasonal adjustments with \code{sadjlags}
#'         set to 1 and 12 (for monthly series) or 4 (for quarterly series).
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' shoes_seas <-
#'     seasonal::seas(shoes2008, x11.save = "d13",
#'        x11 = "", transform.function = "log",
#'        check.print = c("none", "+acf", "+acfplot", "+normalitytest"),
#'        regression.aictest = c("td", "easter"),
#'		  regression.save = c("td", "hol"),
#'        outlier.types = "all",
#'        arima.model = "(0 1 1)(0 1 1)",
#'        forecast.maxlead = 60,
#' 	   	  history.estimates = c("sadj", "sadjchng", "trend"),
#' 	      history.sadjlags = c(1,12),
#' 	      history.print = "all",
#' 	      history.save = c("sae", "sar"))
#' p_shoes_sa_history <- 
#'     plot_sa_history(shoes_seas, add_ori = FALSE, 
#'         main_title = "SA History Graph, Lag 1 and 12",
#'         sub_title = "US Retail Sales of Shoes")
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
plot_sa_history <-
    function(seas_obj = NULL, 
	         add_ori = TRUE,               
             main_title = "Seasonal Adjustment History Graph",
			 sub_title = NULL, 
			 this_x_label = "Time", 
             this_y_label = NULL, 
			 do_grid = FALSE, 
			 do_background = FALSE, 
	         line_color = NULL, 
			 this_palette = "Dark2") {
	# Author: Brian C. Monsell (OEUS) Version 1.8, 9/5/2024

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
	
	this_udg  <- seasonal::udg(seas_obj)
	this_freq <- this_udg$freq
	
	this_sa_history <- seasonal::series(seas_obj, "sae")
	start_history <- start(this_sa_history)
	end_history   <- end(this_sa_history)
	
	this_sa_lags <- as.integer(seasonal::udg(seas_obj, "nsalags"))
	n_sa_lags    <- as.integer(seasonal::udg(seas_obj, "nsalag"))
	
	sa_lag1_index  <- colnames(this_sa_history) == "Conc.01._SA"
	if (sum(sa_lag1_index) == 0) {
        cat("must generate lag 1 concurrent estimates")
	    return(NULL)
	} else {
		sa_history_lag1 <- this_sa_history[, sa_lag1_index]
	}
	
	key_seasonal  <- paste0("Conc.", this_freq, "._SA")
	sa_lagS_index <- colnames(this_sa_history) == key_seasonal
	if (sum(sa_lagS_index) == 0) {
        cat(paste0("must generate lag ", this_freq, 
		           " concurrent estimates"))
	    return(NULL)
	} else {
		sa_history_lagS <- this_sa_history[, sa_lagS_index]
		sa_history_lagS[sa_history_lagS == 0] <- NA
	}

	n_colors <- 4
	if (add_ori) {
		this_ori <- 
			window(seas_obj$x, start = start_history, end = end_history)
		n_colors <- n_colors + 1
	}
	if (is.null(line_color)) {
		line_color <- RColorBrewer::brewer.pal(n_colors, this_palette)
	} else {
		if (length(line_color) < n_colors) {
			cat(paste0("need at least ", n_colors, 
		               " colors in the line_color argument."))
			return(NULL)		    
		}
	}
	
	if (add_ori) {
		this_sa_history_matrix <-
			cbind(this_ori, this_sa_history[,1:2], sa_history_lag1, 
			      sa_history_lagS)
		colnames(this_sa_history_matrix) <- 
			c("ori", "initial_sa", "final_sa", "lag_1", "lag_S")
	} else {
		this_sa_history_matrix <-
			cbind(this_sa_history[,1:2], sa_history_lag1, 
			      sa_history_lagS)
		colnames(this_sa_history_matrix) <- 
			c("initial_sa", "final_sa", "lag_1", "lag_S")
	}
		  		  
	this_sa_history_df <- 
		as.data.frame(this_sa_history_matrix)
	this_sa_history_df$date <- tsbox::ts_df(sa_history_lagS)$time
	
	if (add_ori) {
		p_history <- 
			ggplot2::ggplot(this_sa_history_df, ggplot2::aes(x = .data$date)) + 
			ggplot2::geom_line(ggplot2::aes(y = .data$ori, colour = "ori")) +
			ggplot2::geom_line(ggplot2::aes(y = .data$initial_sa, 
				colour = "initial_sa")) +
			ggplot2::geom_line(ggplot2::aes(y = .data$final_sa, 
				colour = "final_sa")) +
			ggplot2::geom_point(ggplot2::aes(y = .data$lag_1, 
				colour = "lag_1")) +
			ggplot2::geom_point(ggplot2::aes(y = .data$lag_S, 
				colour = "lag_S")) + 
			ggplot2::scale_colour_manual("", 
				breaks = colnames(this_sa_history_df)[1:n_colors],
				values = line_color) +	
			ggplot2::labs(title = main_title,
                  subtitle = sub_title,
                  x = this_x_label,
                  y = this_y_label)
	} else {
		p_history <- 
			ggplot2::ggplot(this_sa_history_df, ggplot2::aes(x = .data$date)) + 
			ggplot2::geom_line(ggplot2::aes(y = .data$initial_sa, 
				colour = "initial_sa")) +
			ggplot2::geom_line(ggplot2::aes(y = .data$final_sa, 
				colour = "final_sa")) +
			ggplot2::geom_point(ggplot2::aes(y = .data$lag_1, 
				colour = "lag_1")) +
			ggplot2::geom_point(ggplot2::aes(y = .data$lag_S, 
				colour = "lag_S")) + 
			ggplot2::scale_colour_manual("", 
				breaks = colnames(this_sa_history_df)[1:n_colors],
				values = line_color) +	
			ggplot2::labs(title = main_title,
                  subtitle = sub_title,
                  x = this_x_label,
                  y = this_y_label)
	}

	# remove grid lines if \code{do_grid = FALSE}
	if (!do_grid) {
	    p_history <- p_history + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
	}
	
	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_history <- p_history + ggplot2::theme_bw()
    }

	return(p_history)
}