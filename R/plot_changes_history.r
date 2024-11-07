#' Revisions History Plot for Seasonal Adjustment Changes
#'
#' Generates a \code{ggplot} object of estimates from a revisions history of a seasonal 
#' adjustment changes for a given series.
#'
#' Version 1.5, 11/6/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series
#'                 This is a required entry.
#' @param plot_start Integer vector of length two. Start of the plot. 
#'        Default is \code{NULL}, which defaults to the start of the history analysis.
#' @param main_title Title for the plot. 
#'        Default is character string \code{'SA Change History Graph'}.
#' @param sub_title Subtitle for the plot. Default is \code{NULL}.
#' @param this_x_label Label for X axis. Default is \code{"Time"}.
#' @param this_y_label Label for Y axis. Default is \code{NULL}.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param line_color Vector with colors used for lines in history plot. 
#'        Should be of length 2.
#'        Default is \code{NULL}, which indicates that the palette specified 
#'        in \code{this_palette} is used to generate colors for this plot.
#' @param this_palette Color used for lines in plot. Default is \code{"Paired"}
#' @return A \code{ggplot}  object that produces a history plot of the seasonal adjustment
#'         changes. The \code{seas} object in the \code{seas_obj} argument must contain 
#'         output for a revisions history analysis for seasonal adjustment changes.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
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
#' 	      history.save = c("che", "chr"))
#' p_shoes_sa_history <- 
#'     plot_changes_history(shoes_seas, plot_start = c(2003, 1), 
#'         main_title = "Seasonal Adjustment Change History Graph",
#'         sub_title = "US Retail Sales of Shoes")
#' @importFrom rlang .data
#' @export
#' @importFrom magrittr %>%
plot_changes_history <-
    function(seas_obj = NULL, plot_start = NULL,               
             main_title = "SA Change History Graph",
			 sub_title = NULL, 
			 this_x_label = "Time", 
             this_y_label = NULL, 
			 do_grid = FALSE, 
			 do_background = FALSE, 
	         line_color = NULL, 
			 this_palette = "Paired") {
	# Author: Brian C. Monsell (OEUS) Version 1.5, 11/6/2024

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
	
	this_change_history <- seasonal::series(seas_obj, "che")
	start_history <- start(this_change_history)
	end_history   <- end(this_change_history)
	
	if (is.null(plot_start)) {
		this_change_history_matrix <- this_change_history[,1:2]
		this_date <- tsbox::ts_df(this_change_history[,1])$time
	} else {
		this_change_history_matrix <- 
			window(this_change_history[,1:2], start = plot_start)
		this_date <- 
			tsbox::ts_df(window(this_change_history[,1], start = plot_start))$time
	}
	colnames(this_change_history_matrix) <- c("initial_change", "final_change")
	
		
	this_change_history_df <- 
		as.data.frame(this_change_history_matrix)
	this_change_history_df$date <- this_date

	n_colors <- 3
	if (is.null(line_color)) {
		line_color <- RColorBrewer::brewer.pal(n_colors, this_palette)
	} else {
		if (length(line_color) < n_colors) {
			cat(paste0("need at least ", n_colors, 
		               " colors in the line_color argument."))
			return(NULL)		    
		}
	}
	
	p_history <- 
		ggplot2::ggplot(this_change_history_df, ggplot2::aes(x = .data$date)) + 
			ggplot2::geom_point(ggplot2::aes(y = .data$initial_change, 
				colour = "Initial")) +
			ggplot2::geom_point(ggplot2::aes(y = .data$final_change, 
				colour = "Final")) + 
			ggplot2::scale_colour_manual(name = "", 
				limits=c("Initial", "Final"), values = line_color) +
			ggplot2::labs(title = main_title,
                  subtitle = sub_title,
                  x = this_x_label,
                  y = this_y_label)

	this_change_segment_df <- 
		data.frame(x = this_change_history_df$date,
				   y = this_change_history_df$initial_change,
				   xend = this_change_history_df$date,
				   yend = this_change_history_df$final_change)
				   
	p_history_final <- p_history +   
		ggplot2::geom_segment(
			data = this_change_segment_df, 
			mapping = ggplot2::aes(x = .data$x, 
			                       y = .data$y, 
								   xend = .data$xend, 
								   yend = .data$yend), 
			inherit.aes = FALSE
		)

	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_history_final <- 
			p_history_final + ggplot2::theme_bw()
    }

	# remove grid lines if \code{do_grid = FALSE}
	if (!do_grid) {
	    p_history_final <- p_history_final + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
	}
	
	return(p_history_final)

}