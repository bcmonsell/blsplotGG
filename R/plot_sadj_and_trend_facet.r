#' Plot X-13 seasonal adjustment, trend in a facet plot
#'
#' Generates a \code{ggplot} object with a time series facet plot that compares 
#' an X-13 seasonal adjustment and trend, optionally including the original series.
#'
#' Version 1.10, 12/17/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series
#'        This is a required entry.
#' @param plot_ori Logical scalar, indicates if original series is included with plot. 
#'        Default is \code{TRUE}.
#' @param main_title Character string; title for the plot. 
#'        By default, the routine will generate a title based on the 
#'        type of adjustment (X-11 and SEATS) done.
#' @param sub_title Subtitle for the plot. Optional entry.
#' @param this_x_label Label for X-axis.  Default is \code{"Time"}
#' @param this_y_label Label for Y-axis.  Default is \code{" "}
#' @param line_color Character vector of length 2 (if \code{plot_ori} is \code{FALSE})
#'        or 3 (if \code{plot_ori} is \code{TRUE}); color used for lines in the plot,
#'        in the order of seasonally adjusted series, trend, original series. 
#'        Default is generated from the \code{RColorBrewer} palette \code{"Dark2"}.
#' @param remove_legend Logical scalar; if TRUE, plot legend will be removed.
#'        Default is FALSE.
#' @return A \code{ggplot} object that generates a facet plot comparing a 
#'         seasonally adjusted series with the trend generated from the same
#'         X-13ARIMA-SEATS seasonal adjustment.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' ukgas_x11_seas   <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", forecast.maxlead = 20,
#'                   x11.seasonalma = "s3x5", x11.save = "d12",
#'                   check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_x11_sadj_and_trend_facet_p <- 
#'     plot_sadj_and_trend_facet(ukgas_x11_seas, plot_ori = TRUE,
#'                         main_title = "UK Gas",
#'                         sub_title = "X-11 Seasonal Adjustment",
#'                         line_color = c("steelblue", "forestgreen", "grey"))
#' ukgas_seats_seas <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", forecast.maxlead = 20,
#'                   seats.save = "s12",
#'                   check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_seats_sadj_and_trend_facet_p <- 
#'     plot_sadj_and_trend_facet(ukgas_seats_seas, plot_ori = FALSE, 
#'                         main_title = "UK Gas Series",
#'                         sub_title = "SEATS Seasonal Adjustment",
#'                         line_color = c("steelblue", "forestgreen"),
#'                         remove_legend = TRUE)
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
plot_sadj_and_trend_facet <- 
    function(seas_obj = NULL, 
             plot_ori = TRUE, 
             main_title = NULL, 
             sub_title = NULL, 
             this_x_label = "Time", 
             this_y_label = " ", 
             line_color = NULL,
			 remove_legend = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 1.10, 12/17/2024

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

	this_sadj  <- seasonal::final(seas_obj)
	this_trend <- seasonal::trend(seas_obj)
	if (plot_ori) {
		this_ori <- seasonal::original(seas_obj)
	}
	
	if ("seatsadj" %in% names(seasonal::udg(seas_obj))) {
		this_sa_type <- "SEATS"
	} else {
		this_sa_type <- "X-11"
	}
	
	if (is.null(main_title)) {
		main_title <- 
			paste0(this_sa_type, " Seasonal Adjutment and Trend")
	}
    
    num_color <- 3
    if (!plot_ori) {
		num_color <- 2
    }
	
    if (is.null(line_color)) {
        line_color <- RColorBrewer::brewer.pal(3, "Dark2")[1:num_color]
    } else {
        if (length(line_color) < num_color) {
	    if (length(line_color) > 1) {
                warning("Number of line colors specified less than series plotted.")
                warning("Will use colorRampPalette to increase number of colors.")
                line_color <- grDevices::colorRampPalette(line_color)(num_color)
            }
        }
    }

    if (!plot_ori) {
        this_facet_df <- 
			data.frame(date = tsbox::ts_df(this_sadj)$time, 
				sadj = as.double(this_sadj), 
				trend = as.double(this_trend)) %>%
			dplyr::select(.data$date, .data$sadj, .data$trend) %>%
			tidyr::gather(key = "comp", value = "value", -date)
		
		p_sadj_trend_facet <- 
			ggplot2::ggplot(this_facet_df, 
			                ggplot2::aes(y = .data$value, x = .data$date, 
			                             color = .data$comp)) +  
			ggplot2::geom_line() + 
			ggplot2::scale_color_manual(values = c("sadj" = line_color[1], 
				                                   "trend" = line_color[2]),
                                        labels = c("SA", "Trend")) +
			ggplot2::facet_grid(dplyr::case_when(.data$comp == "sadj" ~ "SA",
                                                 .data$comp == "trend" ~ "Trend") ~ .) + 
			ggplot2::labs(title = main_title,
				          subtitle = sub_title,
						  x = this_x_label,
						  y = this_y_label)
        
    } else {
        this_facet_df <- 
            data.frame(date = tsbox::ts_df(this_sadj)$time, 
	            ori = as.double(this_ori), 
				sadj = as.double(this_sadj), 
				trend = as.double(this_trend)) %>%
	        dplyr::select(.data$date, .data$ori, .data$sadj, .data$trend) %>%
	        tidyr::gather(key = "comp", value = "value", -date)
	        
		p_sadj_trend_facet <- 
			ggplot2::ggplot(this_facet_df, 
			                ggplot2::aes(y = .data$value, x = .data$date, 
	                                     color = .data$comp)) +  
			ggplot2::geom_line() + 
			ggplot2::scale_color_manual(values = c("ori" = line_color[3],
		                                           "sadj" = line_color[1], 
				                                   "trend" = line_color[2]),
                                        labels = c("Ori", "SA", "Trend")) +
			ggplot2::facet_grid(dplyr::case_when(.data$comp == "ori" ~ "Ori",
                                                .data$comp == "sadj" ~ "SA",
                                                .data$comp == "trend" ~ "Trend") ~ .) + 
			ggplot2::labs(title = main_title,
                          subtitle = sub_title,
                          x = this_x_label,
                          y = this_y_label)
    }
    

 	if (remove_legend) {
		p_sadj_trend_facet <- 
			p_sadj_trend_facet + ggplot2::theme(legend.position = "none")
	}
	
   return(p_sadj_trend_facet)
}