#' History Plot of All Trend Revisions
#'
#' Generates a ggplot2 object of estimates from a revisions history of a trend 
#' component for a given series, incorporating all trend lag revisions.
#'
#' Version 1.8, 9/5/20244
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series
#'        This is a required entry.
#' @param this_trend_name Character string; name of time series. No default.
#' @param main_title Title for the plot. Default is character string \code{'Trend History Plot'}.
#' @param sub_title Subtitle for the plot. Default is \code{NULL}.
#' @param this_x_label Label for X axis. Default is \code{"Time"}.
#' @param this_y_label Label for Y axis. Default is \code{"Ratio"}.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param base_color Character scalar for plot of the initial trend. 
#'        Default is \code{"darkblue"}.
#' @param whisker_color Character scalar for color used for lines representing 
#'        lagged trend estimates in the trend history plot. 
#'        Default is \code{"darkgrey"}
#' @return A \code{ggplot} object that produces a history plot of the trend component.
#'         The \code{seas} object in the \code{seas_obj} argument must contain output for a
#'         revisions history analysis for trends with \code{trendlags}
#'         set to some values - these lags should be in sequence, such as
#'         \code{history.trendlags = 1:4}.
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
#' 	      history.trendlags = 1:4,
#' 	      history.print = "all",
#' 	      history.save = c("tre", "trr"))
#' p_shoes_sa_history <- 
#'     plot_all_trend_lags(shoes_seas, this_trend_name = "shoes", 
#'         main_title = "All Trend Revisions, Lags 1 to 4",
#'         sub_title = "US Retail Sales of Shoes")
#' @importFrom rlang .data
#' @export
#' @importFrom magrittr %>%
plot_all_trend_lags <- 
	function(seas_obj = NULL, 
		this_trend_name = NULL,
		main_title = "All Trend Revision Plot", 
		sub_title = NULL, 
		this_y_label = NULL, 
		this_x_label = "Time", 
		do_grid = FALSE,
		do_background = FALSE,
		base_color = "darkblue",
		whisker_color = "darkgrey") {
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
	this_trend_history  <- seasonal::series(seas_obj, "tre")
	this_trend_lags    <- as.integer(seasonal::udg(seas_obj, "ntrnlags"))

	trend_time <- tsbox::ts_df(this_trend_history[,1])$time

	n_trend_lags    <- length(this_trend_lags)
	n_trend_history <- length(trend_time)
  
	p_base_trend <- 
		blsplotGG::plot_series(this_trend_history[,1], 
			this_trend_name, 
			main_title = main_title,
			sub_title = sub_title,
			this_x_label = this_x_label,
			this_y_label = this_y_label,
			y_limit = range(this_trend_history[this_trend_history > 0]),
            do_grid, do_background,
			line_color = base_color)
			
	i1 <- this_trend_lags[n_trend_lags] + 1						   

	for (i in seq(i1, (n_trend_history - 1))) {
		this_time  <- trend_time[i:(i+n_trend_lags)]
		this_trend <- this_trend_history[i,1]
		for (j in 1:n_trend_lags) {
			this_trend <- c(this_trend, this_trend_history[i,2+j])
		}
   
		trend_df <- data.frame(
			date = this_time,
			trend = this_trend) %>% 
			tidyr::drop_na()
   
		p_base_trend <- p_base_trend +
			ggplot2::geom_line(data=trend_df,
				ggplot2::aes(x=.data$date, y=.data$trend), 
				color = whisker_color)
	}
	
	return(p_base_trend)
}