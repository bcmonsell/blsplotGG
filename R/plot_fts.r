#' Final t-statistics for the outlier identification procedure plot
#'
#' Generates a plot of the final t-statistics for the outlier identification procedure.
#'
#' Version 2.6, 9/5/2024++
#' 
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series
#'                  This is a requited entry.
#' @param start_plot Integer vector of length 2; Starting date for plot. Default is starting date for the time series. 
#' @param main_title Character string; main title of plot.  Default is  \code{'Outlier T-Values'}.
#' @param this_y_label Character string; y-axis label for plot, if specified.
#' @param this_x_label Label for X axis. Default is \code{"Time"}.
#' @param add_identified_otl Logical scalar; indicates if outlier plots will include identified outliers. 
#'                           Default is not including identified outliers. 
#' @param color_otl Character array of length 3; color used for different outliers, with the order being \code{'ao','ls','tc'}. 
#'        Default is NULL.
#' @param this_palette Character string; default \code{RColorBrewer} palette.
#'        Deault is \code{"Dark2"}.
#' @return A \code{ggplot} object which produces a plot of the final t-statistics 
#'         from the automatic outlier identification procedure.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas_outlier <- 
#'     seasonal::seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)", 
#'                    outlier.save = "fts", outlier.types = "all")
#' plot_fts_air <- 
#'     plot_fts(air_seas_outlier, 
#'              main_title = "Outlier T-Values for Airline Passengers")
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_fts <- 
	function(seas_obj = NULL, start_plot = NULL, main_title = "Outlier T-Values", 
	         this_y_label = NULL, this_x_label = "Time", add_identified_otl = FALSE, 
	         color_otl = NULL, this_palette = "Dark2") {
    # Author: Brian C. Monsell (OEUS) Version 2.6, 9/5/2024
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
        }
    }
    
    # Initialize t-statistic data, point labels
    this_fts <- seasonal::series(seas_obj, "fts")
    point_lab <- substr(dimnames(this_fts)[[2]], 3, 4)
    
    # include identified outliers in plot
    if (add_identified_otl) {
        this_auto_outlier <- unlist(strsplit(get_auto_outlier_string(seas_obj), " "))
        n_iter <- seasonal::udg(seas_obj, "addoutlier")
        this_t_vec <- array(0, length(this_auto_outlier))
        for (i in 1:n_iter) {
            this_key <- paste("otlitr", i, "+", sep = ".")
            this_oit <- array(seasonal::udg(seas_obj, this_key))
            this_t_vec[!is.na(match(this_auto_outlier, this_oit[1]))] <- as.numeric(this_oit[4])
        }
        for (i in 1:length(this_auto_outlier)) {
            this_out <- this_auto_outlier[i]
            this_type <- substr(this_out, 1, 2)
            this_year <- as.numeric(substr(this_out, 3, 6))
            if (frequency(this_fts) == 12) {
                this_per <- get_month_index(substr(this_out, 8, 10))
                this_date <- this_year + (this_per - 1)/12
            }
            if (frequency(this_fts) == 4) {
                this_per <- as.integer(stringr::str_sub(this_out, -1, -1))
                this_date <- this_year + (this_per - 1)/4
            }
            this_col <- seq(1, length(point_lab))[!is.na(match(point_lab, this_type))]
            this_row <- seq(1, length(this_fts[, 1]))[time(this_fts) == this_date]
            this_fts[this_row, this_col] <- this_t_vec[i]
        }
    }
    
    # get subset of t-statistics if starting date is specified
    if (!is.null(start_plot)) {
        this_fts <- window(this_fts, start = start_plot)
    }
    
    # if color_otl is NULL, set colors from this_palette
    if (is.null(color_otl)) {
        color_otl <- RColorBrewer::brewer.pal(3, this_palette)[1:length(point_lab)]
    }
    
    # get absolute maximum value of t-statistics, Maximum Position of each column of the Matrix
    fts_max <- 
	    ts(apply(this_fts, 1, absmax), start = start(this_fts), 
	       frequency = frequency(this_fts))
    fts_index <- cbind(time(this_fts), max.col(abs(this_fts), "first"))
    
    # initialize color vector for outlier points
    color_vec <- array(" ", length(point_lab))
    
    # get critical values for outlier identification
    this_crit_vec <- array(0, length(point_lab))
    for (i in 1:length(point_lab)) {
        if (point_lab[i] == "AO") {
            this_crit <- seasonal::udg(seas_obj, "aocrit")
            if (length(this_crit) > 0) {
                this_crit_vec[i] <- as.numeric(this_crit[1])
            }
            color_vec[i] <- color_otl[1]
        }
        if (point_lab[i] == "LS") {
            this_crit <- seasonal::udg(seas_obj, "lscrit")
            if (length(this_crit) > 0) {
                this_crit_vec[i] <- as.numeric(this_crit[1])
            }
            color_vec[i] <- color_otl[2]
        }
        if (point_lab[i] == "TC") {
            this_crit <- seasonal::udg(seas_obj, "tccrit")
            if (length(this_crit) > 0) {
                this_crit_vec[i] <- as.numeric(this_crit[1])
            }
            color_vec[i] <- color_otl[3]
        }
    }
    
    if (length(unique(this_crit_vec)) == 1) {
        sub_title <- 
            paste("Critical Value = ", format(unique(this_crit_vec), decimals = 3), sep = "")
    } else {
        sub_title <- 
            paste("Critical Values = (", paste(format(this_crit_vec, decimals = 3), collapse = " "), ")", sep = " ")
    }
	
	this_data <- 
	    data.frame(date    = tsbox::ts_df(this_fts[,1])$time,
		       fts     = fts_max,
		       outlier = point_lab[fts_index[,2]])
				   
	p_fts <- 
		ggplot2::ggplot(this_data) + 
		ggplot2::geom_point(ggplot2::aes(x = .data$date, 
		                                 y = .data$fts, 
		                                 color = .data$outlier)) + 
		ggplot2::scale_color_manual(labels = point_lab, values = color_vec) +
		ggplot2::labs(title = main_title,
		              subtitle = sub_title,
		              x = this_x_label,
		              y = this_y_label) +
		ggplot2::guides(color = ggplot2::guide_legend("Outlier")) +
		ggplot2::ylim(range(fts_max, this_crit_vec))
    
    # lines for outlier critical values
    if (length(unique(this_crit_vec)) == 1) {
        p_fts <- p_fts + ggplot2::geom_hline(yintercept = unique(this_crit_vec), color = "grey")
    } else {
	for (i in 1:length(this_crit_vec)) {
	     p_fts <- p_fts + ggplot2::geom_hline(yintercept = this_crit_vec[i], color = color_vec[i])
	}
    }
	
	return(p_fts)
        
}
