#' Seasonal sub-plot for two sets of seasonal factors
#'
#' Generates a seasonal sub-plot from two ts objects of seasonal (or combined adjustment) factors
#'
#' Version 1.8, 9/25/2024
#'
#' @param this_sf Time series of seasonal factors from X-11 or SEATS
#' @param this_sf_two Time series of seasonal factors from X-11 or SEATS
#' @param this_sf_range Range of values you wish the plot to be plotted over. Default is range of the series.
#' @param main_title Title for the plot. Default is character string \code{'Ratio Plot'}.
#' @param sub_title Subtitle for the plot. Default is \code{NULL}.
#' @param this_x_label Label for X axis. Default is \code{NULL}.
#' @param this_y_label Label for Y axis. Default is \code{NULL}.
#' @param this_sf_label Character scalar, provides a brief description of the 
#'        first seasonal factors. Default is \code{NULL}.
#' @param this_sf_label_two Character scalar, provides a brief description of the 
#'        second seasonal factors. Default is \code{NULL}.
#' @param subplot_color Character vector of length four, setting color used for 
#'        lines in ratio plot in the order of first factor, first factor mean,
#'        second factor, second factor mean.  
#'        Default is \code{c("blue", "lightblue", "red", "pink")}.
#' @return A \code{ggplot} object that generates a ratio plot.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' ukgas_x11_seas <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", forecast.maxlead = 20,
#'                   x11.seasonalma = "s3x5", 
#'                   check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_seats_seas <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", forecast.maxlead = 20,
#'                   seats.finite = "yes", seats.save = "s10",
#'                   check.print = c( 'pacf', 'pacfplot' ))
#' ukgas_x11_sf     <- seasonal::series(ukgas_x11_seas, "d10")
#' ukgas_seats_sf   <- seasonal::series(ukgas_seats_seas, "s10")
#' p_ukgas_sf_sub   <- 
#'     seasonal_subplot_two(ukgas_x11_sf, ukgas_seats_sf,
#'                      main_title = "UK Gas Seasonal Subplots",
#'                      sub_title = "X-11 and SEATS Seasonal Factors", 
#'                      this_x_label = "Quarter",
#'                      this_sf_label = "X-11",
#'                      this_sf_label_two = "SEATS")
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
seasonal_subplot_two <- function(this_sf = NULL, this_sf_two = NULL, 
                             this_sf_range = NULL, 
                             main_title = "Seasonal Subplot", 
                             sub_title = NULL, 
                             this_x_label = NULL, 
							 this_y_label = NULL,
							 this_sf_label = NULL,
							 this_sf_label_two = NULL,
                             subplot_color = c("blue", "lightblue", "red", "pink")) {
    # Author: Brian C. Monsell (OEUS) Version 1.8, 9/25/2024

    if (is.null(this_sf)) {
        cat("Must specify two time series of seasonal factors.")
        return(NULL)
    }
    
    if (is.null(this_sf_two)) {
        cat("Must specify two time series of seasonal factors.")
        return(NULL)
    }
    
    # check if \code{this_sf} is a ts object
    if (!is.ts(this_sf)) {
        cat("must specify two ts objects")
		return(NULL)
    }
    
    # check if \code{this_sf_two} is a ts object
    if (!is.ts(this_sf_two)) {
        cat("must specify two ts objects")
		return(NULL)
    }
    
    if (is.null(this_sf_range)) {
        this_sf_range <- range(this_sf, this_sf_two)
    }
	
	tsp_comp <- sum(tsp(this_sf) == tsp(this_sf_two))
	if (tsp_comp < 3) {
		cat("Time series must have the same start, end, and frequency")
		return(NULL)
	}
	
	per = sftwo = this_key = x = NULL
    
    freq <- frequency(this_sf)
    
    this_x_var <- vector(mode = "numeric", length = 0)
    this_y_var <- vector(mode = "numeric", length = 0)
    this_y_var_two <- vector(mode = "numeric", length = 0)
    this_per   <- vector(mode = "character", length = 0)

    this_sub_sf_mean <- vector(mode = "numeric", length = 0)
    this_sub_sf_mean_two <- vector(mode = "numeric", length = 0)

    sf_period     <- cycle(this_sf)
    sf_period_two <- cycle(this_sf_two)

	color_vector  <- vector(mode = "character", length = 0)

    for (i in 1:freq) {
        this_y_var  <- c(this_y_var, this_sf[sf_period == i])
        this_y_var_two  <- c(this_y_var_two, this_sf_two[sf_period == i])
        this_sub_num <- length(this_sf[sf_period == i])
    
        s1 <- (i-1) + 0.6
        s2 <- i + 0.4
        this_x_var   <- c(this_x_var, seq(s1, s2, length.out = this_sub_num))
    
        this_sub_sf_mean <- 
          c(this_sub_sf_mean, rep(mean(this_sf[sf_period == i]), this_sub_num))
        this_sub_sf_mean_two <- 
          c(this_sub_sf_mean_two, rep(mean(this_sf_two[sf_period == i]), this_sub_num))
    
        if (freq == 12) {
           this_per <- c(this_per, month.abb[cycle(this_sf)][sf_period == i])
        } else {
           if (freq == 4) {
               this_per <- c(this_per, paste0("q", rep(i, this_sub_num)))    
           } else {
               this_per <- c(this_per, paste0("p", rep(i, this_sub_num)))    
           }
        }
    }

	this_sf_df <- 
		data.frame(sf1 = this_y_var, 
              sf2 = this_y_var_two, 
              x = this_x_var,
              per = this_per,
              mean1 = this_sub_sf_mean,
              mean2 = this_sub_sf_mean_two)  %>%                
		dplyr::select(.data$x, .data$sf1, .data$mean1, .data$sf2, .data$mean2, 
		              .data$per, .data$x) %>%
		tidyr::gather(key = "sftwo", value = "value", -x, -per)

	this_sf_two_df <- tidyr::unite(this_sf_df, this_key, c(sftwo, per))
	
	this_key_sorted <- sort(unique(this_sf_two_df$this_key))
	color_vector[stringr::str_detect(this_key_sorted, "sf1")]   <- subplot_color[1]
	color_vector[stringr::str_detect(this_key_sorted, "mean1")] <- subplot_color[2]
	color_vector[stringr::str_detect(this_key_sorted, "sf2")]   <- subplot_color[3]
	color_vector[stringr::str_detect(this_key_sorted, "mean2")] <- subplot_color[4]
	
	this_line_label <- NULL
	if (!is.null(this_sf_label)) {
		if (!is.null(this_sf_label_two)) {
			this_line_label <- 
				paste0(this_sf_label, " = ", subplot_color[1], ", ",
				       this_sf_label_two, " = ", subplot_color[3])
		}
	}
	
	if (!is.null(this_line_label)) {
		if (is.null(this_x_label)) {
			this_x_label <- this_line_label
		} else {
			this_x_label <- paste0(this_x_label, "\n", this_line_label)
		}
	}
	
	p_sf_two <- 
		ggplot2::ggplot(this_sf_two_df) + 
		ggplot2::geom_line(ggplot2::aes(x = .data$x, 
					y = .data$value, 
					color = .data$this_key)) + 
		ggplot2::scale_color_manual(values = color_vector) +
        ggplot2::labs(title = main_title,
                      subtitle = sub_title,
                      x = this_x_label,
                      y = this_y_label) +
		ggplot2::xlim(range(this_x_var)) + 			  
        ggplot2::scale_y_continuous(limits = this_sf_range) + 
		ggplot2::theme_bw() +
		ggplot2::theme(legend.position="none") 

    return(p_sf_two)
}
