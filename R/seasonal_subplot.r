#' Seasonal sub-plot
#'
#' Generates a seasonal sub-plot from a ts object of seasonal (or combined adjustment) factors
#'
#' Version 2.0, 5/6/2024
#'
#' @param this_sf Time series of seasonal factors from X-11 or SEATS
#' @param this_sf_range Range of values you wish the plot to be plotted over. Default is range of the series.
#' @param main_title Title for the plot. Default is character string \code{'Ratio Plot'}.
#' @param sub_title Subtitle for the plot. Default is \code{NULL}.
#' @param this_x_label Label for X axis. Default is \code{"Time"}.
#' @param this_y_label Label for Y axis. Default is \code{"Ratio"}.
#' @param subplot_color Color used for lines in ratio plot.  Default is \code{"steelblue"}.
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
#' ukgas_x11_sf     <- seasonal::series(ukgas_x11_seas, "d10")
#' p_ukgas_sf_sub   <- 
#'     seasonal_subplot(ukgas_x11_sf, 
#'                      main_title = "UK Gas X-11 Seasonal Factors",
#'                      sub_title = "Seasonal Filter: 3x5", 
#'                      this_x_label = "Quarter")
#' @importFrom rlang .data
#' @export
seasonal_subplot <- function(this_sf = NULL, this_sf_range = NULL, 
                             main_title = "Seasonal Subplot", 
                             sub_title = NULL, 
                             this_x_label = "Month", this_y_label = "",
                             subplot_color = "steelblue") {
    # Author: Brian C. Monsell (OEUS) Version 2.0, 5/6/2024

    if (is.null(this_sf)) {
        cat("Must specify a time series of seasonal factors.")
        return(NULL)
    }
    
    # check if \code{this_sf} is a ts object
    if (!is.ts(this_sf)) {
        cat("must specify a ts object")
	return(NULL)
    }
    
    if (is.null(this_sf_range)) {
        this_sf_range <- range(this_sf)
    }
    
    freq <- frequency(this_sf)
    
    this_x_var <- vector(mode = "numeric", length = 0)
    this_y_var <- vector(mode = "numeric", length = 0)
    this_per   <- vector(mode = "character", length = 0)

    this_sub_sf_mean <- vector(mode = "numeric", length = 0)

    sf_period <- cycle(this_sf)

    for (i in 1:freq) {
        this_y_var  <- c(this_y_var, this_sf[sf_period == i])
        this_sub_num <- length(this_sf[sf_period == i])
    
        s1 <- (i-1) + 0.6
        s2 <- i + 0.4
        this_x_var   <- c(this_x_var, seq(s1, s2, length.out = this_sub_num))
    
        this_sub_sf_mean <- 
          c(this_sub_sf_mean, rep(mean(this_sf[sf_period == i]), this_sub_num))
    
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
        data.frame(sf = this_y_var, x = this_x_var, month = this_per, mean = this_sub_sf_mean)

    return(ggplot2::ggplot(this_sf_df) + 
             ggplot2::geom_line(ggplot2::aes(x = .data$x, 
                                             y = .data$sf, 
                                             group = .data$month, 
                                             color = .data$month)) + 
             ggplot2::scale_color_manual(values = rep(subplot_color, freq)) +
             ggplot2::geom_line(ggplot2::aes(x = .data$x, 
                                             y = .data$mean, 
                                             group = .data$month)) + 
             ggplot2::ggtitle(main_title, subtitle = sub_title) +
             ggplot2::ylab(this_y_label) + ggplot2::xlab(this_x_label) +
             ggplot2::scale_x_continuous(breaks = 1:freq, 
                                         labels = unique(this_per)) + 
             ggplot2::scale_y_continuous(limits = this_sf_range) + 
             ggplot2::theme(legend.position="none"))
}
