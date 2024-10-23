#' Generate ACF plot of the regARIMA model residuals from a matrix of the ACF.
#'
#' Generate ACF, PACF, or squared ACF plot of the regARIMA model residuals 
#' from a matrix of the ACF.
#' 
#' Version 1.3, 10/23/2024 
#'
#' @param acf_matrix Numeric matrix containing the ACF, PACF, or squared ACF with 
#'        columns with SE, Ljung Box Q, lags, if associated with the file.
#'        This is a required entry.
#' @param acf_range Range of values you wish the acf plot to be plotted over. 
#'        Default is range of the series.
#' @param add_ci Logical scalar; indicates if confidence intervals are added to the plot.
#'        Default is confidence intervals are added. 
#' @param acflimit Numeric scalar; the multiplier for the confidence interval usually read 
#'        from the \code{udg}. Default: \code{1.6}.
#' @param main_title Title for the plot. 
#'        Default is character string \code{'ACF Plot'}.
#' @param sub_title Subtitle for the plot. Default is \code{NULL}, or no subtitle.
#' @param this_x_label Label for X axis. Default is \code{"Lags"}.
#' @param this_y_label Label for Y axis. Default is \code{"ACF"}.
#' @param acf_color Color used for lines in ACF plot.  
#'        Default is \code{"steelblue"}.
#' @return A \code{ggplot} object that produces an ACF, PACF, or squared ACF plot
#'         of the regARIMA residuals.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' ukgas_x11_seas <- 
#'    seasonal::seas(UKgas, series.period = 4, 
#'                   arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", 
#'                   forecast.maxlead = 20,
#'                   x11.seasonalma = "s3x5", 
#'                   check.print = c( 'pacf', 'pacfplot' ),
#'                   check.maxlag = 12,
#'                   check.save = c("acf", "pcf", "ac2"))
#' ukgas_acf_matrix   <- 
#'    seasonal::series(ukgas_x11_seas, "acf")
#' p_ukgas_acf   <- 
#'    plot_acf_matrix(ukgas_acf_matrix, 
#'             main_title = "UK Gas Model Squared ACF",
#'             acf_color = "darkblue")
#' @export
plot_acf_matrix <- 
	function(acf_matrix = NULL,
	         acf_range = NULL,
			 add_ci = TRUE, 
			 acflimit = 1.6,
			 main_title = "ACF Plot",
			 sub_title = NULL, 
			 this_x_label = "Lag",
			 this_y_label = "ACF", 
			 acf_color = "steelblue") {
    # Author: Brian C. Monsell (OEUS) Version 1.3, 10/23/2024

	this_cf     <- acf_matrix[,1]
	if (add_ci) {
		this_upperCI <- acflimit * acf_matrix[,2]
		this_lowerCI <- -this_upperCI
	}

	if (is.null(acf_range)) {
		acf_range <- range(this_cf)
		if (add_ci) {
			acf_range <- range(acf_range, this_upperCI, this_lowerCI)
		}
	} 

	this_lag <- seq(1, length(this_cf))
	acf_level <- 0.0

	if (add_ci) {
		acf_df <- data.frame(
				lag = this_lag,
				acf = this_cf,
				lower = this_lowerCI,
				upper = this_upperCI)
					
		p_acf <- ggplot2::ggplot(acf_df) + 
			ggplot2::geom_segment(mapping=ggplot2::aes(x=lag, 
                                        y=acf_level, 
                                        xend=lag, 
                                        yend=acf), 
								color = acf_color) +
			ggplot2::labs(title = main_title, 
                      subtitle = sub_title, 
                      y = this_y_label) + 
			ggplot2::ylim(acf_range) +
			ggplot2::geom_line(ggplot2::aes(x = .data$lag, 
                                        y = .data$lower),
                                        color = "grey") + 
			ggplot2::geom_line(ggplot2::aes(x = .data$lag, 
                                        y = .data$upper),
                                        color = "grey") + 
			ggplot2::geom_hline(yintercept = acf_level, 
                            linetype = "solid", 
                            color = acf_color)
	} else {
		acf_df <- data.frame(
				lag = this_lag,
				acf = this_cf)
					
		p_acf <- ggplot2::ggplot(acf_df) + 
			ggplot2::geom_segment(mapping=ggplot2::aes(x=lag, 
                                        y=acf_level, 
                                        xend=lag, 
                                        yend=acf), 
                            color = acf_color) +
			ggplot2::labs(title = main_title, 
                      subtitle = sub_title, 
                      y = this_y_label) + 
			ggplot2::ylim(acf_range) +
			ggplot2::geom_hline(yintercept = acf_level, 
                            linetype = "solid", 
                            color = acf_color)
	}

	return(p_acf)
}