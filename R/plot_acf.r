#' Generate ACF plot of the regARIMA model residuals.
#'
#' Generate ACF, PACF, or squared ACF plot of the regARIMA model residuals.
#' 
#' Version 2.2, 10/23/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series
#'        This is a required entry.
#' @param this_plot Character string; three character code for the type of plot to be generated.
#'        Allowed entries are \code{"acf"} (sample autocorrelation function, default), 
#'             \code{"pcf"} (sample partial autocorrelation function), 
#'             \code{"ac2"} (squared autocorrelation function).
#' @param acf_range Range of values you wish the acf plot to be plotted over. 
#'        Default is range of the series.
#' @param add_ci Logical scalar; indicates if confidence intervals are added to the plot.
#'        Default is confidence intervals are added. 
#' @param main_title Title for the plot. 
#'        Default is character string \code{'ACF Plot'}.
#' @param sub_title Subtitle for the plot. Default is \code{NULL}, or no subtitle.
#' @param this_x_label Label for X axis. Default is \code{"Lags"}.
#' @param this_y_label Label for Y axis. Default is \code{"ACF"}.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines.
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
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
#' p_ukgas_acf   <- 
#'    plot_acf(ukgas_x11_seas, this_plot = "acf",
#'             main_title = "UK Gas Model Residual ACF",
#'             acf_color = "darkblue")
#' p_ukgas_pacf   <- 
#'    plot_acf(ukgas_x11_seas, this_plot = "pcf",
#'             main_title = "UK Gas Model Residual PACF",
#'             acf_color = "darkblue")
#' p_ukgas_acf2   <- 
#'    plot_acf(ukgas_x11_seas, this_plot = "ac2",
#'             main_title = "UK Gas Model Squared ACF",
#'             acf_color = "darkblue")
#' @export
plot_acf <- 
	function(seas_obj = NULL,
			 this_plot = "acf",
			 acf_range = NULL,
			 add_ci = TRUE, 
			 main_title = "ACF Plot",
			 sub_title = NULL, 
			 this_x_label = "Lag",
			 this_y_label = "ACF", 
			 do_grid = FALSE,
			 do_background = FALSE,			 
			 acf_color = "steelblue") {
    # Author: Brian C. Monsell (OEUS) Version 2.2, 10/23/2024
 
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
        }
    }

    acf_type_all <- c("acf", "pcf", "ac2")
	if(sum(match(acf_type_all, this_plot), na.rm = TRUE) == 0) {
        stop(paste("ACF plot type not valid:", this_plot))
	}
	
	this_matrix <- seasonal::series(seas_obj, this_plot)
	if (add_ci) {
		this_acflimit <- seasonal::udg(seas_obj, "acflimit")
	}
	
#	generate ggplot object for acf plot
	p_acf <- 
		plot_acf_matrix(this_matrix, 
		                acf_range,
						add_ci,
						this_acflimit,
						main_title,
						sub_title,
						this_x_label,
						this_y_label,
						acf_color)
						
	# remove grid lines if \code{do_grid = FALSE}
	if (!do_grid) {
	    p_acf <- p_acf + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
	}
       
	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_acf <- p_acf + ggplot2::theme_bw()
    }

	return(p_acf)
}
