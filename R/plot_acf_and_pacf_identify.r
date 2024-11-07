#' Generate ACF and PACF plot of the regARIMA model residuals.
#'
#' Generate a single plot with ACF and PACF of the regARIMA model residuals.
#' 
#' Version 3.1, 11/6/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series
#'        This is a required entry.
#' @param this_diff Integer scalar; one of the regular differences specified in the 
#'        \code{diff} argument of the \code{identify} spec. Default is \code{0}.
#' @param this_sdiff Integer vector; one of the seasonal differences specified in the 
#'        \code{sdiff} argument of the \code{identify} spec. Default is \code{0}.
#' @param add_ci Character scalar; Overall title for the plot. 
#'        Default is confidence intervals are added. 
#' @param overall_title Overall title for the combined plot. 
#'        Default is a text string showing the orders of differencing selected.
#' @param acf_title Subtitle for the ACF Plot. 
#'        Default is character string \code{'ACF Plot'}.
#' @param pacf_title Subtitle for the PACF Plot. 
#'        Default is character string \code{'PACF Plot'}.
#' @param this_x_label Label for X axis. Default is \code{"Lags"}.
#' @param this_y_label Character vector of length two. Labels for each Y axis. 
#'        Default is \code{c("ACF", "PACF")}.
#' @param this_x_axis_breaks Numeric vector; sets the values for the x-axis.
#'        Default uses the value of \code{this_frequency} to set x-axis.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines.
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param acf_color Color used for lines in ACF plot.  
#'        Default is \code{"steelblue"}.
#' @return A \code{ggplot} object that produces an ACF and PACF plot for the original
#'         series with user-specified orders of differencing.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' shoes_identify_seas <- 
#'    seasonal::seas(shoes2008, 
#'                   identify.diff = c(0, 1),
#'                   identify.sdiff = c(0, 1),
#'                   identify.save = c("iac", "ipc"),
#'                   arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", 
#'                   forecast.maxlead = 36,
#'				     check.maxlag = 36,
#' 				     check.acflimit = 1.96,
#' 				     check.qlimit = 0.01,
#'                   check.print = c( 'pacf', 'pacfplot' ))
#' p_shoes_acf_and_pacf_identify_d0_sd0   <- 
#'    plot_acf_and_pacf_identify(shoes_identify_seas, 
#'             overall_title = "US Shoe Sales - No Differencing",
#'             acf_color = "darkblue")
#' p_shoes_acf_and_pacf_identify_d1_sd1   <- 
#'    plot_acf_and_pacf_identify(shoes_identify_seas, 
#'	           this_diff = 1,
#'			   this_sdiff = 1,
#'             overall_title = "US Shoe Sales - Regular and Seasonal Differencing",
#'             acf_color = "darkblue")
#' @export
plot_acf_and_pacf_identify <- 
	function(seas_obj = NULL,
	         this_diff = 0,
			 this_sdiff = 0,
			 add_ci = TRUE, 
			 overall_title = NULL,
			 acf_title = "ACF Plot", 
			 pacf_title = "PACF Plot", 
			 this_x_label = "Lag",
			 this_y_label = c("ACF", "PACF"),
			 this_x_axis_breaks = NULL,
			 do_grid = FALSE,
			 do_background = FALSE,			 
			 acf_color = "steelblue") {
    # Author: Brian C. Monsell (OEUS) Version 3.1, 11/6/2024
 
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
        }
    }
	
	this_key <- paste0("d", this_diff, "_sd", this_sdiff)
	
	this_acf_list  <- 
		convert_identify_acf(seas_obj, "iac")
	this_pacf_list <- 
		convert_identify_acf(seas_obj, "ipc")
		
	if (!(this_key %in% names(this_acf_list))) {
		stop("Improper order of differencing specified")
	}
							 
	this_acf_matrix  <- this_acf_list[[this_key]]
	this_pacf_matrix <- this_pacf_list[[this_key]]
	this_range <- range(this_acf_matrix[,1], this_pacf_matrix[,1])
	
	this_freq <- seasonal::udg(seas_obj, "freq")
	
	if (add_ci) {
		this_acflimit <- seasonal::udg(seas_obj, "acflimit")
		this_range <- 
			range(this_range, 
				  this_acflimit * this_acf_matrix[,2], 
				  this_acflimit * this_pacf_matrix[,2],
				  -this_acflimit * this_acf_matrix[,2], 
				  -this_acflimit * this_pacf_matrix[,2])
	}
	
	p_acf <- 
		plot_acf_matrix(this_acf_matrix[,2:6],
			 this_range,
			 add_ci = add_ci, 
			 this_acflimit,
			 main_title = acf_title,
			 this_x_label = this_x_label,
			 this_y_label = this_y_label[1],
			 this_frequency = this_freq,
			 this_x_axis_breaks = this_x_axis_breaks,
			 acf_color = acf_color)
			 
	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_acf <- p_acf + ggplot2::theme_bw()
    }	
	
	# remove grid lines if \code{do_grid = FALSE}
	if (!do_grid) {
	    p_acf <- p_acf + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
	}
       
	p_pacf <- 
		plot_acf_matrix(this_pacf_matrix[,2:3],
			 this_range,
			 add_ci = add_ci, 
			 this_acflimit,
			 main_title = pacf_title,
			 this_x_label = this_x_label,
			 this_y_label = this_y_label[2], 
			 this_frequency = this_freq,
			 this_x_axis_breaks = this_x_axis_breaks,
			 acf_color = acf_color)

	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_pacf <- p_pacf + ggplot2::theme_bw()
    }	

	# remove grid lines if \code{do_grid = FALSE}
	if (!do_grid) {
	    p_pacf <- p_pacf + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
	}
       
	p_acf_and_pacf <- 
		ggpubr::ggarrange(p_acf + ggpubr::rremove("x.text"), p_pacf,
						  ncol = 1, nrow = 2)
						  
	if (is.null(overall_title)) {
		overall_title <- 
			paste0("Regular Differences = ", this_diff, ", Seasonal Differences = ", this_sdiff)
	}
	p_acf_and_pacf <- ggpubr::annotate_figure(p_acf_and_pacf, 
		top = ggpubr::text_grob(overall_title))	

	return(p_acf_and_pacf)
}
