#' CUSUM of squares plot for residuals time series object
#'
#' Generate CUSUM of squares plot for the residuals time series (\code{ts}) object. 
#' For information on the CUSUM methodology, see \insertCite{cusum75;textual}{blsplotGG}.
#'
#' Version 1.3, 2026-10-27
#'
#' @param this_series \code{ts} object of a single time series.
#'        This is a required entry.
#' @param nefobs Integer scalar; number of effective obserations -
#'        the number of obserations after differencing.
#'        Default is the length of \code{this_series}.
#' @param nb Integer scalar; number of regressors estimated.
#'        Default is \code{0}.
#' @param nmodel Integer scalar; number of AR and MA parameters estimated.
#'        Default is \code{0}.
#' @param main_title Character string; main title of plot. 
#'        Default is \code{"CUSUM plot of squared residuals"}.
#' @param sub_title Character string; subtitle of plot. 
#'        Default is a key to line colors used in the plot.
#' @param title_size integer scalar; Size of the main titles. 
#'        The subtitle size will be \code{title_size} - 2.
#'        Default is \code{12}.
#' @param this_y_label Character string; y-axis label for plot, if specified.
#' @param this_x_label Label for X axis. Default is \code{"Time"}.
#' @param do_grid Logical scalar; indicates if this plot will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param line_color Character string; color used for the CUSUM in the plot.  
#'        Default is \code{"blue"}. For a listing of R color codes, 
#'        see \insertCite{rchartsColors;textual}{blsplotGG}.
#' @param bound_p5_color Character string; color used for the signficance bounds
#'        for \code{alpha = 0.05} in the plot.  
#'        Default is \code{"forestgreen"}.
#' @param bound_p1_color Character string; color used for the signficance bounds
#'        for \code{alpha = 0.01} in the plot.  
#'        Default is \code{"green"}.
#' @param reference_color Character string; color used for the reference line
#'        for CUSUM in the plot.  
#'        Default is \code{"lightgrey"}. 
#' @return A \code{ggplot} object that generates a CUSUM plot of regARIMA residuals
#'         with significance bounds. 
#'         If \code{seas_obj} not specified, print out error message and 
#'         return \code{NULL}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' set.seed(42)
#' this_sim <- rnorm(180)
#' this_sim <- 
#'     ts(this_sim[61:180], start = c(2005, 1), frequency = 12)
#' air_cusum_p <- 
#'     plot_cusum_series(this_sim, nefobs = length(this_sim),
#'          this_y_label = "Sim", 
#'          main_title = "Normal(0,1) Simulations",
#'          do_grid = TRUE, 
#'          line_color = "darkblue",
#'          reference_color = "darkgrey",
#'          title_size = 10)
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom Rdpack reprompt
#' @export
plot_cusum_series <- 
    function(this_series = NULL,
		nefobs = NULL,
		nb = 0,
		nmodel = 0,
        main_title = NULL, 
        sub_title = NULL,
        title_size = 12,
        this_y_label = NULL, 
        this_x_label = NULL,
        do_grid = FALSE, 
        do_background = FALSE, 
        line_color = "blue",
        bound_p5_color = "forestgreen",
        bound_p1_color = "green",
        reference_color = "lightgrey") {
    # Author: Brian C. Monsell (OEUS) Version 1.3, 2026-10-27
    
    # check if a value is specified for \code{this_series}
    if (is.null(this_series)) {
		cat("must specify a time series object")
		return(NULL)
    }
	
	if (tsbox::ts_boxable(this_series)) {
		if (!is.ts(this_series)) {
		# attempt to convert to \code{ts} format
			this_series <- tsbox::ts_ts(this_series)
		}
	} else {
		cat("must specify a ts object for the this_series argument.")
		return(NULL)
	}
	
	if (is.null(nefobs)) {
		nefobs <- length(this_series)
	}
	
    # check if \code{title_size} is valid, return \code{NULL} if not.
    if (title_size <= 0) {
        cat(paste("Legend text size not valid (must be greater than zero):", 
		           title_size))
        return(NULL)
    }
	    
    e <- this_series
	
	# get length of residuals
    N <- length(e)
	k <- (N - nefobs) + nb + nmodel
	
	e_eff <- 
		ts(e[(k+1):N], end = end(e), frequency = frequency(e))

	# Calculate the Cumulative Sum of Squares (s_t)
    cum_sq_sum <- cumsum(e_eff^2)
    total_sq_sum <- sum(e_eff^2)
    s_t <- cum_sq_sum / total_sq_sum
	
	# Time scaling vector from 0 to 1
    t_scaled <- (1:(N-k)) / (N-k)
	
	# Calculate Upper and Lower Bounds
    c_val_p5 <- 1.358 / sqrt(N-k) 
    c_val_p1 <- 1.63 / sqrt(N-k) 

    upper_bound_p5 <- t_scaled + c_val_p5
    lower_bound_p5 <- t_scaled - c_val_p5

    upper_bound_p1 <- t_scaled + c_val_p1
    lower_bound_p1 <- t_scaled - c_val_p1
	
	# Set value of \code{all_line_color} and \code{line_label|
    all_line_color <- 
      c(line_color, bound_p5_color, bound_p5_color, 
        bound_p1_color, bound_p1_color, reference_color)
		
    line_label <- 
      c("cusum", "low5", "up5", "low1", "up1", "ref")
		
	# set text for line colors
    this_line_color <- 
      paste0("CUSUM = ", line_color, ", 5% = ", bound_p5_color, 
             ", 1% = ", bound_p1_color)
			   
	# set \code{sub_title}
    if (is.null(sub_title)) {
      sub_title <- this_line_color
    } else {
      sub_title <- paste0(sub_title, "\n", this_line_color)
    }

    xval = NULL
    e_time <- time(e_eff)
    cusum_df <- data.frame(
      xval  = as.double(e_time),
      cusum = s_t,
      ref   = t_scaled,
      upper5 = upper_bound_p5,
      lower5 = lower_bound_p5,
      upper1 = upper_bound_p1,
      lower1 = lower_bound_p1) %>%
    dplyr::select(.data$xval, .data$cusum, .data$lower5, .data$upper5, 
			.data$lower1, .data$upper1, .data$ref) %>%
		tidyr::gather(key = "cusum", value = "value", -xval) 
	
    rfreq <- NULL
    p_cusum <- 
      ggplot2::ggplot(cusum_df) + 
      ggplot2::geom_line(ggplot2::aes(x = .data$xval, 
                                      y = .data$value, 
                                      color = .data$cusum)) + 
      ggplot2::scale_color_manual(labels = line_label, 
                                  values = c("cusum"   = all_line_color[1],
                                             "lower5"  = all_line_color[2],
                                             "upper5"  = all_line_color[3],
                                             "lower1"  = all_line_color[4],
                                             "upper1"  = all_line_color[5],
                                             "ref"     = all_line_color[6]),
                                  breaks=c("cpgram", "lower5", "upper5", 
                                           "lower5", "upper5", "ref")) +
      ggplot2::labs(title = main_title,
                    subtitle = sub_title,
                    x = this_x_label,
                    y = this_y_label) + 
      ggplot2::theme(plot.title = ggplot2::element_text(size = title_size),
                     plot.subtitle = ggplot2::element_text(size = title_size - 2))

	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
      p_cusum <- p_cusum + ggplot2::theme_bw()
    }

	# remove grid lines if \code{do_grid = FALSE}
    if (!do_grid) {
	    p_cusum <- p_cusum + 
	      ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
	                     panel.grid.minor = ggplot2::element_blank())
    }
	
    p_cusum <- p_cusum + ggplot2::theme(legend.position = "none")
	
    return(p_cusum)
}
