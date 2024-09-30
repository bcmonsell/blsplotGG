#' add lines for outliers
#'
#' add lines for outliers to a ggplot plot object
#'
#' Version 3.2, 9/5/2024
#'
#' @param this_p A \code{ggplot} object of a time series plot. 
#'        This is a required entry.
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series. 
#'        This is a required entry.
#' @param line_color Character array of length 6; color used for different outliers, with the order being 
#'        \code{'ao', 'ls', 'tc', 'so', 'rp', 'tls'}. 
#'        Default is the \code{RColorBrewer} palette \code{"Dark2"}.
#' @param this_palette Character string; default \code{RColorBrewer} palette.
#'        Deault is \code{"Dark2"}.
#' @param this_line_type Character array of length 6; Line type used for different outliers, with the order being 
#'        \code{'ao', 'ls', 'tc', 'so', 'rp', 'tls'}. 
#'        Default is \code{c('dashed', 'dotdash', 'dashed', 'twodash', 'dotdash', 'dotdash')}. 
#' @return Revised \code{ggplot} object with lines for outliers added.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <- 
#'    seasonal::seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)", x11 = "")
#' air_df   <- 
#'    data.frame(date = tsbox::ts_df(AirPassengers)$time, 
#'               ori = as.double(seasonal::original(air_seas)),  
#'               sa = as.double(seasonal::final(air_seas)), 
#'               trend = as.double(seasonal::trend(air_seas)))
#' this_x11_p <- ggplot2::ggplot() + 
#'   ggplot2::geom_line(mapping = ggplot2::aes(x=date, y = ori), 
#'                      color = "grey", data = air_df) +
#'   ggplot2::geom_line(mapping = ggplot2::aes(x=date, y = sa), 
#'                      color="steelblue", linetype="twodash", data = air_df) + 
#'   ggplot2::geom_line(mapping = ggplot2::aes(x=date, y = trend), 
#'                      color="darkred", linetype="twodash", data = air_df) + 
#'   ggplot2::labs(
#'     title = "Airline Passenger X-11 Seasonal Adjustment",
#'     subtitle = NULL,
#'     x = "Time",
#'     y = "Airline Passengers")
#' this_x11_p_with_outlier_lines <- 
#'     add_outlier_lines(this_x11_p, air_seas, this_palette = "Paired",
#'                       this_line_type = rep("dotted", 6))
#' @importFrom rlang .data
#' @import stats
#' @export
add_outlier_lines <- 
    function(this_p = NULL, 
	         seas_obj = NULL, 
             line_color = c("red", "blue", 'orangered', "green", "steelblue", "blue"),
			 this_palette = "Dark2", 
             this_line_type = c("dashed", "dotdash", "dashed", "twodash", "dotdash", "dotdash")) {
    # Author: Brian C. Monsell (OEUS) Version 3.2, 9/5/2024
  
#   check if \code{this_p} is specified and is a list object
    if (is.null(this_p)) {
        cat("must specify a ggplot object")
        return(NULL)
    } else {
        if (!ggplot2::is.ggplot(this_p)) {
            cat("this is not a ggplot object")
            return(NULL)
        }
    }
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        cat("must specify a seas object")
        return(NULL)
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("Second argument must be a seas object")
        }
    }
	
	if (is.null(line_color)) {
	    this_palette_max <- RColorBrewer::brewer.pal.info[this_palette, "maxcolors"]
	    line_color <- RColorBrewer::brewer.pal(6, this_palette)
	} else {
		if (length(line_color) < 6) {
			if (length(line_color) > 1) {
				line_color <- grDevices::colorRampPalette(line_color)(6)
			}
		}
	}
    
    # inititalize outlier codes.
    all_outliers <- c("ao", "ls", "tc", "so", "rp", "tls")
    i_outliers <- 1:6

    # generate regressors, grab series frequency
    this_reg <- unlist(strsplit(get_reg_string(seas_obj), " "))
    this_freq <- seasonal::udg(seas_obj, "freq")
	n_reg <- length(this_reg)

    if (n_reg > 0) {
        
        # process each regressor.
        for (i in 1:n_reg) {
		    that_reg <- this_reg[i]
            
            # set code for potential outliers
            if (tolower(substr(that_reg, 1, 3)) == "tls") {
                this_code <- "tls"
            } else {
                this_code <- tolower(substr(that_reg, 1, 2))
            }
            
            # check code to see if the regressor is an outlier
            if (sum(match(all_outliers, this_code), na.rm = TRUE) > 0) {
                # process name of regressor to get outlier date(s)
                this_o <- proc_outlier(that_reg, this_freq)
				this_o_period <- this_o$period
				if (this_freq == 4) {
				    this_o_period <- ((this_o_period - 1) * 3) + 1
				}
                x_otlr <- 
				    as.Date(paste0(this_o$year, "-", formatC(this_o_period, width = 2, format = "d", flag = "0"), "-01"))
                i_col <- i_outliers[all_outliers == this_code]

                # draw line for outlier
				this_p <- 
				    this_p + ggplot2::geom_vline(xintercept = x_otlr, linetype = this_line_type[i_col], color = line_color[i_col])
                if (this_code == "rp" | this_code == "tls") {
				    this_o_period <- this_o$period2
				    if (this_freq == 4) {
				        this_o_period <- ((this_o_period - 1) * 3) + 1
				    }
                    x_otlr2 <- 
					    as.Date(paste0(this_o$year2, "-", formatC(this_o_period, width = 2, format = "d", flag = "0"), "-01"))
				    this_p <- 
				        this_p + ggplot2::geom_vline(xintercept = x_otlr2, linetype = this_line_type[i_col], color = line_color[i_col])
                }
            }
        }
	}
	
	return(this_p)
        
}
