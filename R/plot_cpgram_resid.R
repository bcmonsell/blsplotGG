#' Generate cumulative periodogram of the regARIMA residuals
#'
#' Generates a plot of the cumulative periodogram of the regARIMA residuals.
#'
#' Version 1.6, 7/1/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series.
#'                 This is a required entry.
#' @param main_title Title for the plot. Default is character string \code{'Cumulative periodogram'}.
#' @param this_palette Color used for lines in plot.  Default is a color-blind friendly palette 
#'        generated by the function \code{color_blind_palette(FALSE)}
#' @return Generates a \code{ggplot} object of the cumulative periodogram of the regARIMA residuals. 
#'         Diagnostic information is included in the plot subheader.
#' @examples
#' air_seas <- 
#'    seasonal::seas(AirPassengers, transform.function= "log", 
#'                   arima.model = "(0 1 1)(0 1 1)", check.save = "acf")
#' plot_cpgram_resid_air <-
#'      plot_cpgram_resid(air_seas, 
#'                        main_title = "Cumulative periodogram for Airline Passenger Residuals")
#' @import graphics
#' @import stats
#' @export
plot_cpgram_resid <- function(seas_obj = NULL, main_title = "Cumulative periodogram",
                              this_palette = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 1.6, 7/1/2024

    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
        }
    }
    if (is.null(this_palette)) {
        this_palette <- color_blind_palette(FALSE)
    }
    resid <- seasonal::series(seas_obj, "rsd")
    p_cpgram <- ggfortify::ggcpgram(resid)
    
    if (!is.null(main_title)) {
        m_acf <- seasonal::series(seas_obj, "acf")
        freq <- seasonal::udg(seas_obj, "freq")
        
        if (nrow(m_acf) < 2 * freq) {
            sub_header <- paste("LB", freq, "=", format(m_acf[freq, 3], digits = 2, nsmall = 1), ", PV", 
                                freq, "=", format(m_acf[freq, 5], digits = 2, nsmall = 2), sep = "")
        } else {
            f2 <- freq * 2
            sub_header <- 
                paste("LB", freq, "=", format(m_acf[freq, 3], digits = 2, nsmall = 1), ", PV", 
                      freq, "=", format(m_acf[freq, 5], digits = 2, nsmall = 2), ", LB", f2, "=", 
                      format(m_acf[f2, 3], digits = 2, nsmall = 1), ", PV", f2, "=", 
                      format(m_acf[f2, 5], digits = 2, nsmall = 2), sep = "")
        }
    
        p_cpgram <- p_cpgram + ggplot2::labs(title = main_title, subtitle = sub_header)
    
    }
    
    return(p_cpgram)

}
