#' Generate double spectrum plot of the original and seasonally adjusted series.
#'
#' Generate plot of spectrum of original series and seasonally adjusted series on same axis.
#'
#' Version 2.5, 7/1/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series
#'                  This is a required entry.
#' @param xaxis_bls Logical scalar; indicates if x-axis of spectral plot will be frequency by month 
#'                  rather than the actual frequencies. Default sets x-axis to frequency by month.
#' @param main_title Character string; main title of plot.  Default is  \code{'AR Spectrum'}.
#' @param series_name Character scalar; name of the time series used in \code{seas_obj}. 
#'                    Used as the label of the Y-axis if specified.
#' @param this_color Character vector of length 2. Colors used for original and seasonally adjusted spectrum in plot.
#'                     Defaults are \code{c("darkblue", "darkgreen")}.
#' @param this_median_color Character vector of length 2. Colors used for medians of the original and 
#'                           seasonally adjusted spectrum, respectively. Defaults are \code{c("blue", "green")}.
#' @param this_freq_color Character vector of length 2. Colors used for seasonal and trading day frequencies, 
#'                         respectively. Defaults are \code{c("steelblue", "forestgreen")}.
#' @param this_peak_color Character vector of length 2. Colors used for peaks at seasonal and trading day 
#'                         frequencies, respectively. Defaults are \code{c("violet", "brown")}.
#' 
#' @return \code{ggplot} object of spectrum of original series and seasonally adjusted series on same axis.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' air_seas <- seasonal::seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)", x11="",
#'                            spectrum.save = c("sp0", "sp1"))
#' plot_double_spectrum_air <- 
#'      plot_double_spectrum(air_seas, series_name = "AirPassengers",
#'                           this_color = c("steelblue", "forestgreen"),  
#'                           this_median_color = c("blue", "green"), 
#'                           this_freq_color = c("darkblue", "darkgreen"), 
#'                           this_peak_color = c("red", "orange"))
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_double_spectrum <- function(seas_obj = NULL, xaxis_bls = TRUE, 
                                 main_title = "AR Spectrum", series_name = NULL,
                                 this_color = c("darkblue", "darkgreen"), 
                                 this_median_color = c("blue", "green"),
                                 this_freq_color = c("steelblue", "forestgreen"), 
                                 this_peak_color = c("violet", "brown")) {
    # Author: Brian C. Monsell (OEUS) Version 2.5, 7/1/2024

    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
        }
    }

    # extract spectrum of seasonally adjusted series for X-11
    sp1 <- seasonal::series(seas_obj, "sp1")
    # if not available, try to extract spectrum of seasonally adjusted series for SEATS
    if (is.null(sp1)) {
        sp1 <- seasonal::series(seas_obj, "s1s")
        # if not available, print error message and stop
        if (is.null(sp1)) {
            stop("unable to extract spectrum of seasonally adjusted series")
        }
    }
    
    # convert sp1 to a data frame
    sp1_df <- as.data.frame(sp1)
    colnames(sp1_df) <- c("Frequency", "Spectrum.SA")
    
    # extract spectrum of original series
    sp0 <- seasonal::series(seas_obj, "sp0")
    sp0_df <- as.data.frame(sp0)
    colnames(sp0_df) <- c("Frequency", "Spectrum.Ori")
    
    sp_both_df <- 
       data.frame(Frequency =    sp0_df$Frequency, 
                  Spectrum.Ori = sp0_df$Spectrum.Ori,
                  Spectrum.SA  = sp1_df$Spectrum.SA) %>%
          dplyr::select(.data$Frequency, .data$Spectrum.Ori, .data$Spectrum.SA) %>%
          tidyr::gather(key = "spec", value = "value", -.data$Frequency)
                  
    # initialize seasonal and trading day frequencies
    f_seas <- 1:5/12
    f_td <- c(0.3482, 0.432)

    if (xaxis_bls) {
        p_double_spec <- ggplot2::ggplot(sp_both_df) + 
            ggplot2::geom_line(ggplot2::aes(x = .data$Frequency, 
                                            y = .data$value, 
                                            color = .data$spec)) + 
            ggplot2::scale_color_manual(labels = c("Ori", "SA"), 
                                        values = this_color) +
            ggplot2::labs(title = main_title,
                          subtitle = "Dotted Line - Median, Star - Visual Peak",
                          x = "Period in Months",
                          y = series_name) +
            ggplot2::guides(color = ggplot2::guide_legend("Spectrum")) +
            ggplot2::scale_x_continuous(limits = c(0, 0.5),
                                        breaks = c(0, f_seas, 0.5),
                                        labels = ~ifelse(.x == 0, "Inf", 1/.x))
    } else {
        p_double_spec <- ggplot2::ggplot(sp_both_df) + 
            ggplot2::geom_line(ggplot2::aes(x = .data$Frequency, 
                                            y = .data$value, 
                                            color = .data$spec)) + 
            ggplot2::scale_color_manual(labels = c("Ori", "SA"), 
                                        values = this_color) +
            ggplot2::labs(title = main_title,
                          subtitle = "Dotted Line - Median, Star - Visual Peak",
                          x = "Frequency",
                          y = series_name) +
            ggplot2::guides(color = ggplot2::guide_legend("Spectrum"))+
            ggplot2::scale_x_continuous(limits = c(0, 0.5),
                                        breaks = c(0, f_seas, 0.5))
    }
    
    # draw lines for spectrum medians
    p_double_spec <- p_double_spec + 
                     ggplot2::geom_hline(yintercept = as.numeric(seasonal::udg(seas_obj, "spcori.median")), 
                                linetype = 2, 
                                colour = this_median_color[1]) + 
                     ggplot2::geom_hline(yintercept = as.numeric(seasonal::udg(seas_obj, "spcsa.median")), 
                                linetype = 2, 
                                colour = this_median_color[2])

    # draw lines for seasonal and TD frequencies
    p_double_spec <- p_double_spec + 
                     ggplot2::geom_vline(xintercept = f_seas, 
                                         linetype = "dotted", 
                                         color = this_freq_color[1]) +
                     ggplot2::geom_vline(xintercept = f_td, 
		                         linetype = "dotted", 
                                          color = this_freq_color[2])

    # get min and max of x and y axis from par
    y_plot_lim <- ggplot2::layer_scales(p_double_spec)$y$range$range
    x_plot_lim <- ggplot2::layer_scales(p_double_spec)$x$range$range

    # put 'TD' near trading day frequencies
    ypos <- y_plot_lim[2] - (y_plot_lim[2] - y_plot_lim[1])/30
    for (i in 1:length(f_td)) {
        p_double_spec <- p_double_spec +
            ggplot2::geom_text(x = f_td[i], y = ypos, label = "TD", 
                               color = this_freq_color[2], size = 2.5)
    }

    # determine if there are visually significant peaks in the spectrum of the original series.
    vp_ori_seas <- visual_sig_peaks(seas_obj, "ori")
    vp_ori_td <-   visual_sig_peaks(seas_obj, "ori", "td")

    # Add stars for visually significant seasonal peaks.
    if (vp_ori_seas[1] > 0) {
        for (i in 1:length(vp_ori_seas)) {
            indx <- vp_ori_seas[i] + 1
            p_double_spec <- p_double_spec +
                ggplot2::geom_text(x = sp0[indx, 1], y = sp0[indx, 2], label = "*", 
                                   color = this_peak_color[1], size = 5)
        }
    }

    # Add stars for visually significant trading day peaks.
    if (vp_ori_td[1] > 0) {
        for (i in 1:length(vp_ori_td)) {
            indx <- vp_ori_td[i] + 1
            p_double_spec <- p_double_spec +
                ggplot2::geom_text(x = sp0[indx, 1], y = sp0[indx, 2], label = "*", 
                                   color = this_peak_color[2], size = 5)
        }
    }

    # determine if there are visually significant peaks in the spectrum of the SA series.
    vp_sa_seas <- visual_sig_peaks(seas_obj, "sa")
    vp_sa_td <-   visual_sig_peaks(seas_obj, "sa", "td")

    # Add stars for visually significant seasonal peaks.
    if (vp_sa_seas[1] > 0) {
        for (i in 1:length(vp_sa_seas)) {
            indx <- vp_sa_seas[i] + 1
            p_double_spec <- p_double_spec +
                ggplot2::geom_text(x = sp1[indx, 1], y = sp1[indx, 2], label = "*", 
                                   color = this_peak_color[1], size = 5)
        }
    }

    # Add stars for visually significant trading day peaks.
    if (vp_sa_td[1] > 0) {
        for (i in 1:length(vp_sa_td)) {
            indx <- vp_sa_td[i] + 1
             p_double_spec <- p_double_spec +
                ggplot2::geom_text(x = sp1[indx, 1], y = sp1[indx, 2], label = "*", 
                                   color = this_peak_color[2], size = 5)
        }
    }
    
    return(p_double_spec)

}
