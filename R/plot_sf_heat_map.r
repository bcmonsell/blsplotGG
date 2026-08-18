#' Seasonal factor heat map
#'
#' Generates a heat map of the seasonal factors. 
#'
#' Version 1.2, 2026-07-21
#'
#' @param this_sf array of seasonal factors stored as a time series
#' @param main_title Character string; main title of plot.  
#'        Default is \code{'Seasonal Sub-Plots'}.
#' @param sub_title Character string; subtitle of plot.  Subtitle not produced if not specified.
#' @param this_x_label Character string; label for x-axis of plot.  Default is a blank x-axis.
#' @param this_y_label Character string; label for y-axis of plot.  Default is a blank y-axis.
#' @param this_palette Character string; default \code{RColorBrewer} palette \insertCite{RColorBrewer}{blsplotGG}.
#'        Deault is \code{"YlOrRd"}.
#' @return A \code{ggplot} object which generates a heat map of the seasonal factors.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' air_seas    <- 
#'   seasonal::seas(AirPassengers, arima.model = '(0 1 1)(0 1 1)', 
#'                  x11 = '')
#' air_sf      <- seasonal::series(air_seas, 'd10')
#' 
#' air_sf_heat_map <- 
#'     plot_sf_heat_map(air_sf,
#'          main_title = "Airline Passengers Heat Map",
#'          sub_title  = "X-11 Seasonal Factors",
#'          this_x_label = "Month",
#'          this_palette = "YlGnBu")
#' @import graphics
#' @import stats
#' @importFrom Rdpack reprompt
#' @export
plot_sf_heat_map <- 
    function(this_sf = NULL, 
			 main_title = "Seasonal Heat Map", 
			 sub_title = NULL, 
			 this_x_label = NULL, 
			 this_y_label = NULL, 
			 this_palette = "YlOrRd") {
    # Author: Brian C. Monsell (OEUS) Version 1.2, 2026-07-21

    # check if a value is specified for \code{this_sf}
    if (is.null(this_sf)) {
        cat("must specify a set of seasonal factors")
		return(NULL)
    }
	
	this_freq <- frequency(this_sf)
	
	sf_df <- 
		data.frame(Period = as.double(cycle(this_sf)),
			Year = as.double(time(this_sf) %/% 1.0),
			SF = as.double(this_sf))
			
	
	gg_heat_map <-
		ggplot2::ggplot(sf_df, ggplot2::aes(factor(.data$Period), .data$Year, fill= .data$SF)) + 
			ggplot2::geom_tile() +
			ggplot2::scale_fill_distiller(palette = this_palette, direction = 1) + 
			ggplot2::theme_bw() + 
			ggplot2::theme(
				panel.grid.major = ggplot2::element_blank(), 
				panel.grid.minor = ggplot2::element_blank()) +
			ggplot2::scale_x_discrete(name = this_x_label, breaks = 1:this_freq,
				labels = as.character(1:this_freq)) + 
			ggplot2::labs(title = main_title, 
				subtitle = sub_title,
                  y = this_y_label)

	return(gg_heat_map)
	
}