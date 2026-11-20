#' Generate alt text for \code{ggplot} graphs
#'
#' Generates alt text for \code{ggplot} graph objects; adds text as suggested by 
#' Amy Casale in her article "Writing Alt Text for Data Visualization" 
#' \insertCite{Cesal2020}{blsplotGG}.
#'
#' Version 5.0, 2026-11-09
#'
#' @param chart_type character scalar telling what type of plot is used. 
#'        This is a required entry.
#' @param data_type character scalar detailing what data is used. 
#'        This is a required entry.
#' @param reason_text character scalar detailing the reason the plot is generated. 
#'        This is a required entry.
#' @return generate alt text 
#'
#' 
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' air_seas <- seasonal::seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)", x11="")
#' air_df   <- 
#'    data.frame(date = tsbox::ts_df(AirPassengers)$time, 
#'               ori = as.double(seasonal::original(air_seas)), 
#'               sa = as.double(seasonal::final(air_seas)), 
#'               trend = as.double(seasonal::trend(air_seas)))
#' air_p <- ggplot2::ggplot(air_df, ggplot2::aes(x=date)) +
#'   ggplot2::geom_line(ggplot2::aes(y = ori), color = "grey") + 
#'   ggplot2::geom_line(ggplot2::aes(y = sa), 
#'                      color="steelblue", linetype="twodash") + 
#'   ggplot2::geom_line(ggplot2::aes(y = trend), 
#'                      color="darkred", linetype="dotdash") + 
#'   ggplot2::ggtitle("Airline Passenger X-11 Seasonal Adjustment")
#' air_alt_text <- 
#'    generate_alt_text("Time series plot", 
#'                      "International Airline Passengers time series",
#'                      "compare seasonal adjustment and trend to original series")
#' @importFrom Rdpack reprompt
#' @export
generate_alt_text <- 
	function(chart_type = NULL, 
		data_type = NULL, 
		reason_text = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 5.0, 2026-11-09
    
	if (is.null(chart_type)) {
		cat("must specify type of chart")
		return(NULL)
	}
    
	if (is.null(data_type)) {
		cat("must specify description of data used in plot")
		return(NULL)
	}
   
	if (is.null(reason_text)) {
		cat("must specify reason for chart")
		return(NULL)
	}
    
    # create short text based on Amy Casale's article
    return(paste0(chart_type, " of ", data_type, " where ", reason_text))
 
}
