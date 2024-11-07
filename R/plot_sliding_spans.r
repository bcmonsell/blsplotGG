#' Compare spans from the sliding spans analysis
#'
#' Generates a \code{ggplot} object with a time series plot that compares the series from
#' each of the sliding spans generated from a sliding spans analysis
#'
#' Version 1.7, 11/7/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series
#'        This is a required entry.
#' @param this_series Character string; three character code for the type of series to be generated.
#'        Allowed entries are \code{"sfs"} (seasonal factors, default), 
#'             \code{"chs"} (period-to-period changes), 
#'             \code{"sis"} (indirect seasonal factors),
#'             \code{"cis"} (indirect period-to-period changes).
#' @param main_title Title for the plot. 
#'        Default is character string \code{'Plot of Sliding Spans'}.
#' @param sub_title Subtitle for the plot. Optional entry.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param line_color Character vector of length 2 to 4 based on the number of spans.
#'        Default is generated from the \code{RColorBrewer} palette \code{"Dark2"}.
#' @param this_palette Character string; default \code{RColorBrewer} palette.
#'        Deault is \code{"Dark2"}.
#' @param this_guide_legend Title for legend.  Default is \code{"Sliding Spans"}
#' @return A \code{ggplot} object that generates a plot compares the series from
#'         each of the sliding spans generated from a sliding spans analysis
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' shoes_sspan_seas <- 
#'    seasonal::seas(blsplotGG::shoes2008, 
#'                   arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log", 
#'                   forecast.maxlead = 36,
#'                   slidingspans.save = c('sfs', 'chs'))
#' p_shoes_spans_sf <- 
#'     plot_sliding_spans(shoes_sspan_seas, "sfs",
#'                        main_title = "Spans of Seasonal Factors",
#'                        sub_title = "US Shoe Sales",
#'                        this_palette = "Set1")
#' @importFrom rlang .data
#' @export
#' @importFrom magrittr %>%
plot_sliding_spans <- 
    function(seas_obj = NULL, 
             this_series = "sfs", 
             main_title = "Plot of Sliding Spans", 
             sub_title = NULL, 
             do_grid = FALSE,
             do_background = FALSE,
             line_color = NULL, 
             this_palette = "Dark2",
             this_guide_legend = "Sliding Spans") {
    # Author: Brian C. Monsell (OEUS) Version 1.7 11/7/2024
	
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
        }
    }

    spans_type_all <- c("sfs", "chs", "sis", "cis")
	if(sum(match(spans_type_all, this_series), na.rm = TRUE) == 0) {
        stop(paste("Sliding spans series type not valid:", this_series))
	}
	
	this_matrix <- seasonal::series(seas_obj, this_series)
	
	num_spans <- ncol(this_matrix) - 1
	
    if (is.null(line_color)) {
        line_color <- RColorBrewer::brewer.pal(4, this_palette)[1:num_spans]
    } else {
        if (length(line_color) < num_spans) {
			if (length(line_color) > 1) {
                warning("Number of line colors specified less than series plotted.")
                warning("Will use colorRampPalette to increase number of colors.")
                line_color <- grDevices::colorRampPalette(line_color)(num_spans)
            }
        }
    }
	
	spans_df <- 
		data.frame(date = tsbox::ts_df(this_matrix[,1])$time,
	           span1 = as.double(this_matrix[,1]),
			   span2 = as.double(this_matrix[,2]))
	this_legend_label <- c("Span1", "Span2")
	if (num_spans > 2) {
		spans_df$span3 = as.double(this_matrix[,3])
		this_legend_label <- c(this_legend_label, "Span3")
		
		if (num_spans > 3) {
			spans_df$span4 = as.double(this_matrix[,4])
			this_legend_label <- c(this_legend_label, "Span4")
			spans_df <- spans_df %>%          
				dplyr::select(.data$date, .data$span1, .data$span2, .data$span3, .data$span4)
		} else {
			spans_df <- spans_df %>%          
				dplyr::select(.data$date, .data$span1, .data$span2, .data$span3)
		}
	} else {
		spans_df <- spans_df %>%          
			dplyr::select(.data$date, .data$span1, .data$span2)
	}
	
	spans_df <- spans_df %>%          
        tidyr::gather(key = "span", value = "value", -date)
	
	p_spans <- 
		ggplot2::ggplot(spans_df) + 
            ggplot2::geom_line(ggplot2::aes(x = .data$date, 
                                            y = .data$value, 
                                            color = .data$span), na.rm=TRUE) + 
            ggplot2::scale_color_manual(labels = this_legend_label, 
                                        values = line_color) +
            ggplot2::labs(title = main_title,
                          subtitle = sub_title,
                          x = NULL, y = NULL) +
            ggplot2::guides(color = ggplot2::guide_legend(this_guide_legend)) +
            ggplot2::theme(legend.position = "bottom")

	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		p_spans <- p_spans + ggplot2::theme_bw()
    }

	# remove grid lines if \code{do_grid = FALSE}
	if (!do_grid) {
	    p_spans <- p_spans + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
	}
       
	return(p_spans)
}
