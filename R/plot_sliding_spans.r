#' Compare spans from the sliding spans analysis
#'
#' Generates a \code{ggplot} object with a time series plot that compares the series from
#' each of the sliding spans generated from a sliding spans analysis. 
#' For more information on sliding spans analysis, 
#' \insertCite{@see Sections 6.2 and 7.13 of @Census2025;textual}{blsplotGG}, with more
#' details in \insertCite{Findley90;textual}{blsplotGG}.
#'
#' Version 2.5, 2/17/2026
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
#'        For a listing of R color codes, 
#'        see \insertCite{rchartsColors;textual}{blsplotGG}.
#'        Default is the \code{RColorBrewer} palette \code{"Dark"} \insertCite{RColorBrewer}{blsplotGG}. 
#' @param this_palette Character string; default \code{RColorBrewer} palette.
#'        Deault is \code{"Dark2"}.
#' @param this_legend_title Title for legend.  Default is \code{"Sliding Spans"}
#' @param this_legend_title_size integer scalar; Size of the legend title.
#'        Default is \code{12}.
#' @param this_legend_text_size integer scalar; Size of the legend title.
#'        Default is \code{10}.
#' @param this_legend_position character String; The position of the legend, which can be one of
#'        \code{top}, \code{bottom}, \code{left}, \code{right}.
#'        Default is \code{right}.
#' @param this_legend_space integer scalar; Space between legend and plot in points.
#'        Default is \code{10}.
#' @return A \code{ggplot} object that generates a plot compares the series from
#'         each of the sliding spans generated from a sliding spans analysis
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
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
#' @importFrom magrittr %>%
#' @importFrom Rdpack reprompt
#' @export
plot_sliding_spans <- 
    function(seas_obj = NULL, 
             this_series = "sfs", 
             main_title = "Plot of Sliding Spans", 
             sub_title = NULL, 
             do_grid = FALSE,
             do_background = FALSE,
             line_color = NULL, 
             this_palette = "Dark2",
             this_legend_title = "Sliding Spans",
			 this_legend_title_size = 12,
			 this_legend_text_size = 10,
			 this_legend_position = "right",
			 this_legend_space = 10) {
    # Author: Brian C. Monsell (OEUS) Version 2.5, 2/17/2026
	
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        cat("must specify a seas object")
		return(NULL)
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            cat("First argument must be a seas object")
			return(NULL)
       }
    }

    spans_type_all <- c("sfs", "chs", "sis", "cis")
	if(sum(match(spans_type_all, this_series), na.rm = TRUE) == 0) {
        cat(paste("Sliding spans series type not valid:", this_series))
		return(NULL)
	}
	
	# check legend options 
	position_all <- c("top", "bottom", "left", "right")
	if(sum(match(position_all, this_legend_position), na.rm = TRUE) == 0) {
        cat(paste("Legend position choice not valid:", this_legend_position))
		return(NULL)
	}
	
	if(this_legend_text_size <= 0) {
        cat(paste("Legend text size not valid (must be greater than zero):", 
		           this_legend_text_size))
		return(NULL)
	}
	
	if(this_legend_title_size <= 0) {
        cat(paste("Legend title text size not valid (must be greater than zero):", 
		           this_legend_title_size))
		return(NULL)
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
            ggplot2::guides(color = ggplot2::guide_legend(this_legend_title)) +
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
	
	p_spans <- p_spans +
		ggplot2::theme(legend.text = ggplot2::element_text(size = this_legend_text_size),
					   legend.title = ggplot2::element_text(size = this_legend_title_size),
					   legend.position = this_legend_position,
					   legend.box.spacing = ggplot2::unit(this_legend_space, "pt"))

       
	return(p_spans)
}
