#' Plot time series matrix
#'
#' Generate plot of a matrix of user-specified time series.
#' 
#' Version 4.2, 11/6/2024
#'
#' @param this_matrix Numeric matrix; columns of time series object to be plotted.
#' @param main_title Character string; main title of plot. 
#'        The default title is the name of the matrix passed to this function.
#' @param sub_title Character string; subtitle of plot. There is no default subtitle.
#' @param this_y_label Character string; y-axis label for plot, if specified.
#' @param y_limit Numeric vector of length 2; Range of values on plot y-axis
#'        Default is range of the series specified.
#' @param this_x_label Label for X axis. Default is \code{"Time"}.
#' @param start_plot Integer vector of length 2; Starting date for plot. 
#'        Default is starting date for the time series.
#' @param do_grid Logical scalar; indicates if plots will have grid lines. 
#'        Default is no grid lines.
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param line_color Character scalar; color used for plot. 
#'        User should specify one color for each column of the matrix specified.
#'        Default is the \code{RColorBrewer} palette \code{"Dark2"}.
#' @param this_palette Character string; default \code{RColorBrewer} palette.
#'        Deault is \code{"Dark2"}.
#' @param this_line_type Character string; indicates line type of each plot produced. 
#'        Default is \code{"solid"}.
#' @param do_facet Logical scalar; indicates if a facet plot is generated of the 
#'        different colums. 
#'        Default is \code{FALSE}.
#' @param reset_facet_y_axis Logical scalar; indicates if y-axis for facet plots 
#'        reset to \code{y_limit}
#'        Default is \code{FALSE}.
#' @return A \code{ggplot} object that produces a plot of user-specified time series. 
#'         If matrix not specified, print out error message and return NULL.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' BP_Region_Matrix <-
#'    cbind(blsplotGG::xt_data_list$mw1u, blsplotGG::xt_data_list$ne1u, 
#'          blsplotGG::xt_data_list$so1u, blsplotGG::xt_data_list$we1u)
#' colnames(BP_Region_Matrix) <- names(blsplotGG::xt_data_list)
#' p_BP <- blsplotGG::plot_matrix(BP_Region_Matrix, this_y_label = 'Building Permits', 
#'    main_title = "US Building Permits, 1 Family Units",
#'    do_grid = TRUE, this_line_type = rep("solid", 4),
#'    line_color = c("orange", "steelblue", "forestgreen", "brown"))
#' p_BP_facet <- blsplotGG::plot_matrix(BP_Region_Matrix, this_y_label = 'Building Permits', 
#'    main_title = "US Building Permits, 1 Family Units",
#'    do_grid = FALSE, do_facet = TRUE,
#'    line_color = c("orange", "steelblue", "forestgreen", "brown"))
#' @import ggfortify
#' @export
plot_matrix <- 
    function(this_matrix = NULL, 
	         main_title = deparse(substitute(this_matrix)), 
			 sub_title = NULL, 
			 this_y_label = NULL, 
			 y_limit = NULL, 
			 this_x_label = "Time", 
			 start_plot = NULL, 
			 do_grid = FALSE, 
			 do_background = FALSE,
			 line_color = NULL, 
			 this_palette = "Dark2", 
			 this_line_type = "solid", 
			 do_facet = FALSE, 
			 reset_facet_y_axis = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 4.2, 11/6/2024

    if (is.null(this_matrix)) {
        stop("Argument this_matrix must be specified.")
    }

    num_series <- ncol(this_matrix)

    if (length(this_line_type) < num_series) {
	    if (!do_facet) {
            warning("Number of line types specified less than number of series.")
            warning("Remaining elements set to be the first entry.")
		}
        this_line_type <- update_vector(this_line_type, num_series)
    }

	if (is.null(line_color)) {
	    this_palette_max <- RColorBrewer::brewer.pal.info[this_palette, "maxcolors"]
	    if (num_series < 3) {
			line_color <- RColorBrewer::brewer.pal(3, this_palette)[1:num_series]
		} else {
			if (num_series <= this_palette_max) {
				line_color <- RColorBrewer::brewer.pal(num_series, this_palette)
			} else {
				line_color <- 
					grDevices::colorRampPalette(RColorBrewer::brewer.pal(this_palette_max, this_palette))(num_series)
			}
		}
	} else {
		if (length(line_color) < num_series) {
			if (!do_facet) {
				warning("Number of line colors specified less than number of years.")
				warning("Will use colorRampPalette to increase number of colors.")
				line_color <- grDevices::colorRampPalette(line_color)(num_series)
			} else {
				if (length(line_color) > 1) {
					line_color <- grDevices::colorRampPalette(line_color)(num_series)
				}
			}
		}
	}

    # If start_plot specified, shorten series
    if (!is.null(start_plot)) {
        this_matrix <- window(this_matrix, start = start_plot)
    }
    
    this_plot <- 
	    ggplot2::autoplot(this_matrix, facets = do_facet) + 
		    ggplot2::scale_linetype_manual(values = this_line_type) +
		    ggplot2::scale_colour_manual(values = line_color) + 
		    ggplot2::labs(title = main_title, subtitle = sub_title, y = this_y_label,
			              x = this_x_label)
		
		
	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		this_plot <- this_plot + ggplot2::theme_bw()
    }

	# remove grid lines if \code{do_grid = FALSE}
    if (!do_grid) {
	    this_plot <- this_plot + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
    }
	
	if (reset_facet_y_axis) {
		if (do_facet) {
			if (is.null(y_limit)) {
				y_limit <- range(this_matrix)
			}
			this_plot <- this_plot + ggplot2::ylim(y_limit)
		}
	}

	return(this_plot)

}
