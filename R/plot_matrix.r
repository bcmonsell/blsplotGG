#' Plot time series matrix
#'
#' Generate plot of a matrix of user-specified time series.
#' 
#' Version 8.2, 2026-11-02
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
#'        Default is the \code{RColorBrewer} palette \code{"Dark2"}. For a listing of R color codes, 
#'        see \insertCite{rchartsColors;textual}{blsplotGG}.
#' @param this_palette Character string; default \code{RColorBrewer} palette \insertCite{RColorBrewer}{blsplotGG}.
#'        Deault is \code{"Dark2"}.
#' @param this_line_type Character string; indicates line type of each plot produced. 
#'        Default is \code{"solid"}.
#' @param this_legend_title Title for legend.  Default is \code{"Forecast"}
#' @param this_legend_title_size integer scalar; Size of the legend title.
#'        Must be greater than zero.
#'        Default is \code{12}.
#' @param this_legend_text_size integer scalar; Size of the legend title.
#'        Must be greater than zero.
#'        Default is \code{10}.
#' @param this_legend_position Character string; indicates the position of the plot legend.
#'        Possible values are \code{"right"}, \code{"left"}, \code{"top"}, 
#'        \code{"bottom"}, and \code{"none"}
#'        Default is \code{"right"}.
#' @param this_legend_space integer scalar; Space between legend and plot in points.
#'        Must be greater than or equal to zero.
#'        Default is \code{10}.
#' @param do_facet Logical scalar; indicates if a facet plot is generated of the 
#'        different colums. Default is \code{FALSE}.
#' @param facet_scale Character scalar; set the scale for the y-axis of the facet plots. 
#'        Default is \code{"fixed"}.  To set axis to be different for the different plots,
#'        set \code{facet_scale = "free_y"}.
#' @param facet_position Character scalar; set position of the facet lables. Possible values
#'        are \code{"top"}, \code{"bottom"}, \code{"left"}, \code{"right"}.
#'        Default is \code{"right"}.
#' @param facet_text_size Numeric scalar; size of text in facet labels. 
#'        Default is set by the \code{ggplot2} package.
#' @param facet_column Numeric scalar; number of columns in the facet plot.
#'        Default is \code{1}.
#' @return A \code{ggplot} object that produces a plot of user-specified time series
#'         in columns of \code{this_matrix}, either all on the same axis or as a facet plot. 
#'         This function uses the \code{tsibble} package \insertCite{tsibble}{blsplotGG}.
#'         If matrix not specified, print out error message and return \code{NULL}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' UN_Matrix <-
#' 	  cbind(blsplotGG::unemployment_list$n3000013, 
#' 	     blsplotGG::unemployment_list$n3000014, 
#' 	     blsplotGG::unemployment_list$n3000025, 
#' 		 blsplotGG::unemployment_list$n3000026)
#' UN_names <- c("M 16-19", "F 16-19", "M 20+", "F 20+")
#' colnames(UN_Matrix) <- UN_names
#' p_UN <- plot_matrix(UN_Matrix, 
#'    this_x_label = NULL, 
#'    main_title = "US Unemployment", 
#'    this_palette = "Set2",
#'    do_grid = TRUE, 
#'    this_line_type = rep("solid", 4),
#'    this_legend_title = "UNemp",
#'    this_legend_title_size = 8,
#'    this_legend_text_size = 6,
#'    this_legend_position = "bottom",
#'    this_legend_space = 1)
#' p_UN_facet <- plot_matrix(UN_Matrix, 
#'    this_y_label = NULL, this_x_label = NULL, 
#'    main_title = "US Unemployment",
#'    do_grid = FALSE,
#'    do_facet = TRUE, 
#'    facet_text_size = 6,
#'    facet_column = 2, 
#'    facet_scale = "free_y",
#'    facet_position = "top")
#' @importFrom rlang .data
#' @importFrom Rdpack reprompt
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
			 this_legend_title = "Series",
			 this_legend_title_size = 12,
			 this_legend_text_size = 10,
			 this_legend_position = "right",
			 this_legend_space = 10, 
			 do_facet = FALSE, 
			 facet_scale = "fixed",
			 facet_position = "right",
			 facet_text_size = NULL,
			 facet_column = 1) {
    # Author: Brian C. Monsell (OEUS) Version 8.2, 2026-11-02

    if (is.null(this_matrix)) {
        cat("Argument this_matrix must be specified.")
        return(NULL)
    }
	
	if (!is.mts(this_matrix)) {
        cat("Argument this_matrix must be an mts object.")
        return(NULL)
	}

	# check legend options 
	position_all <- c("top", "bottom", "left", "right")
	if (sum(match(position_all, this_legend_position), na.rm = TRUE) == 0) {
        cat(paste("Legend position choice not valid:", this_legend_position))
		return(NULL)
	}
	
    # check if values of \code{this_legend_text_size}, \code{this_legend_title_size}, 
	# and \code{this_legend_space} are valid, return \code{NULL} if not.
	if (this_legend_text_size <= 0) {
        cat(paste("Legend text size not valid (must be greater than zero):", 
		           this_legend_text_size))
        return(NULL)
	}
	
	if (this_legend_title_size <= 0) {
        cat(paste("Legend title text size not valid (must be greater than zero):", 
		           this_legend_title_size))
        return(NULL)
	}
	
	if (this_legend_space < 0) {
        cat(paste("Legend space size not valid (must be greater than or equal to zero):", 
		           this_legend_space))
        return(NULL)
	}
	
    num_series <- ncol(this_matrix)

    if (length(this_line_type) < num_series) {
	    if (!do_facet) {
            warning("Number of line types specified less than number of series.")
            warning("Remaining elements set to be the first entry.")
			this_line_type <- update_vector(this_line_type, num_series)
		}
		if (is.null(this_line_type)) { return(NULL) }
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
	
	if (is.null(this_line_type)) {
		this_line_type <- rep("solid", ncol(this_matrix))
	} else {
		if (length(this_line_type) < ncol(this_matrix)) {
			x <- this_line_type[1]
			this_line_type <- rep(x, ncol(this_matrix))
		}
	}
	
	if (is.null(colnames(this_matrix))) {
		colnames(this_matrix) <- paste0("s", 1:ncol(this_matrix))
	}
	matrix_df <- tsibble::as_tsibble(this_matrix, pivot_longer = TRUE)
	
	if (do_facet) {
		this_plot <-
			ggplot2::ggplot(matrix_df, 
				ggplot2::aes(x = as.Date(.data$index), y = .data$value)) +
				ggplot2::geom_line() + 
				ggplot2::facet_wrap(~ .data$key, scale = facet_scale, 
					strip.position = facet_position, ncol = facet_column)
    } else {
		this_plot <-
			ggplot2::ggplot(matrix_df, 
				ggplot2::aes(x = as.Date(.data$index), y = .data$value, color = .data$key)) +
				ggplot2::geom_line()
	}
	
    this_plot <- this_plot +
		ggplot2::scale_linetype_manual(values = this_line_type) +
		ggplot2::scale_colour_manual(values = line_color) + 
		ggplot2::labs(title = main_title, subtitle = sub_title, y = this_y_label, x = this_x_label)
		
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
	
	if (do_facet) {
		if (!is.null(facet_text_size)) {
			this_plot <- this_plot + 
				ggplot2::theme(strip.text = ggplot2::element_text(size = facet_text_size))
		}
	} else {			
		this_plot <- 
			this_plot + ggplot2::guides(colour = ggplot2::guide_legend(this_legend_title)) +
				ggplot2::theme(legend.text = ggplot2::element_text(size = this_legend_text_size),
					   legend.title = ggplot2::element_text(size = this_legend_title_size),
					   legend.position = this_legend_position,
					   legend.box.spacing = ggplot2::unit(this_legend_space, "pt"))
	}

	return(this_plot)

}
