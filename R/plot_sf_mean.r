#' Seasonal factor mean plot using ggplot
#'
#' Generates a plot of the means of a matrix of seasonal factors. 
#' Each column of the matrix should be a \code{ts} object.
#'
#' Version 6.1, 2026-10-30
#'
#' @param this_sf_matrix time series matrix (mts) object of the seasonal factors from 
#'        a number of seasonal adjustments.
#'        This is a required entry.
#' @param main_title Character string; main title of plot.  
#'        Default is  \code{"Mean of Seasonal Factors"}.
#' @param sub_title Character string; subtitle of plot. There is no default subtitle.
#' @param this_y_label Character string; y-axis label for plot, if specified.
#' @param this_x_label Label for X axis. Default is \code{"Time"}.
#' @param do_grid Logical scalar; indicates if plots will have grid lines. 
#'        Default is no grid lines.
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param line_color Character scalar; color used for plot. User should specify one 
#'        color for each column of the matrix specified. For a listing of R color codes, 
#'        see \insertCite{rchartsColors;textual}{blsplotGG}.
#'        Default is the \code{RColorBrewer} palette \code{"Dark2"} \insertCite{RColorBrewer}{blsplotGG}. 
#' @param this_palette Character string; default \code{RColorBrewer} palette 
#'        Deault is \code{"Dark2"}.
#' @param this_line_type Character string; indicates line type of each plot produced. 
#'        Default is \code{rep("solid", ncol(this_sf_matrix))}.
#' @param do_facet Logical scalar; indicates if a facet plot is generated of the different colums. 
#'        Default is \code{FALSE}.
#' @param facet_scale Character scalar; set the scale for the y-axis of the facet plots. 
#'        Default is \code{"fixed"}.  To set axis to be different for the different plots,
#'        set \code{facet_scale = "free_y"}.
#' @param facet_text_size Numeric scalar; size of text in facet labels. 
#'        Default is set by the \code{ggplot2} package.
#' @param y_limit Numeric vector of length 2; Range of values you wish the plot to be plotted over. 
#'        Default is range of the seasonal factors.
#' @param forecast Integer scalar; Number of forecasts appended to the seasonal factors. 
#'        Default is \code{0}.
#' @param this_legend_entry Character array; entries for the legend. When \code{do_facet = TRUE}, 
#'        entries are used as the facet labels.
#'        Default is \code{colnames(this_sf_matrix)}
#' @param this_legend_title Character string; indicates title of legend. Default is \code{'Series'}.
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
#' @return Generate plot of the means of seasonal factors by period.
#'         If seasonal factors not specified, print out error message and return \code{NULL}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' EM_individual_seas <-
#'  seasonal::seas(employment_list,
#'     x11 = "", transform.function = "log",
#'     check.print = c("none", "+acf", "+acfplot", "+normalitytest"),
#'     regression.aictest = NULL,
#'     outlier.types = "all",
#'     arima.model = "(0 1 1)(0 1 1)",
#'     forecast.maxlead = 60,
#' )
#' # Use Filter function to grab seas objects
#' EM_individual_seas_only <-
#'   Filter(function(x) inherits(x, "seas"), EM_individual_seas)
#' 
#' EM_names <- names(employment_list)
#' names(EM_individual_seas_only) <- EM_names
#' 
#' EM_Comp_Sf <-
#'  cbind(seasonal::series(EM_individual_seas_only$n2000013, "d10"),
#'        seasonal::series(EM_individual_seas_only$n2000014, "d10"),
#'        seasonal::series(EM_individual_seas_only$n2000025, "d10"),
#'        seasonal::series(EM_individual_seas_only$n2000026, "d10"))
#' colnames(EM_Comp_Sf) <- EM_names
#' 
#' em_plot <- plot_sf_mean(EM_Comp_Sf,
#'   main_title = 'US Employment Seasonal Means',
#'   sub_title = 'X-11 Seasonals',
#'   forecast = 60, 
#'   this_legend_title = "SF Means",
#'   this_legend_entry = c("M 16-19", "F 16-19", "M 20+", "F 20+")
#'   )
#' 
#' em_plot_facet <- plot_sf_mean(EM_Comp_Sf,
#'   main_title = 'US Employment Seasonal Means',
#'   sub_title = 'X-11 Seasonals',
#'   forecast = 60, 
#'   do_facet = TRUE, 
#'   facet_scale = "fixed",
#'   this_legend_entry = c("M 16-19", "F 16-19", "M 20+", "F 20+")
#'   )
#' @import graphics
#' @importFrom Rdpack reprompt
#' @export
plot_sf_mean <- 
    function(this_sf_matrix = NULL, 
	    main_title = deparse(substitute(this_sf_matrix)), 
        sub_title = NULL, 
		this_y_label = NULL, 
		this_x_label = "Time", 
	    do_grid = FALSE, 
		do_background = FALSE, 
		line_color = NULL, 
		this_palette = "Dark2", 
		this_line_type = rep("solid", ncol(this_sf_matrix)), 
		do_facet = FALSE, 
		facet_scale = "fixed",
		facet_text_size = NULL,
		y_limit = NULL, 
		forecast = 0, 
		this_legend_entry = colnames(this_sf_matrix),
		this_legend_title = "SF Means",
		this_legend_title_size = 12,
		this_legend_text_size = 10,
		this_legend_position = "right",
		this_legend_space = 10) {
    # Author: Brian C. Monsell (OEUS) Version 6.1, 2026-10-30

    if (is.null(this_sf_matrix)) {
        cat("Argument this_sf_matrix must be specified.")
		return(NULL)
    }
	
	if (!is.mts(this_sf_matrix)) {
        cat("Argument this_sf_matrix must be a time series matrix (mts) object.")
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
	
	if (forecast > 0) {
		n_row <- nrow(this_sf_matrix)
		n_data <- n_row - forecast
		this_f <- frequency(this_sf_matrix)
		this_sf_matrix <- 
		    ts(this_sf_matrix[1:n_data,], 
			   start = start(this_sf_matrix), 
			   frequency = this_f)
	}

    num_series <- ncol(this_sf_matrix)
	
	check_mode_vec <- vector("logical", length = num_series)
	for (i in 1:num_series) {
		check_mode_vec[i] <- sum(this_sf_matrix[,i] < 0.0) > 0
	}
	this_sum <- sum(check_mode_vec)
	if (this_sum > 0 && this_sum < num_series) {
		cat("Seasonal factors should be all multiplicative or all additive")
		return(NULL)
	}

	# check legend options 
	position_all <- c("top", "bottom", "left", "right")
	if (sum(match(position_all, this_legend_position), na.rm = TRUE) == 0) {
        cat(paste("Legend position choice not valid:", this_legend_position))
		return(NULL)
	}
	
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
	
	this_cycle <- cycle(this_sf_matrix)
	this_freq  <- max(this_cycle)
	this_mean_matrix <- matrix(0.0, ncol = num_series, nrow = this_freq)
	this_name  <- colnames(this_sf_matrix)
	this_name_vec <- vector("character", length = this_freq * num_series)
	
	k <- 0
	for (i in 1:num_series) {
		this_series <- this_sf_matrix[,i]
		
		for (j in 1:this_freq) {
			this_mean_matrix[j,i] <- mean(this_series[this_cycle == j])
			k <- k + 1
			this_name_vec[k] <- this_name[i]
		}
	}
	
    if (length(this_line_type) < num_series) {
	    if (!do_facet) {
            warning("Number of line types specified less than number of series.")
            warning("Remaining elements set to be the first entry.")
		}
        this_line_type <- update_vector(this_line_type, num_series)
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

	this_df <-
	    data.frame(mean = as.vector(this_mean_matrix),
		           cycle = rep(1:this_freq, num_series),
				   name = as.factor(this_name_vec))
				   
	this_plot <- ggplot2::ggplot(data = this_df, 
                          ggplot2::aes(x = as.factor(.data$cycle), 
                                       y = .data$mean, 
                                       group = .data$name, 
                                       colour = .data$name)) + 
		ggplot2::geom_line() +
		ggplot2::geom_point() +
		ggplot2::scale_colour_manual(values = line_color, 
		                             labels = this_legend_entry) +
		ggplot2::labs(title = main_title, 
		              subtitle = sub_title, 
					  y = this_y_label,
					  colour = this_legend_title)

# generate x-axis
	if (this_freq == 12) {
		this_plot <- this_plot + 
			ggplot2::scale_x_discrete(name = this_x_label, 
                                      breaks = 1:12, 
                                      labels = month.abb)
	} else {
		if (this_freq == 4) {
			this_q_label <- paste0("Q", 1:4)
			this_plot <- this_plot + 
				ggplot2::scale_x_discrete(name = this_x_label, 
									      breaks = 1:4, 
                                          labels = this_q_label)
		} else {
			this_p_label <- paste0("P", 1:this_freq)
			this_plot <- this_plot + 
				ggplot2::scale_x_discrete(name = this_x_label, 
									      breaks = 1:this_freq, 
                                          labels = this_p_label)
		}
	}

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
		legend_factor <- factor(this_legend_entry)
		this_plot <- 
			this_plot + ggplot2::facet_grid(legend_factor[name] ~ ., 
											scale = facet_scale) + 
						ggplot2::theme(legend.position = "none")
		if (!is.null(facet_text_size)) {
			this_plot <- this_plot + 
				ggplot2::theme(strip.text = ggplot2::element_text(size = facet_text_size))
		}
	} else {
		this_plot <- this_plot +
			ggplot2::theme(legend.text = ggplot2::element_text(size = this_legend_text_size),
					   legend.title = ggplot2::element_text(size = this_legend_title_size),
					   legend.position = this_legend_position,
					   legend.box.spacing = ggplot2::unit(this_legend_space, "pt"))
	}

	return(this_plot)

}