#' Seasonal factor mean plot using ggplot
#'
#' Generates a plot of the means of the seasonal factors
#' 
#' Version 2.7, 9/5/2024
#'
#' @param this_sf_matrix time series object of the seasonal factors from a seasonal adjustment
#' @param main_title Character string; main title of plot.  Default is  \code{'Mean of Seasonal Factors'}.
#' @param sub_title Character string; subtitle of plot. There is no default subtitle.
#' @param this_y_label Character string; y-axis label for plot, if specified.
#' @param this_x_label Label for X axis. Default is \code{"Time"}.
#' @param do_grid Logical scalar; indicates if plots will have grid lines. 
#'        Default is no grid lines.
#' @param do_background Logical scalar; indicates grey background included in plot.
#'        Default is no grey background;
#' @param line_color Character scalar; color used for plot. User should specify one 
#'        color for each column of the matrix specified.
#'        Default is the \code{RColorBrewer} palette \code{"Dark2"}.
#' @param this_palette Character string; default \code{RColorBrewer} palette
#'        Deault is \code{"Dark2"}.
#' @param this_line_type Character string; indicates line type of each plot produced. 
#'        Default is \code{rep("solid", ncol(this_sf_matrix))}.
#' @param do_facet Logical scalar; indicates if a facet plot is generated of the different colums. 
#'        Default is \code{FALSE}.
#' @param reset_facet_y_axis Logical scalar; indicates if y-axis for facet plots reset to \code{y_limit}
#'        Default is \code{FALSE}.
#' @param y_limit Numeric vector of length 2; Range of values you wish the plot to be plotted over. 
#'        Default is range of the seasonal factors.
#' @param this_legend_title Character string; indicates title of legend. Default is \code{'Series'}.
#' @param forecast Integer scalar; Number of forecasts appended to the seasonal factors. Default is 0.
#' @param this_legend_entry Character array; entries for the legend. When \code{do_facet = TRUE}, 
#'        entries are used as the facet labels.
#'        Default is \code{colnames(this_sf_matrix)}
#' @return Generate plot of the means of seasonal factors by period.
#'         If seasonal factors not specified, print out error message and return NULL.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' EM_individual_seas <-
#'  seasonal::seas(
#'     x11 = "", transform.function = "log",
#'     check.print = c("none", "+acf", "+acfplot", "+normalitytest"),
#'     regression.aictest = NULL,
#'     outlier.types = "all",
#'     arima.model = "(0 1 1)(0 1 1)",
#'     forecast.maxlead = 60,
#'     list = list(
#'         list(x = employment_list$n2000013),
#'         list(x = employment_list$n2000014),
#'         list(x = employment_list$n2000025),
#'         list(x = employment_list$n2000026)
#'     )
#' )
#' EM_names <- names(employment_list)
#' # Use Filter function to grab seas objects
#' EM_individual_seas_only <-
#'   Filter(function(x) inherits(x, "seas"), EM_individual_seas)
#' 
#' names(EM_individual_seas_only) <- EM_names
#' 
#' EM_Comp_Sf <-
#'  cbind(seasonal::series(EM_individual_seas_only$n2000013, "d10"),
#'       seasonal::series(EM_individual_seas_only$n2000014, "d10"),
#'        seasonal::series(EM_individual_seas_only$n2000025, "d10"),
#'        seasonal::series(EM_individual_seas_only$n2000026, "d10"))
#' colnames(EM_Comp_Sf) <- EM_names
#' 
#' em_plot <- blsplotGG::plot_sf_mean(EM_Comp_Sf,
#'   main_title = 'US Employment Seasonal Means',
#'   sub_title = 'X-11 Seasonals',
#'   forecast = 60, 
#'   this_legend_title = "SF Means",
#'   this_legend_entry = c("M 16-19", "F 16-19", "M 20+", "F 20+")
#'   )
#' 
#' em_plot_facet <- blsplotGG::plot_sf_mean(EM_Comp_Sf,
#'   main_title = 'US Employment Seasonal Means',
#'   sub_title = 'X-11 Seasonals',
#'   forecast = 60, 
#'   do_facet = TRUE, 
#'   reset_facet_y_axis = TRUE,
#'   this_legend_entry = c("M 16-19", "F 16-19", "M 20+", "F 20+")
#'   )
#' @import graphics
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
			 reset_facet_y_axis = FALSE, 
			 y_limit = NULL, 
			 forecast = 0, 
			 this_legend_title = "SF Means", 
			 this_legend_entry = colnames(this_sf_matrix)) {
    # Author: Brian C. Monsell (OEUS) Version 2.7, 9/5/2024

    if (is.null(this_sf_matrix)) {
        stop("Argument this_sf_matrix must be specified.")
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
		stop("Seasonal factors should be all multiplicative or all additive")
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
		ggplot2::scale_colour_manual(values = line_color) +
		ggplot2::labs(title = main_title, 
		              subtitle = sub_title, 
					  y = this_y_label) + 
		ggplot2::scale_colour_discrete(name = this_legend_title, 
		                               labels = this_legend_entry)

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

	# remove grid lines if \code{do_grid = FALSE}
    if (!do_grid) {
	    this_plot <- this_plot + 
		    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
			               panel.grid.minor = ggplot2::element_blank())
    }

	# remove grey background if \code{do_background = FALSE} 
    if (!do_background) {
		this_plot <- this_plot + ggplot2::theme_bw()
    }

	if (do_facet) {
		if (reset_facet_y_axis) {
			if (is.null(y_limit)) {
				y_limit <- range(this_sf_matrix)
			}
			this_plot <- this_plot + ggplot2::ylim(y_limit)
		}
		legend_factor <- factor(this_legend_entry)
		this_plot <- 
			this_plot + ggplot2::facet_grid(legend_factor[name] ~ .) + 
						ggplot2::theme(legend.position = "none")
	}

	return(this_plot)

}