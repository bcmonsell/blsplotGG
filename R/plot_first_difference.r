#' First Difference Plot
#'
#' Generates a ggplot2 object of the first difference of a time series grouped
#' by months or quarters.
#'
#' Version 1.1, 8/22/2024
#'
#' @param this_series \code{ts} object of a single time series
#'        This is a required entry.
#' @param take_log Logical scalar, specifies that a log transformation will be 
#'        taken before differencing. Default is \code{FALSE} 
#' @param main_title Title for the plot. 
#'        Default is character string \code{'First Difference Plot'}.
#' @param remove_y_axis Logical scalar. 
#'        If \code{TRUE}, removes the y-axis labels and tick marks from all subplots. 
#'        Default is \code{FALSE}, which keeps the y-axis labels and tick marks.
#' @param x_title_size Integer scalar, size of the x-axis title. 
#'        Default is \code{10}.
#' @param geom_text_size Integer scalar, size of the plotting characters. 
#'        Default is \code{2.5}.
#' @param geom_text_color Character scalar, color of the plotting characters. 
#'        Default is \code{"steelblue"}.
#' @return A \code{ggplot} object that produces a plot of the first differences
#'         of a series specified by \code{this_series} grouped by month or quarter.
#'         The time series specified should be either a monthly or quarterly series.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' shoes_seas <-
#'     seasonal::seas(shoes2008, x11.save = "d13",
#'        x11 = "", transform.function = "log",
#'        check.print = c("none", "+acf", "+acfplot", "+normalitytest"),
#'        regression.aictest = c("td", "easter"),
#'		  regression.save = c("td", "hol"),
#'        outlier.types = "all",
#'        arima.model = "(0 1 1)(0 1 1)",
#'        forecast.maxlead = 60)
#' shoesSA <- seasonal::final(shoes_seas)
#' p_shoes_sa_diff <- 
#'     plot_first_difference(shoesSA, 
#'         main_title = "Seasonal Adjustment Change History Graph",
#'         remove_y_axis = FALSE, x_title_size = 12)
#' @importFrom rlang .data
#' @export
plot_first_difference <- function(
	this_series = NULL,
	take_log = FALSE,
    main_title = "First Difference Plot",
	remove_y_axis = TRUE,
	x_title_size = 10,
	geom_text_size = 2.5,
	geom_text_color = "steelblue") {
	# Author: Brian C. Monsell (OEUS) Version 1.1, 8/22/2024
	
    # check if a value is specified for \code{this_series}
    if (is.null(this_series)) {
        cat("must specify a ts object")
        return(this_series)
    }
	
    # check if \code{this_series} is a ts object
    if (!is.ts(this_series)) {
        cat("must specify a ts object")
		return(NULL)
    }
	
	# check if \code(this_series} is either monthly or quarterly
	this_freq <- frequency(this_series)
	if (!(this_freq == 12 | this_freq == 4)) {
        cat("must specify either a monthly or quarterly series")
		return(NULL)
	}
	
	if (take_log) this_series <- log(this_series)

	this_diff <- diff(this_series)
	this_diff_range <- range(this_diff)
	start_diff <- start(this_diff)
	end_diff   <- end(this_diff)

	if (start_diff[2] > 1) {
		this_diff <- c(rep(NA, start_diff[2] - 1), this_diff)
	}

	if (end_diff[2] < this_freq) {
		this_diff <- c(this_diff , rep(NA, this_freq - end_diff[2]))
	}
	
	this_diff <- 
		ts(this_diff, start = c(start_diff[1], 1), frequency = this_freq)
	this_diff_year_matrix <- 
		matrix(substr(as.character(time(this_diff) %/% 1), 4,4), 
			ncol = this_freq, byrow = TRUE)
	this_diff_matrix <- 
		matrix(this_diff, ncol = this_freq, byrow = TRUE)

	this_diff_plot_list <- list()
	npoints <- nrow(this_diff_matrix)

	if (this_freq == 12) {
		this_x_label <- month.abb
	} else {
		this_x_label <- paste0("q", 1:4)
	}

	for (i in 1:this_freq) {
		this_index <- !is.na(this_diff_matrix[,i])
		this_df <- data.frame(
			X  = seq(1,npoints)[this_index], 
			Y  = this_diff_matrix[this_index,i], 
			yr = this_diff_year_matrix[this_index,i])
  
		this_p <- 
			ggplot2::ggplot(this_df, 
				ggplot2::aes(x = .data$X, y = .data$Y, label = .data$yr)) +
			ggplot2::geom_text(size = geom_text_size, color=geom_text_color) +
			ggplot2::labs(title = NULL,
                  subtitle = NULL,
                  x = this_x_label[i],
                  y = NULL) +
			ggplot2::ylim(this_diff_range) +
			ggplot2::xlim(0, npoints) +
			ggplot2::theme(
				axis.text.x  = ggplot2::element_blank(), #remove x axis labels
				axis.ticks.x = ggplot2::element_blank(), #remove x axis ticks
				axis.title.x = ggplot2::element_text(size = x_title_size)) 
  
		if (remove_y_axis) {
			this_p <- this_p + 
				ggplot2::theme(
					axis.text.y  = ggplot2::element_blank(), #remove y axis labels
					axis.ticks.y = ggplot2::element_blank()) #remove y axis ticks
		}
  
		this_key <- paste0("p",i)
  
		this_diff_plot_list[[this_key]] <- this_p
	}

	if (this_freq == 12) {
		p_final <- 
			ggpubr::ggarrange(plotlist = this_diff_plot_list, nrow = 3, ncol = 4)
	} else {
		p_final <- 
			ggpubr::ggarrange(plotlist = this_diff_plot_list, nrow = 2, ncol = 2)
	}

	if (!is.null(main_title)) {
		p_final <- 
			ggpubr::annotate_figure(p_final, top = ggpubr::text_grob(main_title))
	}
	
	return(p_final)

}