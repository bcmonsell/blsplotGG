#' Return color-blind friendly palettes
#'
#' Returns the names of color palettes from the \code{RColorBrewer} package that can be distinguished by color-blind people.
#'
#' Version 1.3, 3/6/2024
#'
#' @param this_category Character string; specify which catagory of color palette will be returned.
#'               Possible choices are \code{"div"} (diverging), \code{"qual"} (qualitative), \code{"seq"}.
#'               If not specified, all color palettes are returned
#' @return Vector of color palette names from the \code{RColorBrewer} package that can be distinguished by color-blind people.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{monsell.brian@@gmail.com}
#'
#' @examples
#' qual_color_blind_palettes <- display_color_blind_palettes("qual")
#' @export
display_color_blind_palettes <- function(this_category = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 1.3, 3/6/2024

	this_color_blind_index <- RColorBrewer::brewer.pal.info[,3]
	if (is.null(this_category)) {
		return(rownames(RColorBrewer::brewer.pal.info)[this_color_blind_index])
	}
	
	this_category_index <- RColorBrewer::brewer.pal.info[,2] %in% this_category
	this_index <- this_color_blind_index & this_category_index
	if (sum(this_index) > 0) {
		return(rownames(RColorBrewer::brewer.pal.info)[this_index])
	} else {
		warning(paste0("Improper category name: ", this_category))
		stop("Categories permitted: 'div', 'qual', 'seq'")
	}
}
