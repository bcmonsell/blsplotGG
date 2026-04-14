#' Color-blind friendly color palette
#'
#' Color palettes that can be used that can be distinguished by color-blind people 
#' (either from \insertCite{RColorBrewer;textual}{blsplotGG}
#' or \insertCite{cookbookrColorsggplot2;textual}{blsplotGG}).
#'
#' Version 3.2, 2/10/2026
#'
#' @param with_grey Logical scalar; whether color blind pallate contains \code{'grey'}, 
#'        otherwise the palette contains \code{black}. Default is TRUE. 
#' @param brewer_palette Character string; a \code{RColorBrewer} palette \insertCite{RColorBrewer}{blsplotGG}.
#'        There is no default - must be a color-blind friendly palette.
#' @return Vector of hexadecimal color codes that form a color palette that can be distinguished by color-blind people.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' this_color_blind <- color_blind_palette(FALSE, brewer_palette = "Dark2")
#' @importFrom Rdpack reprompt
#' @export
color_blind_palette <- function(with_grey = TRUE, brewer_palette = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 3.2, 2/10/2026
	
	# if \code{brewer_palette} is specified, return \code{RColorBrewer} palette if it is color-blind friendly
	if (!is.null(brewer_palette)) {
	    brewer_color_blind <- rownames(RColorBrewer::brewer.pal.info)[RColorBrewer::brewer.pal.info[,3]]
		if (sum(brewer_palette %in% brewer_color_blind) > 0) {
			this_palette_max <- RColorBrewer::brewer.pal.info[brewer_palette, "maxcolors"]
			if (with_grey) {
				return(c("#999999", RColorBrewer::brewer.pal(this_palette_max, brewer_palette)))
			} else {
				return(RColorBrewer::brewer.pal(this_palette_max, brewer_palette))
			}
		} else {
			warning(paste0("RColorBrewer palette ", brewer_palette, " not color-blind friendly."))
			warning("Will return a color-blind friendly palette from Cookbook for R.")
		}
	}
	
	# if a \code{RColorBrewer} palette is not specified, use palette from Cookbook for R - Colors (ggplot2)
    
    # Based on value of \code{with_grey}, return a version of a color blind palette
    if (with_grey) {
        # The palette with grey:
        return(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
    } else {
        # The palette with black:
        return(c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
    }
}
	       