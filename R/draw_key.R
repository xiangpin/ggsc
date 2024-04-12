#' Key drawing functions
#'
#' Each Geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These are the options built into ggplot2.
#'
#' @param data A single row data frame containing the scaled aesthetics to
#'      display in this key
#' @param params A list of additional parameters supplied to the geom.
#' @param size Width and height of key in mm.
#' @return A grid grob.
#' @name draw_key_scattermore2
#' @export
#' @importFrom scales alpha
#' @importFrom ggplot2 draw_key_point
#' @importFrom grid grobTree pointsGrob
draw_key_scattermore2 <- function(data, params, size){
    pointkey <- draw_key_point(data, params, size)
    if (is.null(data$bg_colour)){
        return (pointkey)
    }
    
    stroke_size <- data$stroke %||% 0.5
    stroke_size[is.na(stroke_size)] <- 0

    gp <- gpar(col = data$bg_colour, 
               fontsize = (data$size %||% 1.5) * .pt + stroke_size * .stroke / 2,
               lwd = (data$stroke %||% 0.5) * 4)
    grobTree(pointkey, pointsGrob(0.5, 0.5, pch = 21, gp = gp))
}

.pt <- 2.845276
.stroke <- 3.779528

`%||%` <- function (a, b){
    if (!is.null(a))
        a
    else b
}

rd_aesthetics <- getFromNamespace("rd_aesthetics", "ggplot2")
