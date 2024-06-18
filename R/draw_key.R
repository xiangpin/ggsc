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
#' @name draw_key_bgpoint
#' @export
#' @importFrom scales alpha
#' @importFrom ggplot2 draw_key_point
#' @importFrom grid grobTree pointsGrob
#' @importFrom ggplot2 fill_alpha
#' @export
draw_key_bgpoint <- function(data, params, size){
  if (is.null(data$shape)) {
    data$shape <- 19
  } else if (is.character(data$shape)) {
    data$shape <- translate_shape_string(data$shape)
  }

  # NULL means the default stroke size, and NA means no stroke.
  stroke_size <- data$stroke %||% 0.5
  stroke_size[is.na(stroke_size)] <- 0
  cpointsGrob(0.5, 0.5,
    pch = data$shape,
    bg_colour = data$bg_colour,
    gap_colour = alpha(params$gap_colour %||% "black", params$gap_alpha),
    bg_line_width = params$bg_line_width,
    gap_line_width = params$gap_line_width,
    gp = gpar(
      col = alpha(data$colour %||% "black", data$alpha),
      fill = fill_alpha(data$fill %||% "black", data$alpha),
      fontsize = (data$size %||% 1.5) * .pt + stroke_size * .stroke / 2,
      lwd = stroke_size * .stroke / 2
    )
  )    
}

.pt <- 2.845276
.stroke <- 3.779528

`%||%` <- function (a, b){
    if (!is.null(a))
        a
    else b
}

rd_aesthetics <- getFromNamespace("rd_aesthetics", "ggplot2")
