##' @title sc_geom_point
##' @rdname sc-geom-point
##' @param mapping aesthetic mapping
##' @param ... additional parameters pass to 'scattermore::geom_scattermore()'
##' @return layer of points
##' @importFrom scattermore geom_scattermore
##' @seealso [sc_dim()] and [sc_feature()]
##' @export
##' @examples
##' library(ggplot2)
##' ggplot(iris,
##'  aes(x= Sepal.Length, y = Petal.Width, color=Species)
##' ) + 
##' sc_geom_point()
sc_geom_point <- function(mapping=NULL, ...){
    default_params <- list(mapping = mapping, 
                        pointsize = 4,
                        pixels = c(700, 700)
                    )
    params <- modifyList(default_params, list(...))
    do.call(geom_scattermore2, params)
}

#' @title geom_scattermore2
#' @description
#' this add the background colour for the \code{\link[scattermore]{geom_scattermore}} 
#' @eval rd_aesthetics("geom", "scattermore2")
#' @inheritParams ggplot2::layer
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'     a warning. If `TRUE`, missing values are silently removed.
#' @param interpolate A logical value indicating whether to linearly interpolate
#' the image (the alternative is to use nearest-neighbour interpolation, 
#' which gives a more blocky result). Default \code{FALSE}, 
#' passed to \code{\link[grid]{rasterGrob}}.
#' @param pointsize Radius of rasterized point. Use ‘0’ for single pixels (fastest).
#' @param pixels Vector with X and Y resolution of the raster, default \code{c(512,512)}.
#' @param gap_colour colour between the background and top point point layer,
#' default is \code{white}. 
#' @param bg_line_width numeric the line width of background point layer, 
#' default is \code{0.3}.
#' @param gap_line_width numeric the line width of gap between the background and 
#' top point point layer, default is \code{.05}.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}}.
#' @details
#'  \itemize{
#'     \item \code{colour} the colour of point, default is \code{black}.
#'     \item \code{bg_colour} the colour of background point, default is \code{NA}.
#'     \item \code{alpha} the transparency of colour, default is 1.
#'     \item \code{subset} subset the data frame which meet conditions to display.
#'  }
#' @return polygonal point layer
#' @importFrom ggplot2 layer
#' @author Shuangbin Xu
#' @export
#' @examples
##' library(ggplot2)
##' ggplot(iris,
##'  aes(x= Sepal.Length, y = Petal.Width, color=Species, bg_colour=Species)
##' ) + 
##' geom_scattermore2(pointsize=4, gap_line_width = .1, bg_line_width = .3)
geom_scattermore2 <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                             interpolate = FALSE, pointsize = 0, pixels = c(512, 512),
                             gap_colour = 'white', bg_line_width = .3, gap_line_width = .05){
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    geom = GeomScattermore2,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      interpolate = interpolate,
      pointsize = pointsize,
      pixels = pixels,
      gap_colour = gap_colour,
      bg_line_width = bg_line_width,
      gap_line_width = gap_line_width,
      ...
    )
  )
}


#' @importFrom scattermore scattermore
GeomScattermore2 <- ggplot2::ggproto("GeomScattermore2", ggplot2::Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("alpha", "colour"),
  optional_aes = c("subset"),
  default_aes = ggplot2::aes(
    shape = 19, colour = "black", size = 1.5, fill = NA,
    alpha = 1, stroke = 0.5, bg_colour = NA
  ),
  setup_data = function(data, params){
      if (is.null(data$subset))
          return(data)
      data[which(data$subset),]
  },                                     
  draw_panel = function(data, pp, coord,
                        pointsize = 0,
                        interpolate = FALSE,
                        na.rm = FALSE,
                        pixels = c(512, 512),
                        gap_colour = 'white',
                        bg_line_width = .3,
                        gap_line_width = .05){
    coords <- coord$transform(data, pp)

    upperimage <- scattermore(cbind(coords$x, coords$y),
                              rgba = grDevices::col2rgb(alpha = TRUE, scales::alpha(coords$colour, coords$alpha)),
                              cex = pointsize,
                              xlim = c(0, 1),
                              ylim = c(0, 1),
                              size = pixels)

    if (!all(is.null(coords$bg_colour)) || !all(is.na(coords$bg_colour))){
        tmpsize <- sqrt(pointsize)
        gapsize <- (tmpsize + tmpsize * gap_line_width * 2)^2
        bgsize <- gapsize + (sqrt(bg_line_width) + tmpsize * bg_line_width * 2) ^2
        bgimage <- scattermore(cbind(coords$x, coords$y),
                               rgba = grDevices::col2rgb(alpha = TRUE, scales::alpha(coords$bg_colour, 1)),
                               cex = bgsize,
                               xlim = c(0, 1),
                               ylim = c(0, 1),
                               size = pixels)
        gapimage <- scattermore(cbind(coords$x, coords$y),
                                rgba = grDevices::col2rgb(alpha = TRUE, scales::alpha(gap_colour, 1)),
                                cex = gapsize,
                                xlim = c(0, 1),
                                ylim = c(0, 1),
                                size = pixels)

    }else{
        bgimage <- gapimage <- NULL
    }

    ggplot2:::ggname(
      "geom_scattermore2",
      crasterGrob(
        upperimage, bgimage, gapimage,
        0, 0, 1, 1,
        default.units = "native",
        just = c("left", "bottom"),
        interpolate = interpolate
      )
    )
  },
  draw_key = draw_key_scattermore2
)

#' @importFrom grid grobName
ggname <- function (prefix, grob){
   grob$name <- grobName(grob, prefix)
   grob
}
