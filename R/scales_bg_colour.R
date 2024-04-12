#' Create your own discrete scale
#'
#' @param values a set of aesthetic values to map data values to. If this
#'   is a named vector, then the values will be matched based on the names.
#'   If unnamed, values will be matched in order (usually alphabetical) with
#'   the limits of the scale. Any data values that don't match will be
#'   given `na.value`.
#' @param name The name of the scale. Used as the axis or legend title. If
#' \code{waiver()}, the default, the name of the scale is taken from the first
#' mapping used for that aesthetic. If \code{NULL}, the legend title will be
#' omitted.
#' @param guide A function used to create a guide or its name. See
#' \code{\link[ggplot2:guides]{guides()}} for more information.
#' @param aesthetics The names of the aesthetics that this scale works with.
#' @param breaks One of:
#' \itemize{
#' \item \code{NULL} for no breaks
#' \item \code{waiver()} for the default breaks (the scale limits)
#' \item A character vector of breaks
#' \item A function that takes the limits as input and returns breaks
#' as output. Also accepts rlang \link[rlang:as_function]{lambda} function
#' notation.
#' }
#' @param na.value If \code{na.translate = TRUE}, what aesthetic value should the
#' missing values be displayed as? Does not apply to position scales
#' where \code{NA} is always placed at the far right.
#' @inheritDotParams ggplot2::discrete_scale -expand -position -guide -aesthetics -breaks -na.value -name
#' @name scale_bg_color_manual
#' @return bg_colour scale constructor
#' @importFrom ggplot2 discrete_scale
#' @aliases NULL
NULL

scale_bg_colour <- function(...){
    discrete_scale("bg_colour", palette = scales::hue_pal(), ...)
}

#' @rdname scale_bg_color_manual
#' @export
#' @usage NULL
scale_bg_colour_discrete <- scale_bg_colour


#' @importFrom ggplot2 ScaleDiscreteIdentity waiver
#' @importFrom scales pal_identity
#' @rdname scale_bg_color_manual
scale_bg_colour_identity <- function(name = waiver(), ..., guide = "none",
                                  aesthetics = "bg_colour") {
  discrete_scale(
    aesthetics, name = name,
    palette = pal_identity(), ..., guide = guide,
    super = ScaleDiscreteIdentity
  )
}


#' @rdname scale_bg_color_manual
#' @inheritDotParams ggplot2::discrete_scale -expand -position
#' @export
scale_bg_colour_manual <- function(..., values, aesthetics = "bg_colour",
                                   breaks = waiver(), na.value = "grey50") {
  manual_scale(aesthetics, values, breaks, ..., na.value = na.value)
}

#' @rdname scale_bg_color_manual
#' @usage NULL
scale_bg_color_identity <- scale_bg_colour_identity

#' @export
#' @rdname scale_bg_color_manual
#' @usage NULL
scale_bg_color_manual <- scale_bg_colour_manual

#' @importFrom utils getFromNamespace
#' @keywords internal
manual_scale <- getFromNamespace("manual_scale", "ggplot2")

