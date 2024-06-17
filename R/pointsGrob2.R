##' @importFrom grid unit
##' @importFrom grid gpar
##' @importFrom grid gList
##' @importFrom grid gTree
##' @importFrom grid is.unit
##' @importFrom grid rasterGrob
crasterGrob <- function(image, bg.image = NULL, gap.image = NULL, 
                        x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                        width = NULL, height = NULL, just = "centre",
                        hjust = NULL, vjust = NULL, interpolate = TRUE, 
                        default.units = "npc", name = NULL, gp = gpar(), 
                        vp = NULL){

    upperGrob <- rasterGrob(image, x = x, y = y, width = width, height = height,
                            just = just, hjust = hjust, vjust =vjust, interpolate = interpolate,
                            default.units = default.units, name = name, gp = gp, vp = vp)

    if (is.null(bg.image)){
        return(upperGrob)
    }

    bgGrob <- rasterGrob(bg.image, x = x, y = y, width = width, height = height, 
                         just = just, hjust = hjust, vjust =vjust, 
                         interpolate = interpolate, default.units = default.units, 
                         name = name, gp = gp, vp = vp)

    gapGrob <- rasterGrob(gap.image, x = x, y = y, width = width, height = height,
                         just = just, hjust = hjust, vjust =vjust,
                         interpolate = interpolate, default.units = default.units,
                         name = name, gp = gp, vp = vp)

    grobs <- gList(bgGrob, gapGrob, upperGrob)
    gTree(children = grobs)       
}

grid.craster <- function (image, bg.image, gap.image, x = unit(0.5, "npc"),
                          y = unit(0.5, "npc"), width = NULL, 
                          height = NULL, just = "centre", hjust = NULL, 
                          vjust = NULL, interpolate = TRUE, default.units = "npc",
                          name = NULL, gp = gpar(), vp = NULL){

    rg <- crasterGrob(image, bg.image, gap.image, x = x, y = y, 
                      width = width, height = height, just = just, 
                      hjust = hjust, vjust = vjust, interpolate = interpolate,
                      default.units = default.units, name = name, gp = gp,
                      vp = vp)

    grid.draw(rg)
}


##' @importFrom grid pointsGrob
cpointsGrob <- function(x = stats::runif(10), y = stats::runif(10), pch = 1,
    size = unit(1, "char"), bg_line_width = .3, gap_line_width = .1,
    bg_colour = "black", gap_colour = 'white', default.units = "native",
    name = NULL, gp = gpar(), vp = NULL){

    upperPointGrob <- pointsGrob(x = x, y = y, pch = pch, size = size,
                                default.units = default.units, name = name,
                                gp = gp, vp = vp)

    if (is.null(bg_colour) || all(is.na(bg_colour))){
        return(upperPointGrob)
    }

    gp_bg <- gp
    gp_gap <- gp

    gp_bg$col <- bg_colour
    gp_gap$col <- gap_colour

    tmpsize <- sqrt(gp$fontsize)
    gp_gap_size <- (tmpsize + tmpsize * gap_line_width * 2)^2
    gp_bg_size <- gp_gap_size + (sqrt(bg_line_width) + tmpsize * bg_line_width * 2) ^2
    gp_gap$fontsize <- gp_gap_size
    gp_bg$fontsize <- gp_bg_size
    gapPointGrob <- pointsGrob(x = x, y = y, pch = pch, size = size,
                              default.units = default.units, name = name,
                              gp = gp_gap, vp = vp)

    bgPointGrob <- pointsGrob(x = x, y = y, pch = pch, size = size,
                             default.units = default.units, name = name,
                             gp = gp_bg, vp = vp)

    grobs <- gList(bgPointGrob, gapPointGrob, upperPointGrob)
    gTree(children = grobs)
}

##' @importFrom grid grid.draw
grid.cpoints <- function(x = stats::runif(10), y = stats::runif(10), pch = 1,
    size = unit(1, "char"), bg_line_width = .3, gap_line_width = .1,
    bg_colour = "black", gap_colour = 'white', default.units = "native",
    name = NULL, gp = gpar(), draw = TRUE, vp = NULL){
    pg <- cpointsGrob(x = x, y = y, pch = pch, size = size, bg_line_width = bg_line_width,
                      gap_line_width = gap_line_width, bg_colour = bg_colour,
                      gap_colour = gap_colour, default.units = default.units, name = name,
                      gp = gp, vp = vp)
    if (draw) grid.draw(pg)
    invisible(pg)
}

