##' @title plot_lisa_feature
##' @rdname plot-lisa-feature
##' @param spe SpatialExperiment object.
##' @param feature selected features to be visualized.
##' @param lisa.res the result returned by \code{SVP::runLISA()}.
##' @param assay.type the assay name where data will be used from
##' (e.g., 'data', 'counts'), default is \code{'logcounts'}.
##' @param geom the function of geometric layer, default is \code{geom_bgpoint}, 
##' other option is \code{sc_geom_point}.
##' @param pointsize numeric the size of point, default is \code{10}.
##' @param hlpointsize numeric the size of point which contains corresbonding
##' spatially variable gene(i.e., SVG), default is \code{8}.
##' @param clustertype cell type which is from the result of \code{lisa.res},
##' default is \code{'High'}.
##' @param hlcolor the color of circular line which enfolds the point 
##' that contains SVG, default is \code{'black'}.
##' @param gap_line_width numeric the line width of gap between the background and
##' top point point layer, default is \code{.1}.
##' @param bg_line_width numeric the line width of background point layer,
##' default is \code{0.3}.
##' @param facet_name the name of facet used in \code{ggh4x::facet_nested_wrap()},
##' default is \code{NULL}.
##' @param type the type of the name of gene's prefix that will be substituted
##' with \code{""}, default is \code{'TFT'}, other options are \code{'GO.BP'} and \code{'Reactome'}.
##' @param reduction reduction method, default is \code{NULL} and will 
##' use the default setting store in the object
##' @param ... additional parameters pass to \code{scattermore::geom_scattermore()}
##' \itemize{
##'     \item \code{bg_colour} the colour of background point, default is \code{NA}.
##'      this character also can be set in \code{mappint}.
##'     \item \code{gap_colour} the colour of gap background, default is \code{'white'}.
##'     \item \code{bg_line_width} the line width of background point,
##'      default is \code{.3}.
##'     \item \code{gap_line_width} the gap line width of background point,
##'      default is \code{.1}.
##'     \item \code{alpha} the transparency of colour, default is 1.
##'     \item \code{subset} subset the data frame which meet conditions to display.
##'      this should be set in \code{mapping}.
##'  }
##' @return ggplot object
##' @importFrom ggplot2 theme element_rect
##' @importFrom stats as.formula
##' @export
##' @examples
##' \dontrun{
##' library(ggplot2)
##' library(ggh4x)
##' library(SingleCellExperiment) |> suppressPackageStartupMessages()
##' library(SpatialExperiment) |> suppressPackageStartupMessages()
##' spe <- readRDS("./Visium_humanDLPFC.spe.rds")
##' spe <-scater::logNormCounts(spe)
##' genes <- c('MOBP', 'PCP4', 'SNAP25', 'HBB', 'IGKC', 'NPY')
##' target.features <- rownames(spe)[match(genes, rowData(spe)$gene_name)]
##' lisa.res1 <- runLISA(spe,
##'                      assay.type='logcounts',
##'                      features=target.features[seq(2)],
##'                      weight.method='knn', 
##'			 k=50)
##' plot_lisa_feature(spe, lisa.res=lisa.res1, feature=target.features[seq(2)],
##'                   pointsize=2, hlpointsize=2, gap_line_width=.1)
##' }
plot_lisa_feature <- function(spe,
                         feature,
                         lisa.res,
                         assay.type = 'logcounts',
                         geom = geom_bgpoint,
                         pointsize = 10,
                         hlpointsize = 8,
                         clustertype = 'High',
                         hlcolor = c('black'),
                         gap_line_width = .1,
                         bg_line_width = .3,
                         facet_name = NULL,
                         type = 'TFT',
                         reduction = NULL,
                         ...
                         ){
    prefix.gsub <- switch(type, TFT="_TARGET_GENES|_UNKNOWN", GO.BP="GOBP_", Reactome="REACTOME_")
    rownames(spe) <- gsub(prefix.gsub, "", rownames(spe))
    feature <- gsub(prefix.gsub, "", feature)
    if (is.null(reduction)){
        cnm <- SpatialExperiment::spatialCoordsNames(spe)
        p <- sc_spatial(spe,
                        feature,
                        mapping = aes(x=!!rlang::sym(cnm[1]), y = !!rlang::sym(cnm[2])),
                        pointsize = pointsize,
                        slot = assay.type,
                        gap_colour = NA,
                        image.plot = FALSE,
                        geom = geom,
                        ...
        )
    }else{
        p <- sc_feature(
               spe,
               features = feature,
               reduction = reduction,
               geom = geom,
               pointsize = pointsize,
               slot = assay.type,
               ...
        )
    }
    if (missing(lisa.res) || is.null(lisa.res)){
        message("The lisa result is not provided")
        return(p)
    }

    if (inherits(lisa.res, 'SimpleList') || inherits(lisa.res, "list")){
        names(lisa.res) <- gsub(prefix.gsub, "", names(lisa.res))
        lisa.res <- lisa.res |>
                    lapply(function(x)x|>tibble::rownames_to_column(var='.BarcodeID')) |>
                    dplyr::bind_rows(.id='features')
    }
    if (inherits(p, 'patchwork')){
        `%add+%` <- `&`
    }else{
        `%add+%` <- `+`
    }
    p1 <- p %add+% sc_geom_annot(
                    data = lisa.res,
                    mapping = aes(bg_colour = lisa.res$cluster.test, subset = lisa.res$cluster.test %in% clustertype),
                    pointsize = hlpointsize,
                    gap_line_width = gap_line_width,
                    bg_line_width = bg_line_width
          ) %add+%
          scale_bg_colour_manual(
            values = hlcolor,
            guide = guide_legend(
              theme = theme(
                legend.title = element_text(size = 8),
                legend.text = element_text(size = 6),
                legend.key.width  = grid::unit(.3, "cm"),
                legend.key.height  = grid::unit(.3, "cm")
              ),
              order = 1
            )
          ) %add+%
          guides(colour = guide_colorbar(
            theme = theme(
              legend.title = element_text(size=8),
              legend.text = element_text(size=6),
              legend.key.width  = grid::unit(.4, "cm"),
              legend.key.height = grid::unit(1.5, "cm")
              )
            )
          ) %add+%
          theme(
            strip.background.x=element_rect(color="white")
          )
    spe$sample_id |> unique() |> length() -> len
    if (len > 1 && !is.null(facet_name)){
        if (length(facet_name) > 1){
           tmpf <- paste0(facet_name, collapse="~") |> as.formula()
        }else{
           tmpf <- as.formula("~sample_id")
        }
        p1 <- p1 %add+%
              ggh4x::facet_nested_wrap(tmpf) %add+%
              theme(strip.background.x=element_rect(color="white"))
    }
    return(p1)
}
    	
