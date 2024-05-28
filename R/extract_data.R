##' @importFrom methods as
##' @importFrom SingleCellExperiment reducedDims reducedDimNames
##' @importFrom SummarizedExperiment assay colData assayNames
##' @importFrom cli cli_abort
.extract_sce_data <- function(object, features = NULL, dims = c(1, 2),
                              reduction = NULL, cells = NULL, slot = 1,
                              plot.pie = FALSE, density=FALSE, grid.n = 400,
                              joint = FALSE, joint.fun = prod, sp.coords=NULL){
    if (!is.null(cells)){
        object <- object[, cells]
    }

    xx <- colData(object) |> as.data.frame(check.names=FALSE) |> suppressWarnings()
    reduced.dat <- NULL
    if (!is.null(dims)){
        if (length(reducedDimNames(object)) == 0){
            cli::cli_abort(c("The {.cls {class(object)}} didn't contain the results of reduction."))
        }
        if (is.null(reduction)){
            reduction <- 1
        }
        reduced.dat <- reducedDims(object)[[reduction]][,dims] |>
            as.data.frame(check.names = FALSE)
        xx <- cbind(reduced.dat, xx)
    }

    if (!is.null(features)){
        if (slot == 'data'){
            if ('logcounts' %in% assayNames(object)){
                slot <- 'logcounts'
            }else{
                slot <- 1
            }
        }

        tmp <- assay(object, slot)
        tmp <- tmp[features, ,drop=FALSE]

        if (density && !is.null(reduced.dat) && !plot.pie){
          tmp <- .buildWkde(w = tmp, coords = reduced.dat, n = grid.n,
                            joint = joint, joint.fun = joint.fun)
        }else if (density && !is.null(sp.coords) && !plot.pie){
          tmp <- .buildWkde(w = tmp, coords = sp.coords, n = grid.n,
                            joint = joint, joint.fun = joint.fun)
        }else{
          tmp <- tmp |>
                 as('matrix') |>
                 t() |>
                 as.data.frame(check.names=FALSE)
        }
        xx <- cbind(xx, tmp)
    }
    xx <- cbind(data.frame(.BarcodeID=rownames(xx)), xx)
    return(xx)
}	

get_dim_data <- function(object, features = NULL,
                    dims=c(1,2), reduction=NULL,
                    cells=NULL, slot = "data",
                    plot.pie=FALSE, density = FALSE,
                    grid.n = 400, joint = FALSE,
                    joint.fun = prod, sp.coords = NULL
                    ) {
    rlang::check_installed('SeuratObject', 'for the internal function `get_dim_data()`.')
    reduced.dat <- NULL

    if (is.null(cells)) {
        cells <- colnames(object)
    }
    #xx <- data.frame(ident=SeuratObject::Idents(object)[cells])
    xx <- cbind(data.frame(ident = SeuratObject::Idents(object)[cells]), object@meta.data[cells,,drop=FALSE])

    if (!is.null(dims)) {
        if (is.null(reduction)) {
            reduction <- SeuratObject::DefaultDimReduc(object)
        }
        dims <- paste0(SeuratObject::Key(object = object[[reduction]]), dims)
        reduced.dat <- as.data.frame(SeuratObject::Embeddings(object[[reduction]])[cells, dims])
    }

    if (!is.null(features)){
        if (is.numeric(features)){
            features <- features[features <= nrow(object)]
            features <- rownames(object)[features]
        }
        tmp <- SeuratObject::FetchData(object, vars = features, cells = cells, slot = slot)
        if (density && !is.null(reduced.dat) && !plot.pie){
            tmp <- .buildWkde(t(tmp), reduced.dat, grid.n, joint, joint.fun)
            xx <- cbind(reduced.dat, xx, tmp)
        }else if(density && !is.null(sp.coords) && !plot.pie){
            tmp <- .buildWkde(t(tmp), sp.coords, grid.n, joint, joint.fun)
            xx <- cbind(xx, tmp)
        }else if (!is.null(reduced.dat) && !density){
            xx <- cbind(reduced.dat, xx, tmp)
        }else{
            xx <- cbind(xx, tmp)
        }
    }else{
        if (!is.null(reduced.dat)){
            xx <- cbind(reduced.dat, xx)
        }
    }
    xx <- cbind(data.frame(.BarcodeID=rownames(xx)), xx)
    return(xx)
}
