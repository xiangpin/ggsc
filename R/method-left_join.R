#' @importFrom dplyr left_join
#' @method left_join ggsc
#' @importFrom cli cli_warn
#' @export
left_join.ggsc <- function(x, y, by = NULL, copy = FALSE, suffix=c("", ".y"), ...){
    dat <- x$data
    msg <- c("The {.arg suffix} requires a character vector containing 2 different elements,",
             "The first element must be \"\", and the second element must not be \"\",",
             "it was set {.code suffix=c(\"\", \".y\")} automatically.")
    if (all(nchar(suffix)!=0)){
        cli::cli_warn(msg)
        suffix[1] = ""
    }
    if (all(nchar(suffix)==0)){
        cli::cli_warn(msg)
        suffix[2] = ".y"
    }
    if (nchar(suffix[1])!=0 && nchar(suffix[2])==0){
        cli::cli_warn(msg)
        suffix <- rev(suffix[seq_len(2)])
    }
    da <- dplyr::left_join(dat, y, by = by, copy = copy, suffix = suffix, ...) 

    x$data <- da

    return(x)
}

#' add the external data frame to the ggsc object.
#' @rdname attacher
#' @param x ggsc object, the result of \code{sc_dim}, \code{sc_feature} and \code{sc_spatial}.
#' @param y a data.frame, which the first column should be a barcode id, which is similar with
#' the x$data$`.BarcodeID`. It also can have another column \code{features}.
#' @export
#' @return ggsc object
`%<<+%` <- function(x, y){
    if (!inherits(x, 'ggsc')){
        cli::cli_abort(c("Can not add the `data` in the right to left object, since it is not a `ggsc` class."))
    }
    if (missing(y)){
        cli::cli_abort(c(
                "Cannot use {.code <+} with a single argument.",
                "i" = "Did you accidentally put {.code %<<+%} on a new line?"
        ))
    }
    x <- left_join(x, y)
    return(x)
}
