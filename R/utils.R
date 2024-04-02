.check_aes_mapping <- function(object, mapping = NULL, data, prefix='label', aes.character = "colour"){
    if (prefix %in% colnames(data)){
        default_mapping <- aes(!!rlang::sym(prefix))
    }else{
        if (!aes.character %in% names(mapping)){
            cli::cli_warn(c("'{prefix}' is not in the column name metadata of {.cls {class(object)}}.",
                           "You can set `mapping = aes({aes.character} = AnnotationColumnName)` manually."))   
        }
        default_mapping <- aes(NULL)
    }
    names(default_mapping) <- aes.character

    if (is.null(mapping)) {
        mapping <- default_mapping
    } else {
        mapping <- modifyList(default_mapping, mapping)
    }    
    
    return(mapping)
}


.add_aes_ <- function(x, object, data, prefix = 'label', aes.character = 'x'){
    if (!aes.character %in% names(x)){
        if (prefix %in% colnames(data)){
            new.aes <- aes(!!rlang::sym(prefix))
        }else{
            cli::cli_abort(c("'{prefix}' is not in the column name of metadata of {.cls {class(object)}}",
                             "Yout can set `mapping = aes({aes.character} = XXX)` manually."), call=NULL)
        }
        names(new.aes) <- aes.character
        x <- modifyList(new.aes, x)
    }
    return(x)
}

.add_aes <- function(x, object, data, prefix = c('label', 'value'), aes.character = c('x', 'y')){
    for (i in seq(length(prefix))){
        x <- .add_aes_(x, object, data, prefix = prefix[i], aes.character = aes.character[i])
    }
    return(x)
}



