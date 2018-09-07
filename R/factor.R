#' @export
init.factor <- function(input.data, factor.order){
    return(factor(input.data, levels = factor.order))
}

#' @export
change.factor.levels <- function(input.factor, old.list, new.list){
    for (index in seq_along(old.list)) {
        levels(input.factor)[levels(input.factor) == old.list[index]] <-
            new.list[index]
    }
    return(input.factor)
}

#' @export
change.factor.order <- function(input.factor, new.order){
    return(init.factor(input.factor, new.order))
}
