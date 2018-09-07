#' Initialize a factor
#' @export
init.factor <- function (input.data, factor.order)
{
    return(factor(input.data, levels = factor.order))
}
