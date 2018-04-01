#' Remove metrics that are constant
#'
#' This function makes life simple by checking and removing constant metrics.
#' @param dataset  a data frame for data
#' @param indep  a characters or a vector of characters for independent variables
#' @keywords Constant
#' @export
check.constant.metrics <- function(dataset, indep) {
    constant.metrics <- indep[apply(dataset[, indep], 2, function(x)
        max(x) == min(x))]
    if (length(constant.metrics) != 0) {
        print(paste0(
            'Remove constant metric(s): ',
            paste0(constant.metrics, collapse = ', ')
        ))
    }
    return(indep[!indep %in% constant.metrics])
}
