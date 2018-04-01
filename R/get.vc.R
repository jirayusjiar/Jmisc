#' Get VarClus based on the absolute Spearman correlation coefficients between metrics
#'
#' This function makes life simple by providing a VarClus.
#' @param dataset  a data frame for data
#' @param indep  a characters or a vector of characters for independent variables
#' @import Hmisc
#' @keywords VarClus
#' @export
get.vc <- function(dataset, indep){
    vc <-
        varclus(~ .,
                similarity = 'spearman',
                data = dataset[, indep],
                trans = "abs")
    return(vc)
}
