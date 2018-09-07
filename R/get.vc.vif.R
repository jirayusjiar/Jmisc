

#' Get VarClus based on the absolute Spearman correlation coefficients between metrics
#'
#' This function makes life simple by providing a VarClus.
#' @param dataset  a data frame for data
#' @param indep  a vector of characters or a vector of characters for independent variables
#' @param similarity a character for similarity measures (e.g., Spearman rank correlation), default = spearman
#' @param varclus.threshold a numeric for correlation coefficient threshold value
#' @import Hmisc effsize rms
#' @keywords VarClus
#' @export
get.vc.vif <-
    function(dataset,
             indep,
             dep,
             varclus.threshold,
             vif.threshold) {
        vc.indep <-
            get.automated.varclus(dataset, indep, dep, varclus.threshold, verbose = F)

        vc.vif.indep <- stepwise.vif(dataset,
                                     vc.indep,
                                     vif.threshold = 5,
                                     verbose = F)

        return(vc.vif.indep)

    }
