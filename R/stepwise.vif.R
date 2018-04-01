#' Automated Stepwise Variance Inflation Factor Analysis
#'
#' This function makes life simple by automatically excluding the most predictable metric according to VIF in a stepwise manner. For each step, the most predictable metric that has its VIF score of above the pre-defined threshold (default = 5) will be excluded. Such process is repeated untill all remaining metrics have their VIF score below the threshold.
#'
#' @param dataset  a data frame for data
#' @param indep  a characters or a vector of characters for independent variables
#' @param vif.threshold a numeric for a threshold of VIF score (default = 5)
#' @param verbose  TRUE for printing
#' @param export  TRUE for exporting results of the analysis
#' @param export.path a path for export file
#' @param export.file.name a file name for export file
#' @import rms
#' @keywords VIF
#' @export
stepwise.vif <-
    function(dataset,
             indep,
             vif.threshold = 5,
             verbose = F,
             export = F,
             export.path = '.',
             export.file.name = 'vif.results.csv') {

        dataset$dummy <- rnorm(nrow(dataset))
        # remove constant value metrics
        indep <- check.constant.metrics(dataset, indep)
        output <- indep
        step.count <- 1
        output.results <- list()
        repeat {
            vif.scores <-
                vif(lm(as.formula(paste0(
                    'dummy~', paste0(output, collapse = '+')
                )), data = dataset))


            output.results[[step.count]] <-
                sort(vif.scores, decreasing = F)

            vif.scores <- vif.scores[vif.scores >= vif.threshold]
            if (length(vif.scores) == 0)
                break
            drop.var <- names(vif.scores[vif.scores == max(vif.scores)])[1]
            if (verbose) {
                print(paste0(
                    'Step ',
                    step.count,
                    ' - Exclude ',
                    drop.var,
                    ' (VIF = ',
                    max(vif.scores),
                    ')'
                ))
            }
            step.count <- step.count + 1
            output <- output[!output %in% drop.var]
        }
        names(output.results) <- paste0('Iteration ', 1:step.count)
        names(output.results)[length(output.results)] <- 'Final'

        if (export) {
            lapply(seq_along(output.results), function(x, y, z) {
                o = t(x[[z]])
                row.names(o) = y[z]
                write.table(
                    o,
                    file = paste0(export.path, '/', export.file.name),
                    sep = ',',
                    append = T,
                    col.names = NA
                )
            }, x = output.results, y = names(output.results))
        }
        return(output)
    }
