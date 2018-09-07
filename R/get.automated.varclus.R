

#' Automated Spearman rank correlation test
#'
#' This function automatically select non-correlated metrics based on a Spearman rank correlation test. To do so, we start from the pair of the strongest correlated metrics. Since these two metrics can be linearly predicted with each other, one of these two metrics must be removed while selecting the other. We select the metric that has the lowest Spearman correlation coefficient with the other metrics that are not in the pair. We repeat this process until all metrics have their Spearman correlation coefficient below a threshold value (default = 0.7).
#'
#' @param dataset  a data frame for data
#' @param indep  a characters or a vector of characters for independent variables
#' @param dep a character for dependent variable
#' @param spearman.threshold a numeric for a threshold of Spearman rank correlation test (default = 0.7)
#' @param verbose  TRUE for printing
#' @import Hmisc effsize
#' @keywords Spearman
#' @export

get.automated.varclus <-
    function(dataset, indep, dep, threshold, verbose = F) {
        .getCDValue <- function(dataset, indep, dep) {
            cliffDelta <- {

            }
            for (index in 1:length(indep)) {
                cd <-
                    cliff.delta(dataset[dataset[, dep] == TRUE, indep[index]], dataset[dataset[, dep] == FALSE, indep[index]])$estimate
                cliffDelta <- rbind(cliffDelta, cd)
            }

            row.names(cliffDelta) <- indep
            cliffDelta <-
                cliffDelta[order(-abs(cliffDelta)), , drop = FALSE]

            #cat(row.names(cliffDelta),'\n')
            #cat(cliffDelta,'\n We select ',row.names(cliffDelta)[1],'\nDONE\n')
            return(row.names(cliffDelta))

        }

        cliffDeltaOrder <- getCDValue(dataset, indep, dep)

        repeat {
            if (length(indep) == 1)
                break
            # Variable clustering analysis
            vc <-
                varclus( ~ .,
                         similarity = 'spearman',
                         data = dataset[, indep],
                         trans = "abs")
            #plot(vc)
            varClustered <-
                cutree(vc$hclust, h = (1 - threshold))
            selectedVCIndep <- {

            }
            for (clusterIndex in as.numeric(names(table(varClustered)))) {
                tmpIndep <- (names(varClustered[varClustered == clusterIndex]))
                if (length(tmpIndep) == 1) {
                    # Include if it is the only indep in the cluster
                    selectedVCIndep <-
                        rbind(selectedVCIndep, tmpIndep)
                } else{
                    # Select indep that has the highest cliff delta score
                    selectedVCIndep <- rbind(selectedVCIndep,
                                             cliffDeltaOrder[cliffDeltaOrder %in% tmpIndep][1])
                }
            }

            selectedVCIndep <- unname(selectedVCIndep)
            if (verbose) {
                cat(
                    'Varclus\nnIndep ->',
                    length(indep),
                    '\tnOutput ->',
                    length(selectedVCIndep),
                    '\nRemoved variables\n',
                    indep[!(indep %in% selectedVCIndep)],
                    '\n\n'
                )
            }
            if (length(indep) == length(selectedVCIndep)) {
                break
            } else {
                indep <- indep[indep %in% selectedVCIndep]
            }

        }

        return(indep)
    }
