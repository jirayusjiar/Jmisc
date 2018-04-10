#' Get Random Forest model function (Classification)
#'
#' This function makes life simple by automatically fitting a logistic regression model.
#'
#' @param dataset  a data frame for data
#' @param indep  a characters or a vector of characters for independent variables
#' @param dep a characters for a dependent variable
#' @param training.index  a vector of numerics for training data indexes
#' @param ntree a numeric for the number of trees
#' @import randomForest
#' @keywords RF
#' @export
get.random.forest.classification <-
    function(dataset,
             indep,
             dep,
             training.index,
             ntree = 100) {

        outcome <- ifelse(is.factor(dataset[, dep]), dataset[, dep], factor(dataset[, dep]))
        rf.model <- randomForest(
            x = dataset[training.index, indep, drop = FALSE],
            y = outcome,
            ntree = ntree,
            importance = T,
            keep.forest = T
        )

        return(rf.model)
    }
