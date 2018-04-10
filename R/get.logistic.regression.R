#' Get Logistic Regression Model function
#'
#' This function makes life simple by automatically fitting a logistic regression model.
#'
#' @param dataset  a data frame for data
#' @param indep  a characters or a vector of characters for independent variables
#' @param dep a characters for a dependent variable
#' @param training.index  a vector of numerics for training data indexes
#' @keywords LR
#' @export
get.logistic.regression <-
    function(dataset,
             indep,
             dep,
             training.index) {
        options(contrasts = c("contr.sum", "contr.poly"))
        lr.model <-
            glm(as.formula(paste0(dep,
                                  '~',
                                  paste0(
                                      indep, collapse = '+'
                                  ))),
                data = dataset[training.index, ],
                family = binomial())

        return(lr.model)
    }
