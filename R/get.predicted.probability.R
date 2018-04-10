#' Get predicted probability
#'
#' This function makes life simple by automatically retrieving predicted probability of logistic regression and random forest.
#'
#' @param model a logistic regression or random forest model
#' @param test.data a data frame of testing dataset
#' @param indep a vector for independent variables
#' @import randomForest
#' @keywords predict
#' @export
get.predicted.probability <- function(model, test.data, indep) {
    model.technique <- class(model)[1]
    if (model.technique == 'glm') {
        # Logistic regression
        predicted.probs <- predict(model, newdata = as.data.frame(test.data[, indep, drop = FALSE]), type = "response")
        return(predicted.probs)
    } else if (model.technique == 'randomForest') {
        # Random forest
        predicted.probs <- predict(model, newdata = as.data.frame(test.data[, indep, drop = FALSE]), type = 'prob')
        positive.index <- match('TRUE', names(predicted.probs[1,]))
        predicted.probs <- predicted.probs[, positive.index]
        return(predicted.probs)
    } else {
        stop('Input model must be Logistic Regression or Random Forest')
    }
}
