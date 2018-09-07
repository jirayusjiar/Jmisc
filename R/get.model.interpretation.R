#' Get model interpretation for logistic regression and random forest
#'
#' This function makes life simple by automatically interpreting logistic regression and random forest models.
#' The interpretation techniques for logistic regression are ANOVA Type-I and variants of ANOVA Type-II (i.e., Wald, Chisq, and F).
#' The interpretation techniques for random forest are Gini and Permutation (i.e., Scaled and Raw) importance.
#'
#' @param model a logistic regression or random forest model
#' @param normalized a logical to indicate whether to normalize importance scores
#' @import car randomForest
#' @keywords interpretation
#' @export
get.model.interpretation <- function (model, normalized = F)
{
    model.technique <- class(model)[1]
    if (model.technique == "glm") {
        indep <- names(model$coefficients)[-1]
        anova1 <- anova(model)$Deviance[-1]
        names(anova1) <- indep
        anova1[is.na(anova1)] <- 0

        if(NA %in% model$coefficients){ # Refit excluding NA coefficients
            model <- update(model, as.formula(paste0('.~.-', paste0(indep[is.na(model$coefficients)[-1]], collapse = '-'))))
        }

        anova2.wald <- Anova(model, type = 2, test.statistic = "Wald",
                             singular.ok = T)[, 2]
        names(anova2.wald) <- names(model$coefficients[-1])
        anova2.wald <- anova2.wald[indep]
        anova2.wald[is.na(anova2.wald)] <- 0
        names(anova2.wald) <- indep

        anova2.chisq <- Anova(model, type = 2, test.statistic = "LR",
                              singular.ok = T)[, 1]
        names(anova2.chisq) <- names(model$coefficients[-1])
        anova2.chisq <- anova2.chisq[indep]
        anova2.chisq[is.na(anova2.chisq)] <- 0
        names(anova2.chisq) <- indep

        anova2.f <- Anova(model, type = 2, test.statistic = "F",
                          singular.ok = T)[1:length(indep), 3]
        names(anova2.f) <- names(model$coefficients[-1])
        anova2.f <- anova2.f[indep]
        anova2.f[is.na(anova2.f)] <- 0
        names(anova2.f) <- indep

        if (normalized) {
            anova1 <- anova1/sum(anova1) * 100
            anova2.wald <- anova2.wald/sum(anova2.wald) * 100
            anova2.chisq <- anova2.chisq/sum(anova2.chisq) *
                100
            anova2.f <- anova2.f/sum(anova2.f) * 100
        }
        return(list(ANOVA.TypeI = anova1, ANOVA.TypeII.Wald = anova2.wald,
                    ANOVA.TypeII.Chisq = anova2.chisq, ANOVA.TypeII.F = anova2.f))
    }
    else if (model.technique == "randomForest") {
        indep <- names(model$forest$xlevels)
        gini <- as.data.frame(t(importance(model, type = 2)[,
                                                            1]))
        gini[is.na(gini)] <- 0
        perm.raw <- as.data.frame(t(importance(model, type = 1,
                                               scale = F)[, 1]))
        perm.raw[is.na(perm.raw)] <- 0
        perm.scaled <- as.data.frame(t(importance(model, type = 1,
                                                  scale = T)[, 1]))
        perm.scaled[is.na(perm.scaled)] <- 0
        if (normalized) {
            gini <- gini/sum(gini) * 100
            perm.raw <- perm.raw/sum(perm.raw) * 100
            perm.scaled <- perm.scaled/sum(perm.scaled) * 100
        }
        return(list(Gini = gini, Permutation.Raw = perm.raw,
                    Permutation.Scaled = perm.scaled))
    }
    else {
        stop("Input model must be Logistic Regression or Random Forest")
    }
}
