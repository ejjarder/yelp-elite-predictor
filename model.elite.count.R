library(caret)
library(glmnet)

non.predictors <- c('X', # index
                    'type', # everything is a user
                    'user_id', 'name', # identifiers
                    'yelping_since', # replaced by days.yelping
                    'was.elite', 'elite.count') # results

read.user.dataset <- function() {
    user.dataset <- read.csv('user.data.csv')
    user.dataset[is.na(user.dataset)] <- 0
    user.dataset$was.elite <- as.factor(user.dataset$elite.count > 0)
    user.dataset
}

get.formula <- function(response, predictors) {
    as.formula(paste(response, '~', paste(predictors, collapse = ' + ')))
}

train.glm.model <- function(family, response, predictors, data) {
    print(sprintf('Predicting %s using %s', response, family))
    
    train(get.formula(response, predictors),
          data = data[c(response, predictors)], method = 'glm',
          family = family,
          trControl = trainControl(method = 'cv', verboseIter = TRUE))
}

get.model.per.family <- function(response, model.definition, possible.predictors,
                                 training.set) {
    glm.families <- model.definition[[response]]$glm.families
    models.per.family <- lapply(glm.families, train.glm.model, response,
                                possible.predictors, training.set)
    names(models.per.family) <- glm.families
    models.per.family
}

get.models.per.response <- function(model.definition, possible.predictors,
                                    user.training.set) {
    response.list <- names(model.definition)
    models <- lapply(response.list, get.model.per.family, model.definition,
                     possible.predictors, user.training.set)
    names(models) <- response.list
    models
}

get.models <- function(training.set) {
    column.names <- names(training.set)
    possible.predictors.bool <- !(column.names %in% non.predictors)
    possible.predictors <- column.names[possible.predictors.bool]
    
    get.models.per.response(model.definition, possible.predictors,
                            user.training.set)
}

get.non.intercept.coefficients <- function(model) {
    # get all coefficients except intercept
    model.coefficients <- summary(model)$coef[-1, ]
    estimate.order <- order(abs(model.coefficients[, 'Estimate']),
                            decreasing = TRUE)
    model.coefficients[estimate.order, ]
}
