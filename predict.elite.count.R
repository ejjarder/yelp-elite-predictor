validate.binomial <- function(prediction, expected) {
    confusionMatrix(prediction, expected)
}

predict.per.model <- function(model, response, validation.function, data) {
    prediction <- predict(model, data)
    validation <- validation.function(prediction, data[[response]])
    list(prediction = prediction, validation = validation)
}

predict.per.response <- function(response, models, model.definition, data) {
    validation.function <- model.definition[[response]]$validation.function
    response.models <- models[[response]]
    prediction.per.model <- lapply(response.models, predict.per.model,
                                   response, validation.function, data)
}

get.predictions <- function(models, model.definition, data) {
    predictions <- lapply(names(models), predict.per.response,
                          models, model.definition, data)
    names(predictions) <- names(models)
    predictions
}
