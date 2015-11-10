library(caret)
library(glmnet)

set.seed(59473717)

P.THRESHOLD <- 0.05
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

get.glm.model <- function(response, predictors, data, family) {
    glm(get.formula(response, predictors), data,
        family = family)
}

find.coefficient.data <- function(response, predictors, data, family) {
    current.fit <- get.glm.model(response, predictors, data, family)
    coefficients <- summary(current.fit)$coefficients
    list(coefficients = coefficients,
         max.p = tail(sort(coefficients[-1, 4]), 1))
}

get.predictors <- function(response, predictors, data, family) {
    print(sprintf('Looking for predictors for %s using %s',
                  response, family))
    
    while (length(predictors) > 0) {
        coef.data <- find.coefficient.data(response, predictors, data, family)
        
        if (coef.data$max.p > P.THRESHOLD) {
            predictors <- predictors[predictors != names(coef.data$max.p)]
        } else break
    }
    
    predictors
}

get.possible.predictors <- function(column.names) {
    possible.predictors.bool <- !(column.names %in% non.predictors)
    column.names[possible.predictors.bool]
}

train.glm.model <- function(response, predictors, data, family) {
    glm.predictors <- get.predictors(response, predictors, data, family)
    
    print('Predictors found!')
    print(glm.predictors)
    print('Training Model...')
    
    train(get.formula(response, glm.predictors),
          data = data[c(response, glm.predictors)], method = 'glm',
          family = family)
}

user.dataset <- read.user.dataset()

in.training <-
    createDataPartition(user.dataset$elite.count, p = 0.6, list = FALSE)

user.training.set <- user.dataset[in.training, ]
user.testing.set <- user.dataset[-in.training, ]

possible.predictors <- get.possible.predictors(names(user.training.set))

poisson.model <- train.glm.model('elite.count', possible.predictors,
                                 user.training.set, 'poisson')

quasipoisson.model <- train.glm.model('elite.count', possible.predictors,
                                      user.training.set, 'quasipoisson')

quasibinomial.model <- train.glm.model('was.elite', possible.predictors,
                                       user.training.set, 'quasibinomial')
