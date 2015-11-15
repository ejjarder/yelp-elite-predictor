set.seed(59473717)

# source('parse.user.data.R')
source('model.elite.count.R')
library(Metrics)

source('predict.elite.count.R')

model.definition <- list(
    elite.count = list(
        glm.families = c('quasipoisson'),
        validation.function = rmse
    ),
    was.elite = list(
        glm.families = c('quasibinomial'),
        validation.function = validate.binomial
    )
)

PERCENT.TRAINING <- 0.6

user.dataset <- read.user.dataset()

in.training <-
    createDataPartition(user.dataset$elite.count, p = PERCENT.TRAINING,
                        list = FALSE)

user.training.set <- user.dataset[in.training, ]
user.testing.set <- user.dataset[-in.training, ]

models <- get.models(user.training.set)

training.set.predictions <-
    get.predictions(models, model.definition, user.training.set)
testing.set.predictions <-
    get.predictions(models, model.definition, user.testing.set)
