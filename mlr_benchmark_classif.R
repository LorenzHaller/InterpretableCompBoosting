# mlr wrapper benchmark for classification

library(mlr)
library(OpenML)
source("mlr_wrapper.R")
set.seed(177)

# Multiple learners to be compared
lrns = list(makeLearner("classif.icb",par.vals = list(nu=0.1, epsilon = 0.05, bl2="btree", max_depth = 4)),
            makeLearner("classif.ada"), makeLearner("classif.binomial"),
            makeLearner("classif.logreg"),
            makeLearner("classif.gamboost"), makeLearner("classif.kknn"),makeLearner("classif.glmboost"),
            makeLearner("classif.xgboost"), makeLearner("classif.randomForest")
)


# Choose the resampling strategy
rdesc = makeResampleDesc("Holdout")

# Make a task