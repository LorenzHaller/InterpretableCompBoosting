# mlr wrapper benchmark for classification

library(mlr)
library(OpenML)
source("mlr_wrapper.R")
set.seed(177)

# Multiple learners to be compared
lrns.classif = list(makeLearner("classif.icb",par.vals = list(nu=0.1, epsilon = 0.05, bl2="btree", max_depth = 4)),
            makeLearner("classif.binomial"),
            makeLearner("classif.gamboost"),
            makeLearner("classif.glmboost"),
            makeLearner("classif.rpart"),
            makeLearner("classif.gbm"),
            makeLearner("classif.ada"), 
            makeLearner("classif.kknn"),
            makeLearner("classif.svm"),
            makeLearner("classif.randomForest")
)


# Choose the resampling strategy
rdesc = makeResampleDesc("Holdout")

# Make a task


# Make benchmark
bmr.classif = benchmark(lrns.classif, gunpoint.task, rdesc_v2)


getBMRAggrPerformances(bmr.classif)
plotBMRBoxplots(bmr.classif,pretty.names = F)
getBMRPerformances(bmr.classif, as.df = TRUE)