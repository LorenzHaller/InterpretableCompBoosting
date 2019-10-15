# Mlr benchmark for regression
library(mlr)
source("mlr_wrapper.R")
set.seed(1234)

# Multiple learners to be compared
lrns = list(makeLearner("regr.icb",par.vals = list(nu=0.1, epsilon = 0.05, bl2="btree", max_depth = 4)),
            makeLearner("regr.gbm"), makeLearner("regr.cforest"),makeLearner("regr.crs"),
            makeLearner("regr.gamboost"), makeLearner("regr.glm"),makeLearner("regr.glmboost"),
            makeLearner("regr.lm"), makeLearner("regr.randomForest")
            )


# Choose the resampling strategy
rdesc = makeResampleDesc("Holdout")

# Make a task
data(BostonHousing, package = "mlbench")
bh.task = makeRegrTask(data = BostonHousing, target = "medv")
white <- read.csv2("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", dec = ".", header = TRUE) 
wine.task <- makeRegrTask(wine, target = "quality")

tasks = list(bh.task)


# Tasks from OpenML
# library(OpenML)
# 
# task = getOMLTask(10)
# 
# taskinfo_all = listOMLTasks(task.type = "Supervised Regression", limit = 10,
#                             number.of.instances = c(1000,10000),
#                             number.of.features = c(5,150))
# 
# grid = expand.grid(task.id = taskinfo_all$task.id, 
#                    lrn.ind = seq_along(lrns))
# 
# runs = lapply(seq_row(grid), function(i) {
#   message(i)
#   task = getOMLTask(grid$task.id[i])
#   ind = grid$lrn.ind[i]
#   runTaskMlr(task, lrns[[ind]])
# })
# 
# run = runTaskMlr(task, lrn)
# run.id = uploadOMLRun(run)

# Make benchmark
bmr = benchmark(lrns, tasks, rdesc)





getBMRAggrPerformances(bmr)
plotBMRBoxplots(bmr)
getBMRPerformances(bmr, as.df = TRUE)


plotBMRBoxplots(bmr, measure = mse, order.lrn = getBMRLearnerIds(bmr))
plotBMRSummary(bmr)