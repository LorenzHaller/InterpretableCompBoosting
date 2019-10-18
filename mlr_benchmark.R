# Mlr benchmark for regression
library(mlr)
library(OpenML)
source("mlr_wrapper.R")
set.seed(1234)

# Multiple learners to be compared
lrns = list(makeLearner("regr.icb",par.vals = list(nu=0.1, epsilon = 0.05, bl2="btree", max_depth = 4)),
            makeLearner("regr.gbm"), makeLearner("regr.cforest"),
            #makeLearner("regr.crs"),
            makeLearner("regr.gamboost"), makeLearner("regr.glm"),makeLearner("regr.glmboost"),
            makeLearner("regr.lm"), makeLearner("regr.randomForest")
            )


# Choose the resampling strategy
rdesc = makeResampleDesc("Holdout")

# Make a task

## Task 1: Boston Housing
data(BostonHousing, package = "mlbench")
bh.task = makeRegrTask(data = BostonHousing, target = "medv")

## Task 2: Airquality
data("airquality")
attach(airquality)
airquality <- na.omit(airquality)
oz.task = makeRegrTask(data = airquality, target = "Ozone")

## Task 3: kin8nm from Open ML
oml.task_2280 = getOMLTask(2280)
kin8nm = oml.task_2280$input$data.set$data
kin8nm.task = makeRegrTask(data = kin8nm, target = "y")

## Task 4: Wine quality from Open ML
wine.OML.task = getOMLTask(4768)
wine_data = wine.OML.task$input$data.set$data
wine.task = makeRegrTask(data = wine_data, target = "quality")

# Create list of all tasks
tasks = list(bh.task, oz.task, kin8nm.task, wine.task)



taskinfo_all = listOMLTasks(task.type = "Supervised Regression", limit = 10,
                            number.of.instances = c(1000,10000),
                            number.of.features = c(5,150))

# grid = expand.grid(task.id = taskinfo_all$task.id,
#                    lrn.ind = seq_along(lrns))
# 
# runs = lapply(seq_row(grid), function(i) {
#   message(i)
#   task = getOMLTask(grid$task.id[i])
#   ind = grid$lrn.ind[i]
#   runTaskMlr(task, lrns[[ind]])
# })

# run = runTaskMlr(task, lrn)
# run.id = uploadOMLRun(run)

# Make benchmark
bmr = benchmark(lrns, wine.task, rdesc)





getBMRAggrPerformances(bmr, task.ids = 'oml_2280')
plotBMRBoxplots(bmr)
getBMRPerformances(bmr, as.df = TRUE)


plotBMRBoxplots(bmr, measure = mse, order.lrn = getBMRLearnerIds(bmr))
#plotBMRSummary(bmr)
