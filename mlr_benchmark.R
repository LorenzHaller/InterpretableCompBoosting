# Mlr benchmark for regression
library(mlr)
library(OpenML)
source("mlr_wrapper.R")
set.seed(177)



# Multiple learners to be compared
lrns = list(makeLearner("regr.icb",par.vals = list(nu=0.1, epsilon = 0.005, bl2="btree", max_depth = 4)),
            makeLearner("regr.lm"),
            makeLearner("regr.gamboost"),
            makeLearner("regr.glmboost"),
            makeLearner("regr.rpart"),
            makeLearner("regr.gbm"),
            makeLearner("regr.svm"),
            makeLearner("regr.randomForest")
            #,makeLearner("regr.xgboost")
            )


# Choose the resampling strategy
rdesc = makeResampleDesc("Holdout")
rdesc_v2 = makeResampleDesc("CV",iters=5)

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

## Task 5: puma8NH
puma.OML.task = getOMLTask(2313)
puma8NH = puma.OML.task$input$data.set$data
puma.task = makeRegrTask(data = puma8NH, target = "thetadd3")

# Create list of all tasks
tasks = list(bh.task, oz.task, kin8nm.task, wine.task, puma.task)



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

icb.learner0 = makeLearner("regr.icb",id="btree_0.1_0.05",par.vals = list(nu=0.1, epsilon = 0.05, bl2="btree", max_depth = 4))
icb.learner1 = makeLearner("regr.icb",id="btree_0.1_0.025",par.vals = list(nu=0.1, epsilon = 0.025, bl2="btree", max_depth = 4))
icb.learner2 = makeLearner("regr.icb",id="btree_0.1_0.01",par.vals = list(nu=0.1, epsilon = 0.01, bl2="btree", max_depth = 4))
icb.learner3 = makeLearner("regr.icb",id="btree_0.1_0.005",par.vals = list(nu=0.1, epsilon = 0.005, bl2="btree", max_depth = 4))
icb.learner4 = makeLearner("regr.icb",id="btree_0.1_0.001",par.vals = list(nu=0.1, epsilon = 0.001, bl2="btree", max_depth = 4))
icb.learner4.1 = makeLearner("regr.icb",id="btree_0.1_0.0005",par.vals = list(nu=0.1, epsilon = 0.0005, bl2="btree", max_depth = 4))


icb.learner5 = makeLearner("regr.icb",id="btree_0.05_0.05",par.vals = list(nu=0.05, epsilon = 0.05, bl2="btree", max_depth = 4))
icb.learner6 = makeLearner("regr.icb",id="btree_0.05_0.025",par.vals = list(nu=0.05, epsilon = 0.025, bl2="btree", max_depth = 4))
icb.learner7 = makeLearner("regr.icb",id="btree_0.05_0.01",par.vals = list(nu=0.05, epsilon = 0.01, bl2="btree", max_depth = 4))
icb.learner8 = makeLearner("regr.icb",id="btree_0.05_0.005",par.vals = list(nu=0.05, epsilon = 0.005, bl2="btree", max_depth = 4))
icb.learner9 = makeLearner("regr.icb",id="btree_0.05_0.001",par.vals = list(nu=0.05, epsilon = 0.001, bl2="btree", max_depth = 4))
icb.learner9.1 = makeLearner("regr.icb",id="btree_0.05_0.0005",par.vals = list(nu=0.05, epsilon = 0.0005, bl2="btree", max_depth = 4))


icb.learner10 = makeLearner("regr.icb",id="bbs_0.1_0.01",par.vals = list(nu=0.1, epsilon = 0.01, bl2="bbs", max_depth = 4))
icb.learner11 = makeLearner("regr.icb",id="bbs_0.1_0.005",par.vals = list(nu=0.1, epsilon = 0.005, bl2="bbs", max_depth = 4))
icb.learner12 = makeLearner("regr.icb",id="bbs_0.1_0.001",par.vals = list(nu=0.1, epsilon = 0.001, bl2="bbs", max_depth = 4))


icb_list = list(icb.learner0,icb.learner1,icb.learner2,icb.learner3,icb.learner4,
                icb.learner5,icb.learner6,icb.learner7,icb.learner8,icb.learner9)

icb_list_2 = list(icb.learner2,icb.learner3,icb.learner4,icb.learner4.1,
                  icb.learner7,icb.learner8,icb.learner9,icb.learner9.1)

icb_list_3 = list(icb.learner2,icb.learner3,icb.learner4,
                  icb.learner10,icb.learner11,icb.learner12)

# Make benchmark
bmr = benchmark(lrns, oz.task, rdesc_v2)





getBMRAggrPerformances(bmr)
plotBMRBoxplots(bmr,pretty.names = F)
getBMRPerformances(bmr, as.df = TRUE)


plotBMRBoxplots(bmr, measure = mse, order.lrn = getBMRLearnerIds(bmr))
#plotBMRSummary(bmr)
