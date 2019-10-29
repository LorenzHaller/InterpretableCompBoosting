# Mlr benchmark for regression
library(mlr)
library(OpenML)
source("mlr_wrapper.R")
set.seed(177)
#devtools::install_github("jakob-r/mlrHyperopt", dependencies = TRUE)
library(mlrHyperopt)

###### Find a list of tasks for the benchmark #############################


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





####### Hypetuning Part ##################################################

tsk = oz.task
ctrl = makeTuneControlRandom(maxit = 100L)
rdesc_tune = makeResampleDesc("CV", iters = 3L)


####### Hypertuning for the parameters epsilon and nu for icb method ######

num_ps = makeParamSet(
  makeNumericParam("nu", lower = 0.001, upper = 0.2),
  makeNumericParam("epsilon", lower = 0.00001, upper = 0.1),
  makeDiscreteLearnerParam(id = "bl2", default = "btree", values = c("bbs","btree"), tunable = F),
  makeIntegerLearnerParam(id = "max_depth", default = 8L, lower = 3, upper = 30, tunable = F)
)
print(num_ps)
res = tuneParams("regr.icb", task = tsk, resampling = rdesc_tune,
                 par.set = num_ps, control = ctrl)
res
# Op. pars: nu=0.169; epsilon=0.00172; bl2=btree; max_depth=24

lrn.icb = setHyperPars(makeLearner("regr.icb"), nu = res$x$nu, epsilon = res$x$epsilon,
                       bl2 = "btree", max_depth = 4L)
lrn.icb




# Tuning for randomForest
params <- makeParamSet(makeIntegerParam("mtry",lower = 2,upper = 5),
                       makeIntegerParam("nodesize",lower = 5,upper = 50),
                       makeIntegerParam("ntree", lower = 100, upper = 1000))

res_rf = tuneParams("regr.randomForest", task = oz.task, resampling = rdesc_tune,
                 par.set = params, control = ctrl)


# Tuning for gamboost
params_gamboost <- makeParamSet(makeIntegerParam("mstop", lower = 50, upper = 1000),
                                makeNumericParam("nu", lower = 0.01, upper = 0.2))

res_rf = tuneParams("regr.gamboost", task = oz.task, resampling = rdesc_tune,
                    par.set = params_gamboost, control = ctrl)
#Op. pars: mstop=61; nu=0.164  
  
# Multiple learners to be compared
lrns_regr = list(makeLearner("regr.gamboost"),
            makeLearner("regr.glmboost"),
            makeLearner("regr.rpart"),
            makeLearner("regr.svm"),
            makeLearner("regr.randomForest"),
            makeLearner("regr.xgboost")
            )

getParamSet(makeLearner("regr.lm"))

library(mlrHyperopt)
res_xgb = hyperopt(oz.task, learner = "regr.xgboost")
#Tune result:
# Op. pars: nrounds=211; max_depth=9; eta=0.237; gamma=4.21; 
# colsample_bytree=0.469; min_child_weight=0.774; subsample=0.61
res


rr = makeResampleDesc('CV', stratify = TRUE, iters = 10)

lrns.tuned = lapply(lrns_regr, function(lrn) {
  if (getLearnerName(lrn) == "xgboost") {
    # for xgboost we download a custom ParConfig from the Database
    pcs = downloadParConfigs(learner.name = getLearnerName(lrn))
    pc = pcs[[1]]
  } else {
    pc = getDefaultParConfig(learner = lrn)
  }
  ps = getParConfigParSet(pc)
  # some parameters are dependend on the data (eg. the number of columns)
  ps = evaluateParamExpressions(ps, dict = mlrHyperopt::getTaskDictionary(task = tsk))
  lrn = setHyperPars(lrn, par.vals = getParConfigParVals(pc))
  ctrl = makeTuneControlRandom(maxit = 20)
  makeTuneWrapper(learner = lrn, resampling = rr, par.set = ps, control = ctrl)
})





# Choose the resampling strategy
rdesc = makeResampleDesc("Holdout")
rdesc_v2 = makeResampleDesc("CV",iters=10)



# ### Manually looking for the optimal parameter
# 
# icb.learner0 = makeLearner("regr.icb",id="btree_0.1_0.05",par.vals = list(nu=0.1, epsilon = 0.05, bl2="btree", max_depth = 4))
# icb.learner1 = makeLearner("regr.icb",id="btree_0.1_0.025",par.vals = list(nu=0.1, epsilon = 0.025, bl2="btree", max_depth = 4))
# icb.learner2 = makeLearner("regr.icb",id="btree_0.1_0.01",par.vals = list(nu=0.1, epsilon = 0.01, bl2="btree", max_depth = 4))
# icb.learner3 = makeLearner("regr.icb",id="btree_0.1_0.005",par.vals = list(nu=0.1, epsilon = 0.005, bl2="btree", max_depth = 4))
# icb.learner4 = makeLearner("regr.icb",id="btree_0.1_0.001",par.vals = list(nu=0.1, epsilon = 0.001, bl2="btree", max_depth = 4))
# icb.learner4.1 = makeLearner("regr.icb",id="btree_0.1_0.0005",par.vals = list(nu=0.1, epsilon = 0.0005, bl2="btree", max_depth = 4))
# 
# 
# icb.learner5 = makeLearner("regr.icb",id="btree_0.05_0.05",par.vals = list(nu=0.05, epsilon = 0.05, bl2="btree", max_depth = 4))
# icb.learner6 = makeLearner("regr.icb",id="btree_0.05_0.025",par.vals = list(nu=0.05, epsilon = 0.025, bl2="btree", max_depth = 4))
# icb.learner7 = makeLearner("regr.icb",id="btree_0.05_0.01",par.vals = list(nu=0.05, epsilon = 0.01, bl2="btree", max_depth = 4))
# icb.learner8 = makeLearner("regr.icb",id="btree_0.05_0.005",par.vals = list(nu=0.05, epsilon = 0.005, bl2="btree", max_depth = 4))
# icb.learner9 = makeLearner("regr.icb",id="btree_0.05_0.001",par.vals = list(nu=0.05, epsilon = 0.001, bl2="btree", max_depth = 4))
# icb.learner9.1 = makeLearner("regr.icb",id="btree_0.05_0.0005",par.vals = list(nu=0.05, epsilon = 0.0005, bl2="btree", max_depth = 4))
# 
# 
# icb.learner10 = makeLearner("regr.icb",id="bbs_0.1_0.01",par.vals = list(nu=0.1, epsilon = 0.01, bl2="bbs", max_depth = 4))
# icb.learner11 = makeLearner("regr.icb",id="bbs_0.1_0.005",par.vals = list(nu=0.1, epsilon = 0.005, bl2="bbs", max_depth = 4))
# icb.learner12 = makeLearner("regr.icb",id="bbs_0.1_0.001",par.vals = list(nu=0.1, epsilon = 0.001, bl2="bbs", max_depth = 4))
# 
# 
# icb_list = list(icb.learner0,icb.learner1,icb.learner2,icb.learner3,icb.learner4,
#                 icb.learner5,icb.learner6,icb.learner7,icb.learner8,icb.learner9)
# 
# icb_list_2 = list(icb.learner2,icb.learner3,icb.learner4,icb.learner4.1,
#                   icb.learner7,icb.learner8,icb.learner9,icb.learner9.1)
# 
# icb_list_3 = list(icb.learner2,icb.learner3,icb.learner4,
#                   icb.learner10,icb.learner11,icb.learner12)




oz.list = list(lrn.icb,
               makeLearner("regr.lm"),
               makeLearner("regr.randomForest", 
                           par.vals = list(mtry = res_rf$x$mtry,nodesize = res_rf$x$nodesize, ntree = res_rf$x$ntree)),
)




################ Make benchmark #########################################################
bmr = benchmark(oz.list, oz.task, rdesc_v2)



## Visualize benchmark results ##################################

getBMRAggrPerformances(bmr)
plotBMRBoxplots(bmr,pretty.names = F)
getBMRPerformances(bmr, as.df = TRUE)


plotBMRBoxplots(bmr, measure = mse, order.lrn = getBMRLearnerIds(bmr))
#plotBMRSummary(bmr)
