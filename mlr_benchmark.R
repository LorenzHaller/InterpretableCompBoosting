################## Mlr benchmark for regression ###############################


library(mlr)
library(OpenML)
source("mlr_wrapper.R")
set.seed(177)
#devtools::install_github("jakob-r/mlrHyperopt", dependencies = TRUE)
library(mlrHyperopt)
library(caret)
library(mboost)
library(partykit)



###### Find a list of tasks for the benchmark #############################


taskinfo_all = listOMLTasks(task.type = "Supervised Regression", limit = NULL,
                            number.of.instances = c(1000,100000),
                            number.of.features = c(5,150))

taskinfo_all_v3 = taskinfo_all[taskinfo_all$number.of.symbolic.features > 0,]

# Task IDs for benchmark:
## mv 4774
## visualizing_soil 4999

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

## TAsk 6: mv
mv.OML.task = getOMLTask(4774)
mv = mv.OML.task$input$data.set$data
mv.task = makeRegrTask(data = mv, target = "y")


# Create list of all tasks
tasks = list(bh.task, oz.task, kin8nm.task, wine.task, puma.task)








####### Hyperparametertuning Part ##################################################

tsk = mv.task
ctrl = makeTuneControlRandom(maxit = 30L)
rdesc_tune = makeResampleDesc("CV", iters = 3L)


####### Hypertuning for the parameters epsilon and nu for icb method ######

num_ps = makeParamSet(
  makeNumericParam("nu", lower = 0.001, upper = 0.2),
  makeNumericParam("epsilon", lower = 0.0005, upper = 0.1),
  makeDiscreteLearnerParam(id = "bl2", default = "btree", values = c("btree"), tunable = F),
  makeIntegerLearnerParam(id = "max_depth", lower = 3, upper = 8, tunable = F),
  makeIntegerLearnerParam(id = "min_split",  lower = 5L, upper = 30L, tunable = F),
  makeIntegerLearnerParam(id = "min_bucket", lower = 2L, upper = 15L, tunable = F)
)
print(num_ps)
res = tuneParams("regr.icb", task = tsk, resampling = rdesc_tune,
                 par.set = num_ps, control = ctrl)
res
### Op. pars: nu=0.169; epsilon=0.00172; bl2=btree; max_depth=24
# Tune result for bh.task with 30 runs:
#   Op. pars: nu=0.199; epsilon=0.0459; bl2=btree; max_depth=6; min_split=8; min_bucket=8
# mse.test.mean=12.6906784

lrn.icb = setHyperPars(makeLearner("regr.icb"), nu = res$x$nu, epsilon = res$x$epsilon,
                       bl2 = "btree", max_depth = res$x$max_depth, 
                       min_split = res$x$min_split, min_bucket = res$x$min_bucket )
lrn.icb




# # Tuning for randomForest
# params <- makeParamSet(makeIntegerParam("mtry",lower = 2,upper = 5),
#                        makeIntegerParam("nodesize",lower = 5,upper = 50),
#                        makeIntegerParam("ntree", lower = 100, upper = 1000))
# 
# res_rf = tuneParams("regr.randomForest", task = tsk, resampling = rdesc_tune,
#                  par.set = params, control = ctrl)


# Tuning for gamboost
params_gamboost <- makeParamSet(makeIntegerParam("mstop", lower = 50, upper = 1000),
                                makeNumericParam("nu", lower = 0.01, upper = 0.2))

res_gamb = tuneParams("regr.gamboost", task = tsk, resampling = rdesc_tune,
                    par.set = params_gamboost, control = ctrl)

regr.gamboost = setHyperPars(makeLearner("regr.gamboost"), mstop = res_gamb$x$mstop, 
                             nu = res_gamb$x$nu)
#Op. pars: mstop=61; nu=0.164  


# Tuning for glmboost
res_glmb = hyperopt(tsk, learner = "regr.glmboost")
regr.glmboost = setHyperPars(makeLearner("regr.glmboost"), mstop = res_glmb$x$mstop,
                             nu = res_glmb$x$nu) 

# Tuning for an rpart tree
res_rpart = hyperopt(tsk, learner = "regr.rpart")
regr.rpart = setHyperPars(makeLearner("regr.rpart"), cp = res_rpart$x$cp,
                          maxdepth = res_rpart$x$maxdepth, minbucket = res_rpart$x$minbucket,
                          minsplit = res_rpart$x$minsplit)

# Tuning for SVM
res_svm = hyperopt(tsk, learner = "regr.svm")
regr.svm = setHyperPars(makeLearner("regr.svm"), cost = res_svm$x$cost,
                        gamma = res_svm$x$gamma) 

# Tuning for Random Forest
res_rf = hyperopt(tsk, learner = "regr.randomForest")
regr.rf = setHyperPars(makeLearner("regr.randomForest"), nodesize = res_rf$x$nodesize,
                       mtry = res_rf$x$mtry)

# Tuning for xgboost
res_xgb = hyperopt(tsk, learner = "regr.xgboost")
regr.xgboost = setHyperPars(makeLearner("regr.xgboost"), nrounds = res_xgb$x$nrounds,
                            max_depth = res_xgb$x$max_depth,
                            eta = res_xgb$x$eta,
                            gamma = res_xgb$x$gamma,
                            colsample_bytree = res_xgb$x$colsample_bytree,
                            min_child_weight = res_xgb$x$min_child_weight,
                            subsample = res_xgb$x$subsample)


# Extract the parameters from a single learner
getParamSet(makeLearner("regr.svm"))



#Tune result:
# Op. pars: nrounds=211; max_depth=9; eta=0.237; gamma=4.21; 
# colsample_bytree=0.469; min_child_weight=0.774; subsample=0.61
res


# rr = makeResampleDesc('CV', stratify = TRUE, iters = 10)
# 
# lrns.tuned = lapply(lrns_regr, function(lrn) {
#   if (getLearnerName(lrn) == "xgboost") {
#     # for xgboost we download a custom ParConfig from the Database
#     pcs = downloadParConfigs(learner.name = getLearnerName(lrn))
#     pc = pcs[[1]]
#   } else {
#     pc = getDefaultParConfig(learner = lrn)
#   }
#   ps = getParConfigParSet(pc)
#   # some parameters are dependend on the data (eg. the number of columns)
#   ps = evaluateParamExpressions(ps, dict = mlrHyperopt::getTaskDictionary(task = tsk))
#   lrn = setHyperPars(lrn, par.vals = getParConfigParVals(pc))
#   ctrl = makeTuneControlRandom(maxit = 20)
#   makeTuneWrapper(learner = lrn, resampling = rr, par.set = ps, control = ctrl)
# })





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




lrn.list = list(lrn.icb,
               makeLearner("regr.lm"),
               regr.gamboost,
               regr.glmboost,
               regr.rpart,
               regr.svm,
               regr.rf
               #,regr.xgboost
               )




################ Make benchmark #########################################################
bmr = benchmark(lrn.list, tsk, rdesc_v2)



## Visualize benchmark results ##################################

getBMRAggrPerformances(bmr)
plotBMRBoxplots(bmr,pretty.names = T)
getBMRPerformances(bmr, as.df = TRUE)


plotBMRBoxplots(bmr, measure = mse, order.lrn = getBMRLearnerIds(bmr))
#plotBMRSummary(bmr)
