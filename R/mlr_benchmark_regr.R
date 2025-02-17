################## Mlr benchmark for regression ###############################

setwd("C:/Users/halle/Downloads/Uni/Interpretable Machine Learning/InterpretableCompBoosting/R")

library(mlr)
library(OpenML)
library(mboost)
library(partykit)

source("mlr_wrapper.R")
source("xgboost_regr.R")




################ Find a list of tasks for the benchmark #############################


taskinfo_all = listOMLTasks(task.type = "Supervised Regression", limit = NULL,
                            number.of.instances = c(500,10000),
                            number.of.features = c(5,30))

##### Remove duplicates by name
taskinfo_all = taskinfo_all[!duplicated(taskinfo_all$name) & taskinfo_all$number.of.instances.with.missing.values == 0,]

##### Filter for task with at least one categorical feature
taskinfo_all_cat = taskinfo_all[taskinfo_all$number.of.symbolic.features > 0,]


################# Preparation of tasks used in the benchmark #########################

## Task 1: Boston 
bh.OML.task = getOMLTask(4857)
bh = bh.OML.task$input$data.set$data
bh.task = makeRegrTask(data = bh, target = "MEDV")

## Task 2: cps_85_wages
wages.OML.task = getOMLTask(4859)
wages = wages.OML.task$input$data.set$data
wages.task = makeRegrTask(data = wages, target = "WAGE")

## Task 3: credit-g
creditg.OML.task = getOMLTask(146813)
creditg = droplevels(creditg.OML.task$input$data.set$data)
creditg$num_dependents <- as.factor(creditg$num_dependents)
creditg$installment_commitment <- as.factor(creditg$installment_commitment)
creditg$existing_credits <- as.factor(creditg$existing_credits)
creditg$residence_since <- as.factor(creditg$residence_since)
creditg.task = makeRegrTask(data = creditg, target = "credit_amount")

## Task 4: kin8nm 
oml.task_2280 = getOMLTask(2280)
kin8nm = oml.task_2280$input$data.set$data
kin8nm.task = makeRegrTask(data = kin8nm, target = "y")

## Task 5: puma8NH
puma.OML.task = getOMLTask(2313)
puma8NH = puma.OML.task$input$data.set$data
puma.task = makeRegrTask(data = puma8NH, target = "thetadd3")

## Task 6: Wine quality 
wine.OML.task = getOMLTask(4768)
wine_data = wine.OML.task$input$data.set$data
wine.task = makeRegrTask(data = wine_data, target = "quality")





####### Create Learners & Hyperparametertuning ################################################

tsk = creditg.task

seed = 179

ctrl = makeTuneControlRandom(maxit = 30L)
inner = makeResampleDesc("CV", iters = 2L)
outer = makeResampleDesc("CV", iters = 5L)


####### Hypertuning for icb method 

# icb using tree stumps
num_ps_tree = makeParamSet(
  makeNumericParam("nu", lower = 0.001, upper = 0.2),
  makeNumericParam("epsilon", lower = 0.0005, upper = 0.1),
  makeDiscreteLearnerParam(id = "bl2", default = "btree", values = c("btree"), tunable = F),
  makeIntegerLearnerParam(id = "max_depth", lower = 3, upper = 8, tunable = T)
)

icb.tree = makeTuneWrapper("regr.icb", resampling = inner, par.set = num_ps_tree,
                           control = ctrl)
set.seed(seed)
r = resample(icb.tree, tsk, resampling = outer, extract = getTuneResult)


# icb using splines
num_ps_spline = makeParamSet(
  makeNumericParam("nu", lower = 0.001, upper = 0.2),
  makeNumericParam("epsilon", lower = 0.0005, upper = 0.1),
  makeDiscreteLearnerParam(id = "bl2", default = "bbs", values = c("bbs"), tunable = F),
  makeIntegerLearnerParam(id = "max_depth", lower = 3, upper = 8, tunable = T),
  makeIntegerLearnerParam(id = "df_spline", lower = 2L, upper = 3L, tunable = T)
)

icb.spline = makeTuneWrapper("regr.icb", resampling = inner, par.set = num_ps_spline,
                           control = ctrl)
set.seed(seed)
r.icb.spline = resample(icb.spline, tsk, resampling = outer, extract = getTuneResult)


# ksvm
ps_ksvm = makeParamSet(
  makeNumericParam("C", lower = -15, upper = 15, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -15, upper = 15, trafo = function(x) 2^x)
)

ksvm = makeTuneWrapper("regr.ksvm", resampling = inner, par.set = ps_ksvm,
                      control = ctrl, show.info = FALSE)
set.seed(seed)
r.ksvm = resample(ksvm, tsk, resampling = outer, extract = getTuneResult)



# random Forest
params.rf <- makeParamSet(makeIntegerParam("mtry",lower = 2,upper = 5),
                       makeIntegerParam("nodesize",lower = 5,upper = 50),
                        makeIntegerParam("ntree", lower = 100, upper = 1000))
rf = makeTuneWrapper("regr.randomForest", resampling = inner, par.set = params.rf,
                     control = ctrl, show.info = FALSE)
set.seed(seed)
r.rf = resample(rf, tsk, resampling = outer, extract = getTuneResult)


# linear model
lm = makeLearner("regr.lm")
set.seed(seed)
r.lm = resample(lm, tsk, resampling = outer)


# gamboost
params.gamboost <- makeParamSet(makeIntegerParam("mstop", lower = 50, upper = 1000),
                                makeNumericParam("nu", lower = 0.01, upper = 0.2)
                                ,makeDiscreteParam("baselearner",values="btree")
                                )
gamb = makeTuneWrapper("regr.gamboost", resampling = inner, par.set = params.gamboost,
                     control = ctrl, show.info = FALSE)
set.seed(seed)
r.gamb = resample(gamb, tsk, resampling = outer, extract = getTuneResult)


# glmboost
params.glmboost <- makeParamSet(makeIntegerParam("mstop", lower = 50, upper = 1000),
                                makeNumericParam("nu", lower = 0.01, upper = 0.2))
glmb = makeTuneWrapper("regr.glmboost", resampling = inner, par.set = params.glmboost,
                       control = ctrl, show.info = FALSE)
set.seed(seed)
r.glmb = resample(glmb, tsk, resampling = outer, extract = getTuneResult)


# rpart tree
params.rpart = makeParamSet(
  makeIntegerParam("maxdepth", lower = 1, upper = 20),
  makeNumericParam("cp", lower = 0, upper = 1)
  )
rpart = makeTuneWrapper("regr.rpart", resampling = inner, par.set = params.rpart,
                       control = ctrl, show.info = FALSE)
set.seed(seed)
r.rpart = resample(rpart, tsk, resampling = outer, extract = getTuneResult)


# xgboost
params.xgboost = makeParamSet(
  makeIntegerParam ("max_depth" , lower = 1, upper = 10),
  makeIntegerParam("nrounds", lower = 1, upper = 1000)
)
xgb = makeTuneWrapper("regr.xgboost.mod", resampling = inner, par.set = params.xgboost,
                      control = ctrl, show.info = FALSE)
set.seed(seed)
r.xgb = resample(xgb, tsk, resampling = outer, extract = getTuneResult)



############################ RESULTS #################################################


results = c(r, r.icb.spline, r.lm, r.gamb, r.glmb, r.rpart, r.rf, r.xgb, r.ksvm)
results

