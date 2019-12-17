# mlr wrapper benchmark for classification

setwd("C:/Users/halle/Downloads/Uni/Interpretable Machine Learning/InterpretableCompBoosting/R")

library(mlr)
library(OpenML)
library(mboost)
library(partykit)

source("mlr_wrapper.R")
source("xgboost_classif.R")



# Choose the resampling strategy
rdesc = makeResampleDesc("CV",iters=5)

################ Find a list of tasks for the benchmark #############################

taskinfo_all_classif = listOMLTasks(task.type = "Supervised Classification",
                            number.of.instances = c(500,10000),
                            number.of.features = c(5,30), number.of.classes = 2)
taskinfo_all_classif = taskinfo_all_classif[!duplicated(taskinfo_all_classif$name) & taskinfo_all_classif$number.of.instances.with.missing.values == 0,]

taskinfo_all_classif_num = taskinfo_all_classif[taskinfo_all_classif$number.of.symbolic.features < 2,]


################# Preparation of tasks used in the benchmark #########################

## Task 1: churn
churn.OML.task = getOMLTask(146227)
churn = churn.OML.task$input$data.set$data
churn = churn[!churn$number_customer_service_calls %in% c(8,9),]
churn.task = makeClassifTask(data = churn, target = "class")

## Task 2: Bank-marketing
bank.OML.task = getOMLTask(9899)
bank = bank.OML.task$input$data.set$data
bank.task = makeClassifTask(data = bank, target = "Class")

## Task 3: car
car.OML.task = getOMLTask(3854)
car = car.OML.task$input$data.set$data
car.task = makeClassifTask(data = car, target = "binaryClass")

## Task 4: Japanese Vowels
Jap.OML.task = getOMLTask(3839)
Jap = Jap.OML.task$input$data.set$data
Jap.task = makeClassifTask(data = Jap, target = "binaryClass")

## Task 5: delta_elevators
delta.OML.task = getOMLTask(3684)
delta = delta.OML.task$input$data.set$data
delta.task = makeClassifTask(data = delta, target = "binaryClass")

## Task 6: pollen
pollen.OML.task = getOMLTask(3775)
pollen = pollen.OML.task$input$data.set$data
pollen.task = makeClassifTask(data = pollen, target = "binaryClass")





####### Create Learners & Hyperparametertuning ################################################


tsk = churn.task
seed = 177

ctrl = makeTuneControlRandom(maxit = 30L)
inner = makeResampleDesc("CV", iters = 2L)
outer = makeResampleDesc("CV", iters = 5L)




# icb using tree stumps
num_ps_tree = makeParamSet(
  makeNumericParam("nu", lower = 0.001, upper = 0.2),
  makeNumericParam("epsilon", lower = 0.001, upper = 0.1),
  makeDiscreteLearnerParam(id = "bl2", default = "btree", values = c("btree"), tunable = F),
  makeIntegerLearnerParam(id = "max_depth", lower = 3, upper = 8, tunable = T)
)

icb.tree = makeTuneWrapper("classif.icb", resampling = inner, par.set = num_ps_tree,
                           control = ctrl)
set.seed(seed)
r = resample(icb.tree, tsk, resampling = outer, extract = getTuneResult)

# icb using splines
set.seed(seed)
num_ps_spline = makeParamSet(
  makeNumericParam("nu", lower = 0.001, upper = 0.2),
  makeNumericParam("epsilon", lower = 0.0005, upper = 0.1),
  makeDiscreteLearnerParam(id = "bl2", default = "bbs", values = c("bbs"), tunable = F),
  makeIntegerLearnerParam(id = "max_depth", lower = 3, upper = 8, tunable = T),
  makeIntegerLearnerParam(id = "df_spline", lower = 2L, upper = 5L, tunable = T)
)

icb.spline = makeTuneWrapper("classif.icb", resampling = inner, par.set = num_ps_spline,
                             control = ctrl)
set.seed(seed)
r.icb.spline = resample(icb.spline, tsk, resampling = outer, extract = getTuneResult)


# ksvm

ps_ksvm = makeParamSet(
  makeNumericParam("C", lower = -15, upper = 15, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -15, upper = 15, trafo = function(x) 2^x)
)

ksvm = makeTuneWrapper("classif.ksvm", resampling = inner, par.set = ps_ksvm,
                       control = ctrl, show.info = FALSE)
set.seed(seed)
r.ksvm = resample(ksvm, tsk, resampling = outer, extract = getTuneResult)



# random Forest

params.rf <- makeParamSet(makeIntegerParam("mtry",lower = 2,upper = 5),
                          makeIntegerParam("nodesize",lower = 5,upper = 50),
                          makeIntegerParam("ntree", lower = 100, upper = 1000))
rf = makeTuneWrapper("classif.randomForest", resampling = inner, par.set = params.rf,
                     control = ctrl, show.info = FALSE)
set.seed(seed)
r.rf = resample(rf, tsk, resampling = outer, extract = getTuneResult)


# linear model
logreg = makeLearner("classif.logreg")
set.seed(seed)
r.logreg = resample(logreg, tsk, resampling = outer)


# gamboost
params.gamboost <- makeParamSet(makeIntegerParam("mstop", lower = 50, upper = 1000),
                                makeNumericParam("nu", lower = 0.01, upper = 0.2)
)
gamb = makeTuneWrapper("classif.gamboost", resampling = inner, par.set = params.gamboost,
                       control = ctrl, show.info = FALSE)
set.seed(seed)
r.gamb = resample(gamb, tsk, resampling = outer, extract = getTuneResult)


# glmboost
params.glmboost <- makeParamSet(makeIntegerParam("mstop", lower = 50, upper = 1000),
                                makeNumericParam("nu", lower = 0.01, upper = 0.2))
glmb = makeTuneWrapper("classif.glmboost", resampling = inner, par.set = params.glmboost,
                       control = ctrl, show.info = FALSE)
set.seed(seed)
r.glmb = resample(glmb, tsk, resampling = outer, extract = getTuneResult)


# Tuning for an rpart tree
params.rpart = makeParamSet(
  makeIntegerParam("maxdepth", lower = 1, upper = 20),
  makeNumericParam("cp", lower = 0, upper = 1)
)
rpart = makeTuneWrapper("classif.rpart", resampling = inner, par.set = params.rpart,
                        control = ctrl, show.info = FALSE)
set.seed(seed)
r.rpart = resample(rpart, tsk, resampling = outer, extract = getTuneResult)


# Tuning for xgboost
params.xgboost = makeParamSet(
  makeIntegerParam ("max_depth" , lower = 1, upper = 10),
  makeIntegerParam("nrounds", lower = 1, upper = 1000)
)
xgb = makeTuneWrapper("classif.xgboost.mod", resampling = inner, par.set = params.xgboost,
                      control = ctrl, show.info = FALSE)
set.seed(seed)
r.xgb = resample(xgb, tsk, resampling = outer, extract = getTuneResult)



############################ RESULTS #################################################


results = c(r, r.icb.spline, r.logreg, r.gamb, r.glmb, r.rpart, r.rf, r.xgb, r.ksvm)
results

