# Applying the mlr wrapper

source("mlr_wrapper.R")

data(BostonHousing, package = "mlbench")
#excluded <- "chas"
#BostonHousing_num <- BostonHousing[,which(!colnames(BostonHousing) %in% excluded)]

regr.task = makeRegrTask(id = "bh", data = BostonHousing, target = "medv")

n = getTaskSize(regr.task)
train.set = seq(1, n, by = 2)
test.set = seq(2, n, by = 2)

regr.icb = makeLearner("regr.icb",par.vals = list(nu=0.1, epsilon = 0.05, bl2="btree", max_depth = 4))

#str(regr.icb)

mod.icb = mlr::train(regr.icb, task = regr.task, subset = train.set)

pred.icb = predict(mod.icb, task = regr.task, subset = test.set)

