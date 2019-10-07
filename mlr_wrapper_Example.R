# Applying the mlr wrapper

source("mlr_wrapper.R")


data(BostonHousing, package = "mlbench")
excluded <- "chas"
BostonHousing_num <- BostonHousing[,which(!colnames(BostonHousing) %in% excluded)]

regr.task = makeRegrTask(id = "bh", data = BostonHousing_num, target = "medv")
regr.task
