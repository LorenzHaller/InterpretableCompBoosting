# Applying the mlr wrapper for regression

source("mlr_wrapper.R")

data(BostonHousing, package = "mlbench")
#excluded <- "chas"
#BostonHousing_num <- BostonHousing[,which(!colnames(BostonHousing) %in% excluded)]

regr.task = makeRegrTask(id = "bh", data = BostonHousing, target = "medv")

n = getTaskSize(regr.task)
train.set = seq(1, n, by = 2)
test.set = seq(2, n, by = 2)

regr.icb = makeLearner("regr.icb",par.vals = list(nu=0.1, epsilon = 0.05, 
                                                  bl2="btree", max_depth = 4,
                                                  min_split = 20L, min_bucket = 7L))

#str(regr.icb)

mod.icb = mlr::train(regr.icb, task = regr.task, subset = train.set)

pred.icb = predict(mod.icb, task = regr.task, subset = test.set)


# Create partial dependence plots with mlr
pd = generatePartialDependenceData(mod.icb, regr.task, "lstat")
pd.lst = generatePartialDependenceData(mod.icb, regr.task, c("tax", "lstat"), TRUE)

plotPartialDependence(pd.lst, facet = "tax")




# Apply the mlr wrapper for a classification task

data(BreastCancer, package = "mlbench")
df = BreastCancer
df$Id = NULL
df = na.omit(df)


classif.task = makeClassifTask(id = "BreastCancer", data = df, target = "Class")

n.classif = getTaskSize(classif.task)
train.set.classif = seq(1, n.classif, by = 2)
test.set.classif = seq(2, n.classif, by = 2)

classif.icb = makeLearner("classif.icb",par.vals = list(nu=0.1, epsilon = 0.05, bl2="btree", max_depth = 4))

mod.icb.classif = mlr::train(classif.icb, task = classif.task, subset = train.set.classif)

pred.icb = predict(mod.icb.classif, task = classif.task, subset = test.set.classif)
