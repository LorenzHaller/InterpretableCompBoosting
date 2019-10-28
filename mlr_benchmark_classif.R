# mlr wrapper benchmark for classification

library(mlr)
library(OpenML)
library(caret)
library(partykit)
library(mboost)
source("mlr_wrapper.R")
set.seed(177)

# Multiple learners to be compared
lrns.classif = list(makeLearner("classif.icb",par.vals = list(nu=0.1, epsilon = 0.005, bl2="btree", max_depth = 4)),
            makeLearner("classif.binomial"),
            #makeLearner("classif.gamboost"),
            makeLearner("classif.glmboost"),
            makeLearner("classif.rpart"),
            makeLearner("classif.gbm"),
            makeLearner("classif.ada"), 
            makeLearner("classif.kknn"),
            makeLearner("classif.svm"),
            makeLearner("classif.randomForest")
)


# Choose the resampling strategy
rdesc = makeResampleDesc("CV",iters=5)

# Make a task

taskinfo_all_classif = listOMLTasks(task.type = "Supervised Classification", limit = 50,
                            number.of.instances = c(1000,100000),
                            number.of.features = c(5,150), number.of.classes = 2)
taskinfo_all_classif

# Task 1: spam -> from mlr -> spam.task

# Task 2: credit
credit.OML.task = getOMLTask(31)
credit = na.omit(credit.OML.task$input$data.set$data)
credit.task = makeClassifTask(data = credit, target = "class")

# Task 3: electricity
electricity.OML.task = getOMLTask(219)
electricity = na.omit(electricity.OML.task$input$data.set$data)
electricity.task = makeClassifTask(data = electricity, target = "class")

# Task 4: BNG id 322
bng.OML.task = getOMLTask(206)
bng = na.omit(bng.OML.task$input$data.set$data)
bng.task = makeClassifTask(data = bng, target = "Class")

# Task 5: adult (2071)
mammo.OML.task = getOMLTask(3048)
mammo = na.omit(mammo.OML.task$input$data.set$data)
mammo.task = makeClassifTask(data = mammo, target = "class")


# Make benchmark
bmr.classif = benchmark(lrns.classif, mammo.task, rdesc_v2)


getBMRAggrPerformances(bmr.classif)
plt = plotBMRBoxplots(bmr.classif,pretty.names = F)
#levels(plt$data$learner.id) = c("LDA", "CART", "RF")
plt + theme(axis.text.x = element_text(angle = -75))
getBMRPerformances(bmr.classif, as.df = TRUE)