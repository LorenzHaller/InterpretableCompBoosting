# Plots for the thesis

## Feature Importance and Partial Dependence Plot

library(iml)

# Load data
data("airquality")
attach(airquality)
airquality <- na.omit(airquality)


set.seed(187)
library("iml")
library("randomForest")
rf = randomForest(Ozone ~ ., data = airquality, ntree = 100)

X = airquality[which(names(airquality) != "Ozone")]
predictor = Predictor$new(rf, data = X, y = airquality$Ozone)

pdp = FeatureEffect$new(predictor = predictor, feature = "Temp" , method = "pdp")
plot(pdp)

ale = FeatureEffect$new(predictor = predictor, feature = "Temp" , method = "ale")
plot(ale)

imp = FeatureImp$new(predictor, loss = "mse")
plot(imp)
