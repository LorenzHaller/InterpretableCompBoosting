setwd("C:/Users/halle/Downloads/Uni/Interpretable Machine Learning/InterpretableCompBoosting")

# Laden der Daten

library(titanic)
data("titanic_train")
data("titanic_test")
data_train = titanic_train
data_validation = titanic_test

data <- as.data.frame(data_train)
data <- na.omit(data)

# Modellformel und Aufbereitung Daten

formula <- Survived ~ Pclass + Age + Fare
#formula <- terms.formula(formula)

class(titanic_train$Survived)

source("family.R")




### OWN METHOD MBOOST WRAPPER
source("icb_mboost_wrapper.R")
micb_wrapper = interpretable_comp_boost_wrapper(titanic_train, formula, nu=0.1, 
                                                family=Binomial(),epsilon = 0.001)
avg_risk_wrapper = micb_wrapper$Risk / dim(train)[1]

# Make predictions
source("icb_predict_wrapper.R")
pred = icb_predict_wrapper(icb_object = micb_wrapper, newdata = titanic_test, target="Ozone")
avg_risk_test = pred$TestRisk
