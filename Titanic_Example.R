setwd("C:/Users/halle/Downloads/Uni/Interpretable Machine Learning/InterpretableCompBoosting")

# Laden der Daten

library(titanic)
data("titanic_train")
data("titanic_test")

# data_dummy = dummyVars(" ~ .", data = titanic_train)
# trsf <- data.frame(predict(data_dummy, newdata = titanic_train))

data_train = titanic_train
data_validation = titanic_test

data <- as.data.frame(data_train)
data <- na.omit(data)

# Modellformel und Aufbereitung Daten

formula <- Survived ~ Pclass + Age + Fare
#formula <- terms.formula(formula)

class(titanic_train$Survived)

source("family.R")
source("helper_functions.R")

# Split the data in training and test data (75/25 split)
set.seed(12)
sample <- sample.int(n = nrow(data), size = floor(.5*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]


### OWN METHOD defined as MBOOST WRAPPER 
source("icb_mboost_wrapper_offset.R")
micb_wrapper = interpretable_comp_boost_wrapper(train, formula, nu=0.1, 
                                                target_class="Binomial",epsilon = 0.0001)
avg_risk_wrapper = micb_wrapper$Risk / dim(na.omit(titanic_train))[1]
avg_label_risk = micb_wrapper$LabelRisk / dim(na.omit(titanic_train))[1]

# Make predictions
source("icb_predict_wrapper_offset.R")
pred = icb_predict_wrapper(icb_object = micb_wrapper, newdata = test, target="Survived")
avg_risk_test = pred$TestRisk
avg_test_label_risk = pred$TestLabelRisk



##### Plot the risk vs the number of iterations 

plot(1:length(micb_wrapper$Risk),avg_risk_wrapper, xlab="Iteration",ylab="Average Risk",col="red",type="l", 
     xlim=c(0,micb_wrapper$Input_Parameters[[2]]),ylim=c(0,max(avg_risk_wrapper,avg_risk_test,avg_label_risk,avg_test_label_risk)),
     main="Own method vs mboost with different base learners")
abline(v = micb_wrapper$`Transition Iterations`[1])
abline(v = micb_wrapper$`Transition Iterations`[2])
points(1:length(avg_risk_test),avg_risk_test,type="l",col="blue")
points(1:length(avg_label_risk),avg_label_risk,type="b",col="red")
points(1:length(avg_test_label_risk),avg_test_label_risk,type="b",col="blue")

