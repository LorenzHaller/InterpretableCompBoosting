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

# Split the data in training and test data (75/25 split)
set.seed(1995)
sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]


### OWN METHOD MBOOST WRAPPER
source("icb_mboost_wrapper_offset.R")
micb_wrapper = interpretable_comp_boost_wrapper(train, formula, nu=0.1, 
                                                target_class="Binomial",epsilon = 0.001)
avg_risk_wrapper = micb_wrapper$Risk / dim(titanic_train)[1]

predicted_train_labels <- numeric(length(micb_wrapper$Fitted_Values))
for(lp in 1:length(predicted_labels)){
  if(micb_wrapper$Fitted_Values[lp] < 0){
    predicted_train_labels[lp] <- 0
  } else{
    predicted_train_labels[lp] <- 1
  }
}

riskfct(y=train$Survived,f=predicted_train_labels) / dim(train)[1]

# Make predictions
source("icb_predict_wrapper_offset.R")
pred = icb_predict_wrapper(icb_object = micb_wrapper, newdata = test, target="Survived")
avg_risk_test = pred$TestRisk
riskfct(y=test$Survived,f=pred$`Predicted Labels`) / dim(test)[1]

##### Plot the risk vs the number of iterations 

plot(1:length(micb_wrapper$Risk),avg_risk_wrapper, xlab="Iteration",ylab="Average Risk",col="red",type="l", 
     xlim=c(0,micb_wrapper$Input_Parameters[[2]]),ylim=c(0,max(avg_risk_wrapper,avg_risk_test)),
     main="Own method vs mboost with different base learners")
abline(v = micb_wrapper$`Transition Iterations`[1])
abline(v = micb_wrapper$`Transition Iterations`[2])
points(1:length(avg_risk_test),avg_risk_test,type="p",col="red")

