# Benchmark to compare test and train errors

# Airquality Data Example

setwd("C:/Users/halle/Downloads/Uni/Interpretable Machine Learning/InterpretableCompBoosting")

# Load data
data("airquality")
attach(airquality)
data <- na.omit(airquality)

# Split the data in training and test data (75/25 split)
set.seed(1492)
sample <- sample.int(n = nrow(data), size = floor(.66*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]

# Get model formula and prepare data
formula <- Ozone ~ Solar.R + Wind + Temp + Month + Day
formula <- terms.formula(formula)

#Load family.R
source("family.R")



##############################

# Applying combined method to data (MBOOST VERSION)
source("linearmodels_splines_trees_mboost.R")
micb_500 = interpretable_comp_boost_m(train, formula, nu=0.05, mstop=200, family=Gaussian(),
                                      epsilon = 0.0025)
avg_risk = micb_500$Risk / dim(train)[1]

source("icb_predict.R")
pred = icb_predict(icb_object = micb_500, newdata = test, target="Ozone")

avg_risk_test = pred$TestRisk / dim(test)[1]



# Plot the risk vs the number of iterations
plot(1:length(micb_500$Risk),avg_risk, xlab="Iteration",ylab="Risk",col="blue",type="l", 
     ylim=c(0,1000),xlim=c(0,micb_500$Input_Parameters[2]))
points(1:length(avg_risk_test),avg_risk_test,type="l",col="darkgreen")


library(mboost)

# Using mboost with splines
mboost_bols_bs = mboost::gamboost(formula = formula, data = train, baselearner = "bbs",
                                  control = boost_control(nu = 0.05, mstop = 200))

# Checking mboost with trees
mboost_tree = mboost::mboost(formula = formula, data = train, baselearner = "btree",
                             control = boost_control(nu = 0.05, mstop = 200))

# Combine to mboost using splines
points(1:length(mboost_bols_bs$risk()),mboost_bols_bs$risk()/dim(train)[1],type="l",col="red")
# Combine to mboost using trees
points(1:length(mboost_tree$risk()),mboost_tree$risk()/dim(train)[1],type="l",col="orange")

mb_spline_pred = mboost_bols_bs$predict(test)
h_pred=micb_500$Riskfunction(y=test$Ozone,f=mb_spline_pred)/dim(test)[1]
abline(h=h_pred)

mb_tree_pred = mboost_tree$predict(test)
h_pred_tree=micb_500$Riskfunction(y=test$Ozone,f=mb_tree_pred)/dim(test)[1]
abline(h=h_pred_tree)
#################################

## Visualize the individual coefficients of the three phases

## Linear coefficients

micb_500$Coefficients$Linear_coefficients
