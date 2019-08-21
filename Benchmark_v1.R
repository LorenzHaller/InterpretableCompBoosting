# Benchmark to compare test and train errors

# Airquality Data Example

setwd("C:/Users/halle/Downloads/Uni/Interpretable Machine Learning/InterpretableCompBoosting")

# Load data
data("airquality")
attach(airquality)
data <- na.omit(airquality)

# Split the data in training and test data (75/25 split)
set.seed(1488)
sample <- sample.int(n = nrow(data), size = floor(.5*nrow(data)), replace = F)
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
micb_500 = interpretable_comp_boost_m(train, formula, nu=0.005, mstop=1500, family=Gaussian(),
                                      epsilon = 0.0005)
avg_risk = micb_500$Risk / dim(train)[1]

source("icb_predict.R")
pred = icb_predict(icb_object = micb_500, newdata = test, target="Ozone")

avg_risk_test = pred$TestRisk / dim(test)[1]



# Plot the risk vs the number of iterations
plot(1:length(micb_500$Risk),avg_risk, xlab="Iteration",ylab="Risk",col="blue",type="l")
points(1:length(avg_risk_test),avg_risk_test,type="l",col="darkgreen")

