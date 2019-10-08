######################################################################################
#### Benchmark to compare test and train errors: Airquality Data Example
######################################################################################

##### PREPARATION ####################################################################

setwd("C:/Users/halle/Downloads/Uni/Interpretable Machine Learning/InterpretableCompBoosting")

# Load data for Airquality Example
data("airquality")
attach(airquality)
data <- na.omit(airquality)
# Get model formula and prepare data
formula <- Ozone ~ Solar.R + Wind + Temp + Month + Day
formula <- terms.formula(formula)

# Load data for Boston Housing Example
data(BostonHousing, package = "mlbench")
data <- BostonHousing
formula <- medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + b + lstat
formula <- terms.formula(formula)


# Split the data in training and test data (75/25 split)
set.seed(2807)
sample <- sample.int(n = nrow(data), size = floor(.66*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]




#Load family.R
source("family.R")


### SET PARAMETERS ###################################################################

mstop_bm = 300
nu_bm = 0.05


##### MODEL TRAINING #################################################################

## OWN METHOD ########################################################################
# source("linearmodels_splines_trees_mboost.R")
# micb_500 = interpretable_comp_boost_m(train, formula, nu=nu_bm, mstop=mstop_bm, 
#                                       family=Gaussian(),epsilon = 0.001)
# avg_risk = micb_500$Risk / dim(train)[1]
# 
# # Make predictions
# source("icb_predict.R")
# pred = icb_predict(icb_object = micb_500, newdata = test, target="Ozone")
# 
# risk_test = pred$TestRisk / dim(test)[1]


### OWN METHOD MBOOST WRAPPER
source("icb_mboost_wrapper_offset.R")
micb_wrapper = interpretable_comp_boost_wrapper(train, formula, nu=0.1, 
                                            target_class = "Gaussian", bl2 = "btree",
                                            epsilon = 0.005)
avg_risk_wrapper = micb_wrapper$Risk

# Make predictions
source("icb_predict_wrapper_offset.R")
pred = icb_predict_wrapper(icb_object = micb_wrapper, newdata = test, target="medv")
avg_risk_test = pred$TestRisk

# Show results in table
source("helper_functions.R")
stage_risk(micb_object = micb_wrapper)
stage_risk(pred_object = pred)

# Visualize feature effects
source("pdp_function.R")
pdp_function(icb_object = micb_wrapper)

# Plot number of features over time
plot(1:length(micb_wrapper$Feature_Counter),micb_wrapper$Feature_Counter,type="l",
     ylab="Number of features",xlab="Iterations")



## MBOOST METHODS #######################################################################

library(mboost)
# Mboost with linear terms
mboost_bols = mboost(formula = formula, data = train, baselearner = "bols",
                     control = boost_control(nu = nu_bm, mstop = mstop_bm))
mb_bols_pred = mboost_bols$predict(test)
# Using mboost with splines
mboost_bols_bs = mboost::gamboost(formula = formula, data = train, baselearner = "bbs",
                                  control = boost_control(nu = nu_bm, mstop = mstop_bm))
mb_spline_pred = mboost_bols_bs$predict(test)
# Checking mboost with trees
mboost_tree = mboost::mboost(formula = formula, data = train, baselearner = "btree",
                             control = boost_control(nu = nu_bm, mstop = mstop_bm))
mb_tree_pred = mboost_tree$predict(test)

# # Look at partial dependence plot of mboost tree with depth=2
# ctrl = partykit::ctree_control(maxdepth = 2L)
# tree_formula <- paste(target, "~ btree(", feature_string, ",tree_controls = ctrl)")
# tree_formula <- as.formula(tree_formula)
# mb_tree_2 = mboost::mboost(formula = tree_formula, 
#                  data = train, family = family,
#                  control = boost_control(nu = nu_bm, mstop = mstop_bm))
# plot(mb_tree_2)
# environment(mb_tree_2$fitted)$ens[[1]]



##### VISUALISATION / COMPARISON #########################################################

##### Plot the risk vs the number of iterations 

plot(1:length(micb_wrapper$Risk),avg_risk_wrapper, xlab="Iteration",ylab="Average Risk",col="red",type="l", 
     ylim=c(0,max(micb_wrapper$Risk)),xlim=c(0,micb_wrapper$Input_Parameters[[2]]),main="Own method vs mboost with different base learners")
abline(v = micb_wrapper$`Transition Iterations`[1]+1)
abline(v = micb_wrapper$`Transition Iterations`[2]+1)
abline(v = micb_wrapper$`Transition Iterations`[3]+1)

points(1:length(avg_risk_test),avg_risk_test,type="b",col="red")

#Mboost using linear terms
points(1:length(mboost_bols$risk()),mboost_bols$risk()/dim(train)[1],type="l",col="brown")
h_pred_bols=micb_wrapper$Riskfunction(y=test$Ozone,f=mb_bols_pred)/dim(test)[1]
abline(h=h_pred_bols, col="brown")
# Mboost using splines
points(1:length(mboost_bols_bs$risk()),mboost_bols_bs$risk()/dim(train)[1],type="l",col="blue")
h_pred=micb_wrapper$Riskfunction(y=test$Ozone,f=mb_spline_pred)/dim(test)[1]
abline(h=h_pred, col="blue")
# Combine to mboost using trees
points(1:length(mboost_tree$risk()),mboost_tree$risk()/dim(train)[1],type="l",col="green")
h_pred_tree=micb_wrapper$Riskfunction(y=test$Ozone,f=mb_tree_pred)/dim(test)[1]
abline(h=h_pred_tree, col = "green")

# Add a legend to the plot
legend(60,1900, 
       legend=c("Own method", "Mboost using linear elements","Mboost using only own splines","Mboost using trees"),
       col=c("red", "brown","blue","green"), 
       lty=1:2, 
       cex=0.75)



### Plot visualising training and test risk vs the number of iterations

##### Plot the risk vs the number of iterations 

#points(1:length(micb_500$Risk),avg_risk,type="l",col="red")
#points(1:length(avg_risk_test),avg_risk_test,type="l",col="blue")

plot(1:length(micb_wrapper$Risk),avg_risk_wrapper, xlab="Iteration",ylab="Average Risk",col="red",type="l", 
     ylim=c(0,2000),xlim=c(0,micb_wrapper$Input_Parameters[[2]]), main="Training vs test risk for own method")
points(1:length(avg_risk_test),avg_risk_test,type="l",col="blue")
abline(v = micb_wrapper$`Transition Iterations`[1])
abline(v = micb_wrapper$`Transition Iterations`[2])
legend(60,1900, 
       legend=c("Training", "Test"),
       col=c("red","blue"), 
       lty=1:2, 
       cex=0.75)
points(1:length(avg_risk),avg_risk,type="l",col="green")
points(1:length(risk_test),risk_test,type="l",col="orange")


#################################

## Visualize the feature effects / individual coefficients of the three phases

# Compare to mboost plot
plot(mboost_bols_bs)
plot(mboost_tree)

source("pdp_function.R")
pdp_function(icb_object = micb_wrapper)


## Linear coefficients

micb_500$Coefficients$Linear_coefficients

abline(a = micb_500$Coefficients$Linear_coefficients[1], b = micb_500$Coefficients$Linear_coefficients[4])
## 

class(X_scaled)

# Just plot y and x as comparison
## scaled feature
plot(y=y,x=X_scaled[,4])
abline(a = micb_500$Coefficients$Linear_coefficients[1], b = micb_500$Coefficients$Linear_coefficients[4])
## original feature
plot(y=y,x=data$Temp)
as = micb_500$Coefficients$Linear_coefficients[[1]] * micb_500$`Feature Statistics`[2][4] + micb_500$`Feature Statistics`[1][4]
bs = micb_500$Coefficients$Linear_coefficients[4] * micb_500$`Feature Statistics`[2][4] 
abline(a = as, b = bs )

#Bs*Xmean/sdx

library(pdp)

lmtest <- lm(formula=formula, data=data)
