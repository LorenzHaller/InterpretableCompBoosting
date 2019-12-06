######################################################################################
#### Benchmark to compare test and train errors: Airquality Data Example
######################################################################################

##### PREPARATION ####################################################################

setwd("C:/Users/halle/Downloads/Uni/Interpretable Machine Learning/InterpretableCompBoosting")
# for cip pool
#setwd("H:/Interpretable Comp Boost/InterpretableCompBoosting")


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

# Bike Demand Data Kaggle
library(OpenML)
bike.OML.task = getOMLTask(7393)
bike = bike.OML.task$input$data.set$data
exclude_cols = c("datetime")
data = bike[,!colnames(bike) %in% exclude_cols]
formula <- count ~ time + season + holiday + workingday + weather + temp + atemp + humidity + windspeed + dayOfWeek

# Classification Data: 
bank.OML.task = getOMLTask(9899)
bank = bank.OML.task$input$data.set$data
#bank.task = makeClassifTask(data = bank, target = "Class")
data = bank
formula <- Class ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16


# Add another data set to explore interactions
# library(jtools)
# states <- as.data.frame(state.x77)
# data <- states
# colnames(data) <- c("Population","Income","Illiteracy","LifeExp",
#                     "Murder","HSGrad","Frost","Area")
# 
# formula <- Income ~ Population + Illiteracy + LifeExp + Murder + HSGrad + Frost + Area
# formula <- terms.formula(formula)



# Split the data in training and test data (75/25 split)
set.seed(280795)
sample <- sample.int(n = nrow(data), size = floor(.66*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]




#Load family.R
source("family.R")




##### MODEL TRAINING #################################################################


### OWN METHOD MBOOST WRAPPER
source("icb_mboost_wrapper_offset.R")
#source("icb_factors.R")
micb_wrapper = interpretable_comp_boost_wrapper(train, formula, nu=0.1, 
                                            target_class = "Gaussian", bl2 = "bbs",
                                            epsilon = 0.005, max_depth = 4)

# Make predictions
source("icb_predict_wrapper_offset.R")
#source("Icb_predict_factors.R")
pred = icb_predict_wrapper(icb_object = micb_wrapper, newdata = test, target="count")

# Show results in table
source("helper_functions.R")
stage_risk(micb_object = micb_wrapper)
stage_risk(pred_object = pred)

# Show results for multiple data sets
source("helper_functions.R")
train_list = list()
train_list[[1]] = test_Air
train_list[[2]] = test_bh
train_list[[3]] = test_bike
data_risk_table(icb_list = train_list, train = F, 
                data_names = c("Airquality","Boston Housing","Bike"))

# Show individual results for all observations in predict data
source("helper_functions.R")
individual_stage_risk(pred, subset = c(1,7,34))
individual_barplot(pred, subset = c(1,7,5,19,34))

# Visualize feature effects
source("pdp_function.R")
pdp_function(icb_object = micb_wrapper)

# Plot number of features over time
source("helper_functions.R")
plot.icb(micb_object = train_air, predict_object = test_air, fcount = T,
         data_name = "Airquality")
  
  



## MBOOST METHODS #######################################################################

### SET PARAMETERS ###################################################################

mstop_bm = 300
nu_bm = 0.05

library(mboost)
# Mboost with linear terms
mboost_bols = mboost::gamboost(formula = formula, data = train, baselearner = "bols",
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
ctrl = partykit::ctree_control(maxdepth = 2L)
feature_string <- paste(colnames(train)[- which(colnames(train) %in% target)], collapse=", ")
tree_formula <- paste(target, "~ btree(", feature_string, ",tree_controls = ctrl)")
tree_formula <- as.formula(tree_formula)
mb_tree_2 = mboost::mboost(formula = tree_formula,
                 data = train, 
                 control = boost_control(nu = nu_bm, mstop = mstop_bm))
plot(mb_tree_2)
mb_tree_2_pred = mb_tree_2$predict(test)
environment(mb_tree_2$fitted)$ens[[1]]



##### VISUALISATION / COMPARISON ######################################################

##### Plot the risk vs the number of iterations 

plot(1:length(micb_wrapper$Risk),avg_risk_wrapper, xlab="Iteration",ylab="Average Risk",col="red",type="l", 
     ylim=c(0,max(micb_wrapper$Risk)),xlim=c(0,micb_wrapper$Input_Parameters[[2]]),main="Own method vs mboost with different base learners")
abline(v = micb_wrapper$`Transition Iterations`[1]+1)
abline(v = micb_wrapper$`Transition Iterations`[2]+1)
abline(v = micb_wrapper$`Transition Iterations`[3]+1)
points(1:length(avg_risk_test),avg_risk_test,type="b",col="red")

#Mboost using linear terms
points(1:length(mboost_bols$risk()),mboost_bols$risk()/dim(na.omit(train))[1],type="l",col="brown")
h_pred_bols=micb_wrapper$Riskfunction(y=test$medv,f=mb_bols_pred)/dim(na.omit(test))[1]
abline(h=h_pred_bols, col="brown")
# Mboost using splines
points(1:length(mboost_bols_bs$risk()),mboost_bols_bs$risk()/dim(na.omit(train))[1],type="l",col="blue")
h_pred=micb_wrapper$Riskfunction(y=test$medv,f=mb_spline_pred)/dim(na.omit(test))[1]
abline(h=h_pred, col="blue")
# Combine to mboost using trees
points(1:length(mboost_tree$risk()),mboost_tree$risk()/dim(na.omit(train))[1],type="l",col="green")
h_pred_tree=micb_wrapper$Riskfunction(y=test$medv,f=mb_tree_pred)/dim(na.omit(test))[1]
abline(h=h_pred_tree, col = "green")
# Combine to mboost using trees of depth = 2
points(1:length(mb_tree_2$risk()),mb_tree_2$risk()/dim(na.omit(train))[1],type="l",col="orange")
h_pred_tree_2=micb_wrapper$Riskfunction(y=test$medv,f=mb_tree_2_pred)/dim(na.omit(test))[1]
abline(h=h_pred_tree_2, col = "orange")

# Add a legend to the plot
legend(60,80, 
       legend=c("Own method", "Mboost using linear elements","Mboost using only own splines",
                "Mboost using trees","Mboost using trees of depth = 2"),
       col=c("red", "brown","blue","green","orange"), 
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
