##############################################################################################
#### Example script to show the functionalities of Interpretable Component-wise Boosting
##############################################################################################

setwd("C:/Users/halle/Downloads/Uni/Interpretable Machine Learning/InterpretableCompBoosting/R")


#### Load packages ###########################################################################
library(mboost)
library(OpenML)
library(iml)
library(formattable)
library(partykit)
library(ggplot2)

#### Load scripts ############################################################################
source("family.R")
source("helper_functions.R")
source("icb.R")
source("icb_predict.R")
source("helper_functions.R")
source("pdp_function.R")
source("FunComplexity.R")
source("icb_plot.R")


#### Prepare the Data Examples ###############################################################

### Airquality Example
data("airquality")
attach(airquality)
data <- na.omit(airquality)
# Get model formula and prepare data
formula <- Ozone ~ Solar.R + Wind + Temp + Month + Day
formula <- terms.formula(formula)
target <- "Ozone"

### Boston Housing Example
data(BostonHousing, package = "mlbench")
data <- BostonHousing
formula <- medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + b + lstat
formula <- terms.formula(formula)
target <- "medv"

### Bike Demand Data Kaggle
bike.OML.task <- getOMLTask(7393)
bike <- bike.OML.task$input$data.set$data
exclude_cols <- c("datetime")
data <- bike[,!colnames(bike) %in% exclude_cols]
data <- data[data$weather != 4,]
data$weather <- factor(data[,5], levels = c(1,2,3))
formula <- count ~ time + season + holiday + workingday + weather + temp + atemp + humidity + windspeed + dayOfWeek
target <- "count"

### Bank Data (Classification)
bank.OML.task <- getOMLTask(9899)
bank <- bank.OML.task$input$data.set$data
data <- bank
formula <- Class ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16
target <- "Class"


### Split the data in training and test data (66/34 split)
set.seed(280798)
sample <- sample.int(n = nrow(data), size = floor(.66*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]







##### MODEL TRAINING #################################################################

icb.model = interpretable_comp_boost_wrapper(train, formula, nu=0.1, 
                                            target_class = "Gaussian", bl2 = "btree",
                                            epsilon = 0.005, max_depth = 4)

##### PREDICTING #####################################################################

pred = icb_predict_wrapper(icb_object = icb.model, newdata = test, target=target)


#### VISUALIZATION ###################################################################

# Show results for one data set in table
riskplot.icb(icb_object = icb.model, pred_object = pred)


# Show results for multiple data sets
train_list = list()
train_list[[1]] = air.model
train_list[[2]] = bh.model
train_list[[3]] = bike.model
riskplot.icb(icb_object = train_list, multiple = TRUE, 
             data_names = c("Airquality","Boston Housing","Bike"))


# Show individual results for all observations in predict data in a table
riskplot.icb(pred_object = pred, data_subset = c(1,7,27,181))

# Show individual results for all observations in predict data in a table
riskplot.icb(pred_object = pred, data_subset = c(1,7,27,181), type = "barplot", 
             plot.which = "Loss")
riskplot.icb(pred_object = pred, data_subset = c(1,7,27,181), type = "barplot", 
             plot.which = "Prediction")

# Plot number of features over time
riskplot.icb(icb_object = icb.model, pred_object = pred, fcount = T, type = "histogram",
         data_names = "Airquality")


################## Visualize feature effects ###########################################

# Plot Partial Dependence Plots
featureplot.icb(icb_object = icb.model, col = "black", type = "pdp")

# Plot Main Effect Complexity
featureplot.icb(icb.model, data = train, feature = "humidity", type = "ale")



## MBOOST METHODS #######################################################################

### SET PARAMETERS ###################################################################

mstop_bm = 300
nu_bm = 0.05

library(mboost)
# Mboost with linear terms

traintest=rbind(train,test)
X = sparse.model.matrix(as.formula(paste("count ~", paste(colnames(train[,colnames(train)!= "count"]), sep = "", collapse=" +"))), data = traintest)


mboost_bols = mboost::gamboost(formula = formula, data = droplevels(traintest[1:nrow(train),]), baselearner = "bols",
                     control = boost_control(nu = nu_bm, mstop = mstop_bm))
mb_bols_pred = mboost_bols$predict(droplevels(traintest[-(1:nrow(train)),]))

# Using mboost with splines
mboost_bols_bs = mboost::gamboost(formula = formula, data = train, baselearner = "bols",
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

plot(1:length(icb.model$Risk),avg_risk_wrapper, xlab="Iteration",ylab="Average Risk",col="red",type="l", 
     ylim=c(0,max(icb.model$Risk)),xlim=c(0,icb.model$Input_Parameters[[2]]),main="Own method vs mboost with different base learners")
abline(v = icb.model$`Transition Iterations`[1]+1)
abline(v = icb.model$`Transition Iterations`[2]+1)
abline(v = icb.model$`Transition Iterations`[3]+1)
points(1:length(avg_risk_test),avg_risk_test,type="b",col="red")

#Mboost using linear terms
points(1:length(mboost_bols$risk()),mboost_bols$risk()/dim(na.omit(train))[1],type="l",col="brown")
h_pred_bols=icb.model$Riskfunction(y=test$medv,f=mb_bols_pred)/dim(na.omit(test))[1]
abline(h=h_pred_bols, col="brown")
# Mboost using splines
points(1:length(mboost_bols_bs$risk()),mboost_bols_bs$risk()/dim(na.omit(train))[1],type="l",col="blue")
h_pred=icb.model$Riskfunction(y=test$medv,f=mb_spline_pred)/dim(na.omit(test))[1]
abline(h=h_pred, col="blue")
# Combine to mboost using trees
points(1:length(mboost_tree$risk()),mboost_tree$risk()/dim(na.omit(train))[1],type="l",col="green")
h_pred_tree=icb.model$Riskfunction(y=test$medv,f=mb_tree_pred)/dim(na.omit(test))[1]
abline(h=h_pred_tree, col = "green")
# Combine to mboost using trees of depth = 2
points(1:length(mb_tree_2$risk()),mb_tree_2$risk()/dim(na.omit(train))[1],type="l",col="orange")
h_pred_tree_2=icb.model$Riskfunction(y=test$medv,f=mb_tree_2_pred)/dim(na.omit(test))[1]
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

plot(1:length(icb.model$Risk),avg_risk_wrapper, xlab="Iteration",ylab="Average Risk",col="red",type="l", 
     ylim=c(0,2000),xlim=c(0,icb.model$Input_Parameters[[2]]), main="Training vs test risk for own method")
points(1:length(avg_risk_test),avg_risk_test,type="l",col="blue")
abline(v = icb.model$`Transition Iterations`[1])
abline(v = icb.model$`Transition Iterations`[2])
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
pdp_function(icb_object = icb.model)


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
