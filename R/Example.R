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

icb.model = icb(train, formula, nu=0.1, target_class = "Gaussian", bl2 = "btree",
                                            epsilon = 0.005, max_depth = 4)

##### PREDICTING #####################################################################

pred = icb_predict(icb_object = icb.model, newdata = test, target=target)


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

