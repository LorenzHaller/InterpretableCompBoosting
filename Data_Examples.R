# Airquality Data Example

setwd("C:/Users/halle/Downloads/Uni/Interpretable Machine Learning/InterpretableCompBoosting")

# Load data
data("airquality")
attach(airquality)
data <- na.omit(airquality)

# Get model formula and prepare data
formula <- Ozone ~ Solar.R + Wind + Temp + Month + Day
formula <- terms.formula(formula)

#Load family.R
source("family.R")


# Applying combined method to data
source("linearmodels_splines.R")
icb_500 = interpretable_comp_boost(data, formula, nu=0.1, mstop=2000, family=Gaussian(),
                          epsilon_rel_lin = 0.00001)

# Applying combined method to data (MBOOST VERSION)
source("linearmodels_splines_mboost.R")
micb_500 = interpretable_comp_boost_m(data, formula, nu=0.1, mstop=2000, family=Gaussian(),
                                   epsilon_rel_lin = 0.00001)
# 
# # Using own method only with splines
# source("compboosting_splines.R")
# scb_500 = splines_comp_boost(data, formula, nu=0.1, mstop=500, family=Gaussian(),
#                     epsilon_rel_lin = 0.0001)

library(mboost)

# Using mboost with splines
mboost_bols_bs = mboost::gamboost(formula = formula, data = data, baselearner = "bbs",
                                control = boost_control(nu = 0.1, mstop = 2000))
#mboost_bols_bs$coef()

# Checking mboost with trees
mboost_tree = mboost::mboost(formula = formula, data = data, baselearner = "btree",
                                  control = boost_control(nu = 0.1, mstop = 500))
mboost_tree$`btree(Solar.R)`
str(mboost_tree)
risk()


# Plot the risk vs the number of iterations
plot(1:length(icb_500$Risk),icb_500$Risk, xlab="Iteration",ylab="Risk",col="blue",
     xlim=c(0,500),ylim=c(0,120000),type="l")
# Combine to mboost using splines
points(1:length(mboost_bols_bs$risk()),mboost_bols_bs$risk(),type="l")
# Compare to own method using only splines
#points(1:length(scb_500$Risk),scb_500$Risk,col="green")
# Compare to own method using only splines from mboost
points(1:length(micb_500$Risk),micb_500$Risk,col="red",type="l")
# Add a legend to the plot
legend(100,110000, legend=c("Own method combined", "Own method using MBOOST splines", "Mboost using splines"),
       col=c("blue", "red","black"), lty=1:2, cex=0.8)










# data("Titanic")
# Titanic_data <- as.data.frame(Titanic)
# Titanic_data <- na.omit(Titanic_data)
# 
# formula <- Survived ~ Class + Sex + Age + Freq
# formula <- terms.formula(formula)
# 
# interpretable_comp_boost(data = Titanic_data, formula, nu=0.1, mstop=500, family=Binomial(),
#                          epsilon_rel_lin = 0.00001)
