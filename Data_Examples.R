# Airquality Data Example

setwd("C:/Users/halle/Downloads/Uni/Interpretable Machine Learning")

# Load data
data("airquality")
attach(airquality)
data <- na.omit(airquality)

# Get model formula and prepare data
formula <- Ozone ~ Solar.R + Wind + Temp + Month + Day
formula <- terms.formula(formula)



# Applying combined method to data
icb_500 = interpretable_comp_boost(data, formula, nu=0.05, mstop=500, family=Gaussian(),
                          epsilon_rel_lin = 0.0001)

# Using only splines
scb_500 = splines_comp_boost(data, formula, nu=0.05, mstop=500, family=Gaussian(),
                    epsilon_rel_lin = 0.0001)

# Using mboost with splines
mboost_bols_bs = mboost::gamboost(formula = formula, data = data, baselearner = "bbs",
                                control = boost_control(nu = 0.05, mstop = 500, center=FALSE))
mboost_bols_bs$risk()
str(mboost_bols_bs)

?bbs


# Plot the risk vs the number of iterations
plot(1:length(icb_500$Risk),icb_500$Risk, xlab="Iteration",ylab="Risk",col="blue")
# Combine to mboost using splines
points(1:length(mboost_bols_bs$risk()),mboost_bols_bs$risk())
# Compare to own method using only splines
points(1:length(scb_500$Risk),scb_500$Risk,col="green")
# Add a legend to the plot
legend(100,110000, legend=c("Own method combined", "Own method using only splines", "Mboost using splines"),
       col=c("blue", "green","black"), lty=1:2, cex=0.8)










# data("Titanic")
# Titanic_data <- as.data.frame(Titanic)
# Titanic_data <- na.omit(Titanic_data)
# 
# formula <- Survived ~ Class + Sex + Age + Freq
# formula <- terms.formula(formula)
# 
# interpretable_comp_boost(data = Titanic_data, formula, nu=0.1, mstop=500, family=Binomial(),
#                          epsilon_rel_lin = 0.00001)
