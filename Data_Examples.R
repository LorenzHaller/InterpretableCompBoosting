icb_500 = interpretable_comp_boost(data, formula, nu=0.05, mstop=500, family=Gaussian(),
                          epsilon_rel_lin = 0.00001)

mboost_bols_bs = mboost::gamboost(formula = formula, data = data, baselearner = "bbs",
                                control = boost_control(nu = 0.05, mstop = 500, center=FALSE))
mboost_bols_bs$risk()
str(mboost_bols_bs)

?bbs


# Plot the risk vs the number of iterations
plot(1:length(icb_500$Risk),icb_500$Risk, xlab="Iteration",ylab="Risk",col="blue")
# Combine to mboost using splines
points(1:length(mboost_bols_bs$risk()),mboost_bols_bs$risk())












# data("Titanic")
# Titanic_data <- as.data.frame(Titanic)
# Titanic_data <- na.omit(Titanic_data)
# 
# formula <- Survived ~ Class + Sex + Age + Freq
# formula <- terms.formula(formula)
# 
# interpretable_comp_boost(data = Titanic_data, formula, nu=0.1, mstop=500, family=Binomial(),
#                          epsilon_rel_lin = 0.00001)
