interpretable_comp_boost(data, formula, nu=0.05, mstop=500, family=Gaussian(),
                          epsilon_rel_lin = 0.00001)

mboost_bols_bs = mboost::gamboost(formula = formula, data = data, baselearner = "bbs",
                                control = boost_control(nu = 0.05, mstop = 500, center=FALSE))
mboost_bols_bs$coef()
str(mboost_bols_bs)

?bbs



















# data("Titanic")
# Titanic_data <- as.data.frame(Titanic)
# Titanic_data <- na.omit(Titanic_data)
# 
# formula <- Survived ~ Class + Sex + Age + Freq
# formula <- terms.formula(formula)
# 
# interpretable_comp_boost(data = Titanic_data, formula, nu=0.1, mstop=500, family=Binomial(),
#                          epsilon_rel_lin = 0.00001)
