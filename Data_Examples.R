interpretable_comp_boost(data, formula, nu=0.05, mstop=500, family=Gaussian(),
                          epsilon_rel_lin = 0.00001)

mboost_bols_bs = mboost::mboost(formula = formula, data = data,
                                control = boost_control(nu = 0.05, mstop = 500))
mboost_bols_bs$risk()






















# data("Titanic")
# Titanic_data <- as.data.frame(Titanic)
# Titanic_data <- na.omit(Titanic_data)
# 
# formula <- Survived ~ Class + Sex + Age + Freq
# formula <- terms.formula(formula)
# 
# interpretable_comp_boost(data = Titanic_data, formula, nu=0.1, mstop=500, family=Binomial(),
#                          epsilon_rel_lin = 0.00001)
