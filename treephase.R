# Start by comparing to mboost tree method

mboost_btree = mboost::mboost(formula = formula, data = data, baselearner = "btree",
                           control = boost_control(mstop = 500))
str(mboost_btree)
mboost_btree$response
mboost_btree$basemodel
