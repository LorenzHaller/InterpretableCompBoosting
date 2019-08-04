# Start by comparing to mboost tree method

mboost_btree = mboost::mboost(formula = formula, data = data, baselearner = "btree",
                           control = boost_control(mstop = 500))
str(mboost_btree)
mboost_btree$response
mboost_btree$basemodel

# Start building code using gbt-package

mf <- model.frame(formula=formula, data=data)
xx <- model.matrix(attr(mf, "terms"), data=mf)
yy <- model.response(mf)

mf
xx
yy
yy[[1]]

# create empty vector with all features (for coefficients)
ri <- xx[1,]
ri[] <- 0
ri

# process features
pf <- process.features(xx)
pf


#if classification
t<-table(y)
t
  if(length(t) != 2) {
    stop("The response must contain exactly 2 classes")
  }
  if(as.integer(names(t)[1]) != 0 || as.integer(names(t)[2]) != 1) {
    stop("The response must be either 0 or 1")
  }
  loss.integer <- 1
}
else {
  loss.integer <- 0
}