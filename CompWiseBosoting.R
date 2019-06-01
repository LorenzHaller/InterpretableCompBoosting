setwd("C:/Users/halle/Downloads/Uni/Interpretable Machine Learning")
source("family.R")

### Set input parameters ####

family = Gaussian()
weights = rep(1, NROW(response))


### CompWiseBoosting Function

comp_boosting = function(formula, data, mstop = 100, nu = 0.1){
  
  ### extract negative gradient and risk functions
  ngradient <- family@ngradient
  riskfct <- family@risk
  
  # coerce to data.frame
  data <- as.data.frame(data)
  
  # handle formula
  formula <- terms.formula(formula)
  
  # get the design matrix
  X <- model.matrix(formula, data)
  
  # extract target
  y <- data[, as.character(formula)[2]]
  
  # initial fit
  fit <- yhat.init
  
  # initialize the gradient with yhat.init
  u <- ngradient(y = y, yhat = fit)
  
  # initial working parameter (our theta)
  # this is just an empty body
  theta <- rep(0, ncol(X))
  
  # track loss
  loss <- c()
  
  
  
  # boost from 1 to stop
  for (i in 1:stop) {
    
    # estimate working parameter (via OLS)
    # theta_i = (X'X)^-1 X'y
    # This is the (base procedure) where you can literally place 
    # any optimization algorithm
    base_prod <- lm.fit(x = X, y = u)
    theta_i <- coef(base_prod)
    
    # update our theta
    theta <- theta + nu*as.vector(theta_i)
    
    # new fitted values
    fit <- fit + nu * fitted(base_prod)
    
    # update gradient
    u <- grad.fun(y = y, yhat = fit)
    
    # update loss 
    loss <- append(loss, loss.fun(y = y, yhat = fit))
  
  
}






RET <- list(mstop = mstop, nu = nu,
            risk = risk, stopintern = stopintern,
            center = center, trace = trace)