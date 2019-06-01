# Gradient Boosting

# loss function
# y - the target
# yhat - the fitted values

source("family.R")

### Set input parameters and derive gradient and loss/risk####

family = Gaussian()
ngradient <- family@ngradient
riskfct <- family@risk


#'www.github.com/andrebleier/cheapml

grad_boost <- function(formula, data, nu = 0.01, stop, 
                       grad.fun, loss.fun, yhat.init = 0) {
  
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
  u <- ngradient(y = y, f = fit)
  
  # initial working parameter (our theta)
  # this is just an empty body
  theta <- rep(0, ncol(X))
  
  # track risk/loss
  risk <- c()
  
  
  # How to reasonably stop the iterations before 'stop' is reached ?
  
  # boost from 1 to stop
  for (iter in 1:stop) {
    
    # estimate working parameter (via OLS)
    # theta_i = (X'X)^-1 X'y
    
    base_prod <- lm.fit(x = X, y = u)
    theta_iter <- coef(base_prod)
    
    # update our theta
    theta <- theta + nu*as.vector(theta_iter)
    
    # new fitted values
    fit <- fit + nu * fitted(base_prod)
    
    # update gradient
    u <- ngradient(y = y, f = fit)
    
    # update loss 
    risk <- append(risk, riskfct(y = y, f = fit))
    
  }
  
}