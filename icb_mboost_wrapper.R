# Combine linear models and splines (using mboost code)

interpretable_comp_boost_wrapper <- function(data, formula, nu=0.1, family=Gaussian(),
                                     epsilon = 0.0025){
  # data:     a data frame containing target and features
  # formula:  a formula specifying the model
  ## y:       the target variable
  ## X:       the feature matrix
  # nu:       the step size or shrinkage parameter (default = 0.1)
  # mstop:    the maximum number of iterations
  # family:   the family of the target variable (default = Gaussian)
  # epsilon:  necessary relative epsilon improvement for one iteration
  
  
  # Performing checks on the input parameters: formula, data, nu, mstop, family
  
  
  # Preparing the formula and data by seperating the target(y) and the features(X)
  formula_orig <- formula
  formula <- terms.formula(formula)
  X <- model.matrix(formula, data)
  y <- data[, as.character(formula)[2]]
  target <- all.vars(formula)[1]
  
  
  # # Load the gradient and risk function (using the mboost family.R code)
  source("family.R")
  ngradient <- family@ngradient
  riskfct <- family@risk
    
    
  # Initialize with Intercept model (similar to family@offset(y))
  intercept_model <- lm.fit(x=as.matrix(X[,1]), y=y)
  fit_0 <- intercept_model$fitted.values
  fitted_values <- fit_0
  
  # Calculate the risk of the intercept model 
  risk_0 <- riskfct(y = y, f = fitted_values)
  
  
  
  ### Phase 1: Linear models as base learners
  
  # Initialize the current iteration number
  iteration <- 1
  
  # Start with an initial mboost iteration
  mb_linear <- mboost::mboost(formula = formula, data = data, family = family, offset = fitted_values,
                             baselearner = "bols", control = boost_control(nu = nu, mstop = 1))
  
  while((mb_linear$risk()[iteration] / mb_linear$risk()[iteration+1]) >= (1 + epsilon)){
      
      #Add one to the iteration number
      iteration <- iteration + 1
      
      # Create next iteration 
      mb_linear <- mb_linear[iteration]
      
    }
    
  transition_splines <- iteration
  
  # Save fitted values of the last linear model iteration to use them as offset
  fitted_values <- mb_linear$fitted()
  
  
  
  ### Phase 2: Splines
  
  iteration <- iteration + 1 
  
  mb_spline = mboost::gamboost(formula = formula, data = data, family = family, 
                               offset = fitted_values, baselearner = "bbs", 
                               control = boost_control(nu = nu, mstop = 1))
  
  
  while((mb_spline$risk()[iteration-transition_splines] / mb_spline$risk()[iteration-transition_splines+1]) >= (1 + epsilon)){
    
    #Add one to the iteration number
    iteration <- iteration + 1
    
    # Create next iteration
    mb_spline <- mb_spline[iteration - transition_splines]
    
  }
  
  transition_trees <- iteration
  
  # Save fitted values of the last linear model iteration to use them as offset
  fitted_values <- mb_spline$fitted()
  
  
  
  ### Phase 3: Trees
  
  iteration <- iteration + 1 
  
  ctrl = partykit::ctree_control(maxdepth = 2L)
  
  
  # Extract feature name
  feature_string <- paste(colnames(X)[2:length(colnames(X))], collapse=", ")
  
  # Create formula for applying btree
  tree_formula <- paste(target, "~ btree(", feature_string, ",tree_controls = ctrl)")
  tree_formula <- as.formula(tree_formula)
  
  mb_tree = mboost(formula = tree_formula, 
                     data = data, family = family, offset = fitted_values, 
                     control = boost_control(nu = nu, mstop = 1))
  
  
  while((mb_tree$risk()[iteration-transition_trees] / mb_tree$risk()[iteration-transition_trees+1]) >= (1 + epsilon)){
    
    #Add one to the iteration number
    iteration <- iteration + 1
    
    mb_tree <- mb_tree[iteration - transition_trees]
    
  }
  
  
  ### Create a list to return 
  return_list <- list()
  return_list[["Coefficients"]] <- c(mb_linear$coef(), mb_spline$coef())
  return_list[["Fitted_Values"]] <- mb_tree$fitted()
  return_list[["Transition Iterations"]] <-c(transition_splines,transition_trees)
  return_list[["Risk"]] <- c(mb_linear$risk(),mb_spline$risk(),mb_tree$risk())
  return_list[["Prediction_Models"]] <-c(mb_linear,mb_spline,mb_tree)
  return_list[["Input_Parameters"]] <-c(nu, iteration, epsilon)
  return_list[["Data"]] <- X
  return_list[["Riskfunction"]] <- riskfct
  
  # Print the coefficients of the final model
  return(return_list)


}