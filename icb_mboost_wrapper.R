# Combine linear models and splines (using mboost code)

interpretable_comp_boost_m <- function(data, formula, nu=0.1, family=Gaussian(),
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
  
  feature_list = attr(formula, "variables")
  strsplit(as.character(feature_list), ",")
  
  # Extract feature name
  feature_str = names(mb_spline$coef()[1])
  feature_str = substring(feature_str, 5)
  mboost_feature = strsplit(feature_str, ",")[[1]][1]
  
  
  
  mb_tree = mboost(Ozone ~ btree(Solar.R, Wind, Temp, Month, Day, 
                                     tree_controls = ctrl), 
                     data = data, family = family, offset = fitted_values, 
                     control = boost_control(nu = nu, mstop = 1))
  
   
  
  while((iteration <= mstop) & ((risk_temp / risk_iter[iteration+1]) >= (1 + epsilon))){
    
    #Add one to the iteration number
    iteration <- iteration + 1
    
    #Calculate new risk for the current iteration (before updating the fitted values)
    risk_temp <- riskfct(y = y, f = fitted_values)
    
    # Calculate the new negative gradient 
    u <- ngradient(y = y, f = fitted_values)
    
    ############################################################################################
    # Trying to use mboost instead of own spline method
    data_temp[,target] <- as.numeric(data_temp[,target])
    mb_tree = mboost::mboost(formula = formula, data = data_temp, family = family, 
                                 baselearner = "btree", control = boost_control(nu = nu, mstop = 1))
    
    mb_tree$baselearner[[1]]$get_vary
    # Extract information from the mboost object
    # Extracting the spline coefficients
    mboost_coeff = mb_tree$coef()[[1]]
    # Extract feature name
    feature_str = names(mb_spline$coef()[1])
    feature_str = substring(feature_str, 5)
    mboost_feature = strsplit(feature_str, ",")[[1]][1]
    # Extract risk
    mboost_risk = mb_tree$risk()[2]
    
    # Update model parameters in original coefficients matrix
    coeff_list[[mboost_feature]] <- coeff_list[[mboost_feature]] + mboost_coeff
    
    # Save tree model to list
    tree_models[[iteration-transition_trees]] <- mb_tree
    
    # Update the fitted values
    fitted_values <- fitted_values + mb_tree$fitted()
    
    # Save the risk of the iteration
    risk_iter[iteration+1] <- riskfct(y = y, f = fitted_values)
    
    # Calculate the negative gradient and update the data frame
    data_temp[,target] <- ngradient(y = y, f = fitted_values)
    
  }
  
  
  ### Create a list to return 
  return_list <- list()
  return_list[["Coefficients"]] <- coeff_list
  return_list[["Fitted_Values"]] <- fitted_values
  return_list[["Transition Iterations"]] <-c(transition_splines,transition_trees)
  return_list[["Risk"]] <- risk_iter
  return_list[["Prediction_Models"]] <-c(linear_coefficients,spline_coefficients,tree_models)
  return_list[["Input_Parameters"]] <-c(nu, mstop, epsilon)
  return_list[["Data"]] <- X
  return_list[["Riskfunction"]] <- riskfct
  return_list[["Feature Statistics"]] <- feature_statistics
  
  # Print the coefficients of the final model
  return(return_list)


}