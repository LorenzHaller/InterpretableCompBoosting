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
  
  
  # # Load the gradient and risk function (using the mboost family.R code)
  source("family.R")
  ngradient <- family@ngradient
  riskfct <- family@risk
  
  
  # # Standardize features
  #   # Save mean and standard deviation of the features for later
  #   X_means <- apply(X, 2, mean)
  #   X_stds <- apply(X, 2, sd)
  #   feature_statistics <- list()
  #   feature_statistics[["Means"]] <- X_means
  #   feature_statistics[["StdDevs"]] <- X_stds
  #   # if all features are numeric
  #   X <- X
  #   X_lin <- X
  #   X_lin[,2:dim(X)[2]] <- scale(X)[,2:dim(X)[2]]
    
    
  # Initialize with Intercept model (similar to family@offset(y))
  intercept_model <- lm.fit(x=as.matrix(X[,1]), y=y)
  fit_0 <- intercept_model$fitted.values
  fitted_values <- fit_0
  
  # Calculate the risk of the intercept model 
  risk_0 <- riskfct(y = y, f = fitted_values)
  
  # Initialize a vector to save the risk values
  risk_iter <- numeric()
  risk_iter[1] <- risk_0
  
  
  
  ### Phase 1: Linear models as base learners
  
  # Set up vectors for the fit and the coefficients
  lm_fit <- numeric(dim(X)[2])
  lm_coeffs <- numeric(dim(X)[2])
  names(lm_coeffs) <- names(lm_fit) <- colnames(X)
  # Add the intercept to the model coefficients
  lm_coeffs[1] <- intercept_model$coefficients[1]
  
  # Set up a matrix for all the fitted values (one column for each feature)
  pred_matrix <- matrix(0, nrow = dim(X)[1], ncol = dim(X)[2])
  
  # Set up a list to save the linear coefficients for each iteration
  linear_coefficients <- list()
  
  # Save parameter of intercept model to list
  linear_coefficients[[1]] <- vector(mode = "numeric", length = dim(data)[2])
  names(linear_coefficients[[1]]) <- names(lm_coeffs)
  linear_coefficients[[1]][1] <- intercept_model$coefficients[[1]]
  
  # Initialize the current iteration number
  iteration <- 1
  
  # Start with an initial mboost iteration
  mb_linear <- mboost::mboost(formula = formula, data = data, family = family, offset = fitted_values,
                             baselearner = "bols", control = boost_control(nu = nu, mstop = 1))
  
  # Update the intercept 
  lm_coeffs[1] <- lm_coeffs[1] + mb_linear[iteration]$coef()[[1]][[1]]
  
  # Extract feature name
  feature_str <- names(mb_linear$coef()[1])
  feature_str <- substring(feature_str, 6)
  mboost_feature <- strsplit(feature_str, ")")[[1]][1]
  # Update the feature coefficient
  lm_coeffs[mboost_feature] <- lm_coeffs[mboost_feature] + mb_linear[iteration]$coef()[[1]][[2]]
  
  # Save parameter of intercept model to list
  linear_coefficients[[iteration+1]] <- vector(mode = "numeric", length = dim(data)[2])
  names(linear_coefficients[[iteration+1]]) <- names(lm_coeffs)
  linear_coefficients[[iteration+1]][mboost_feature] <- mb_linear[iteration]$coef()[[1]][[2]]
  
  # Extract risk
  risk_iter[iteration+1] <- mb_linear[iteration]$risk()[2]
  
  
  while((risk_iter[iteration] / risk_iter[iteration+1]) >= (1 + epsilon)){
      
      #Add one to the iteration number
      iteration <- iteration + 1
      
      # Update the intercept 
      lm_coeffs[1] <- lm_coeffs[1] + mb_linear[iteration]$coef()[[1]][[1]]
      
      # Extract feature name
      feature_str <- names(mb_linear[iteration]$coef()[1])
      feature_str <- substring(feature_str, 6)
      mboost_feature <- strsplit(feature_str, ")")[[1]][1]
      # Update the feature coefficient
      lm_coeffs[mboost_feature] <- lm_coeffs[mboost_feature][[1]] + mb_linear[iteration]$coef()[[1]][[2]]
      
      # Save parameter of intercept model to list
      linear_coefficients[[iteration+1]] <- vector(mode = "numeric", length = dim(data)[2])
      names(linear_coefficients[[iteration+1]]) <- names(lm_coeffs)
      linear_coefficients[[iteration+1]][mboost_feature] <- mb_linear[iteration]$coef()[[1]][[2]]
      
      # Extract risk
      risk_iter[iteration+1] <- mb_linear[iteration]$risk()[iteration+1]
      
    }
    
  transition_splines <- iteration
  
  ### Phase 2: Splines
  
  library(splines)
  library(mboost)
  
  # Create a working data set and set target variable
  target <- all.vars(formula)[1]
  data_temp <-  as.data.frame(cbind(data[,target],X[,-1]))
  colnames(data_temp) <- colnames(data)
  
  # create a vector to save the fit of all features
  spline_fit = numeric(dim(X)[2])
  names(spline_fit) <- colnames(X)
  
  # Create a list with the coefficients for all the features
  # as it is necessary for the splines.
  # The list consists of multiple vectors of length 24 (for the splines)
  coeff_list <- list()
  
  # Add the intercept from the linear model part
  coeff_list[["Intercept"]] <- lm_coeffs[1]
  names(coeff_list[["Intercept"]]) <- colnames(X)[1]
  
  # Add the linear coefficients from the first phase
  coeff_list[["Linear_coefficients"]] <- lm_coeffs
  
  # Add vectors of length 24 for all other features
  for (cn in 2:length(colnames(X))){
    coeff_list[[colnames(X)[cn]]] = vector(mode = "numeric", length = 24)
  }
  
  # Calculate the negative gradient and update the data frame
  data_temp[,target] <- ngradient(y = y, f = fitted_values)
  
  # Create a list for the temporary results 
  coeff_list_temp <- list()
  coeff_list_temp[["Intercept"]] <- vector(mode = "numeric", length = dim(X)[2])
  names(coeff_list_temp[["Intercept"]]) <- colnames(X)
  for (cn in 2:length(colnames(X))){
    coeff_list_temp[[colnames(X)[cn]]] = vector(mode = "numeric", length = 24)
  }
  # Create a list to save the spline mboost object per iteration
  spline_coefficients <- list()
  
  # Increase risk_temp to make first spline iteration possible
  risk_temp <- risk_temp * 2
  
  
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
    mb_spline = mboost::gamboost(formula = formula, data = data_temp, family = family, 
                                 baselearner = "bbs", control = boost_control(nu = nu, mstop = 1))
    
    # Extract information from the mboost object
    # Extracting the spline coefficients
    mboost_coeff = mb_spline$coef()[[1]]
    # Extract feature name
    feature_str = names(mb_spline$coef()[1])
    feature_str = substring(feature_str, 5)
    mboost_feature = strsplit(feature_str, ",")[[1]][1]
    # Extract risk
    mboost_risk = mb_spline$risk()[2]
    
    # Save the spline model of this iteration
    spline_coefficients[[iteration-transition_splines]] <- mb_spline
    
    # Update model parameters in original coefficients matrix
    coeff_list[[mboost_feature]] <- coeff_list[[mboost_feature]] + mboost_coeff
    
    # Update the fitted values
    fitted_values <- fitted_values + mb_spline$fitted()
    
    # Save the risk of the iteration
    risk_iter[iteration+1] <- riskfct(y = y, f = fitted_values)
    
    # Calculate the negative gradient and update the data frame
    data_temp[,target] <- ngradient(y = y, f = fitted_values)
    
  }
  
  transition_trees <- iteration
  
  ### Phase 3: Trees
  
  # Create a list to save the mboost tree model in every iteration
  tree_models <- list()
  
  # Increase risk_temp to make first spline iteration possible
  risk_temp <- risk_temp * 2 
  
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