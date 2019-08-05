# Combine linear models and splines (using mboost code)

interpretable_comp_boost_m <- function(data, formula, nu=0.1, mstop=200, family=Gaussian(),
                                     epsilon_rel_lin = 0.00001){
  # data: a data frame containing target and features
  # formula: a formula specifying the model
  ## y: the target variable
  ## X: the feature matrix
  # nu: the step size or shrinkage parameter (default = 0.1)
  # mstop: the maximum number of iterations
  # family: the family of the target variable (default = Gaussian)
  # epsilon_rel_lin: relative epsilon improvement for the linear part
  
  
  # Performing checks on the input parameters: formula, data, nu, mstop, family
  
  
  # Preparing the formula and data by seperating the target(y) and the features(X)
  formula <- terms.formula(formula)
  X <- model.matrix(formula, data)
  y <- data[, as.character(formula)[2]]
  
  
  # # Load the gradient and risk function (using the mboost family.R code)
  source("family.R")
  ngradient <- family@ngradient
  riskfct <- family@risk
  
  
  # Standardize features
    # if all features are numeric
    X_scaled <- X
    X_scaled[,2:dim(X)[2]] <- scale(X)[,2:dim(X)[2]]
    
    
  # Initialize with Intercept model (similar to family@offset(y))
  fit_0 <- numeric(dim(X_scaled)[1])
  intercept_model <- lm.fit(x=as.matrix(X_scaled[,1]), y=y)
  fit_0 <- intercept_model$fitted.values
  fitted_values <- fit_0
  
  # Calculate the risk of the intercept model and set an epsilon for the linear part
  risk_0 <- riskfct(y = y, f = fitted_values)
  epsilon_lin <- epsilon_rel_lin * risk_0
  
  # Initialize the current iteration number
  iteration <- 0
  
  # Initialize a vector to save the risk values
  risk_iter <- numeric()
  risk_iter[1] <- risk_0
  
  # Temporary risk
  risk_temp <- risk_0 + 2*epsilon_lin
  
  ### Phase 1: Linear models as base learners
  
  # Set up vectors for the fit and the coefficients
  lm_fit = numeric(dim(X_scaled)[2])
  lm_coeffs = numeric(dim(X_scaled)[2])
  names(lm_coeffs) <- names(lm_fit) <- colnames(X_scaled)
  # Set up a matrix for all the fitted values (one column for each feature)
  pred_matrix = matrix(0, nrow = dim(X_scaled)[1], ncol = dim(X_scaled)[2])
  # Add the intercept to the model coefficients
  lm_coeffs[1] <- intercept_model$coefficients[1]
  
  while((iteration <= mstop) & (risk_temp - risk_iter[iteration+1] >= epsilon_lin)){
      
      #Add one to the iteration number
      iteration <- iteration + 1
      
      #Calculate new risk for the current iteration (before updating the fitted values)
      risk_temp <- riskfct(y = y, f = fitted_values)
      
      # Calculate the new negative gradient 
      u <- ngradient(y = y, f = fitted_values)
      
      #Create a temporary coefficients vector
      lm_coeffs_temp = numeric(dim(X_scaled)[2])
      
      # Fit base learners for each feature to the negative gradient
      for(feat in 1:dim(X)[2]){
        # fit linear model for the current feature
        bl_model <- lm.fit(x=as.matrix(X_scaled[,feat]), y=u)
        # calculate the risk
        lm_fit[feat] <- riskfct(y=u, f=bl_model$fitted.values)
        # save the fitted values
        pred_matrix[,feat] <- bl_model$fitted.values
        # save the current model coefficient
        lm_coeffs_temp[feat] <- bl_model$coefficients
      }
      
      # Choose model with smallest loss
      model_select <- which.min(lm_fit)
      
      # Update model parameters
      lm_coeffs[model_select] <- lm_coeffs[model_select] + nu * lm_coeffs_temp[model_select]
      
      # Create the new fitted values using the features and the coefficients
      fitted_values <- X_scaled %*% lm_coeffs
      
      # Save the risk of the iteration
      risk_iter[iteration+1] <- riskfct(y = y, f = fitted_values)
      
    }
    
  
  ### Phase 2: Splines
  
  library(splines)
  
  # Create a working data set and set target variable
  data_temp <- data
  target <- all.vars(formula)[1]
  
  # create a vector to save the fit of all features
  spline_fit = numeric(dim(X_scaled)[2])
  names(spline_fit) <- colnames(X_scaled)
  
  # Create a list with the coefficients for all the features
  # as it is necessary for the splines.
  # The list consists of multiple vectors of length 24 (for the splines)
  coeff_list <- list()
  
  # Add the intercept from the linear model part
  coeff_list[["Intercept"]] <- lm_coeffs[1]
  names(coeff_list[["Intercept"]]) <- colnames(X_scaled)[1]
  
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
  names(coeff_list_temp[["Intercept"]]) <- colnames(X_scaled)
  for (cn in 2:length(colnames(X))){
    coeff_list_temp[[colnames(X)[cn]]] = vector(mode = "numeric", length = 24)
  }
  
  # Increase risk_temp to make first spline iteration possible
  risk_temp <- risk_temp + 2*epsilon_lin
  
  
  while((iteration <= mstop) & (risk_temp - risk_iter[iteration+1] >= epsilon_lin)){
    
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
    
    # Update model parameters in original coefficients matrix
    coeff_list[[mboost_feature]] <- coeff_list[[mboost_feature]] + nu * mboost_coeff
    
    # Update the fitted values
    fitted_values <- fitted_values + mb_spline$fitted()
    
    # Save the risk of the iteration
    risk_iter[iteration+1] <- riskfct(y = y, f = fitted_values)
    
    # Calculate the negative gradient and update the data frame
    data_temp[,target] <- ngradient(y = y, f = fitted_values)
    
  }
  
  
  ### Phase 3: Trees
  
  
    
  
  # Create a list to return 
  return_list <- list()
  return_list[["Coefficients"]] <- coeff_list
  return_list[["Fitted_Values"]] <- fitted_values
  return_list[["Risk"]] <- risk_iter
  
  # Print the coefficients of the final model
  return(return_list)


}