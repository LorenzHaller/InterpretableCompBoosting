# Combine linear models and splines

interpretable_comp_boost <- function(data, formula, nu=0.1, mstop=200, family=Gaussian(),
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
    
   
  
  
  
  
}