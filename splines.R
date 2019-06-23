# Splines
library(splines)

spline_model_boost <- function(y=y, X=X, nu=0.1, mstop=100000, family=Gaussian()){
  
  # Create a working data set and set target variable
  data_temp <- data
  target <- all.vars(formula)[1]
  #colnames_temp = c(colnames(data),"(Intercept)")
  
  # Load Gradient- and Loss/Riskfunction
  source("family.R")
  ngradient <- family@ngradient
  riskfct <- family@risk
  
  # Standardize features
  
  # if all features are numeric
  X_scaled <- X
  X_scaled[,2:dim(X)[2]] <- scale(X)[,2:dim(X)[2]]
  
  # only numeric features
  # numeric_features = c()
  # X <- X %>% mutate_each_(funs(scale(.) %>% as.vector), 
  #                             vars=numeric_features)
  
  
  # Initialize with Intercept model (similar to family@offset(y))
  
  fit_0 <- numeric(dim(X_scaled)[1])
  intercept_model <- lm.fit(x=as.matrix(X_scaled[,1]), y=y)
  fit_0 <- intercept_model$fitted.values
  
  # init_zero <- rep(0, dim(X)[1])
  # init_zero
  
  fitted_values <- fit_0
  
  # Specify set of base learners: For now just splines
  
  
  # Set up vectors
  
  # create a vector to save the fit of all features
  spline_fit = numeric(dim(X_scaled)[2]-1)
  # create a vector to save all feature coeffcients
  spline_coeffs = numeric(dim(X_scaled)[2])
  # create a matrix to save all fitted values
  pred_matrix = matrix(0, nrow = dim(X_scaled)[1], ncol = dim(X_scaled)[2])
  # assign the column names
  names(spline_coeffs) <- names(spline_fit) <- colnames(X_scaled)
  # add the intercept coefficient to the coefficient vector
  spline_coeffs[1] <- intercept_model$coefficients[1]
  
  
  for(i in 1:mstop){
    # Calculate the negative gradient and update the data frame
    
    data_temp[,target] <- u <- ngradient(y = y, f = fitted_values)
    
  
  
    # Extract the target from the data for the formula
    #target = eval(colnames(data)[1])
    
    
    # Fit base learners to the negative gradient
    
    spline_coeffs_temp = numeric(dim(X_scaled)[2])
    
    for(feat in 1:(dim(data)[2] - 1)){
      
      # Create a formula for the current feature
      feature = eval(colnames(data)[feat+1])
      feature_spline = paste("bs(", feature, ")")
      formula_temp = as.formula(paste(target, feature_spline, sep = " ~ "))
      formula_temp = terms.formula(formula_temp)
      formula_temp
      
      # fit the model
      bl_model <- lm(formula = formula_temp, data = data_temp)
      # calculate the fit and save it
      spline_fit[feat] <- riskfct(y=u, f=bl_model$fitted.values)
      # fill the matrix of all fitted values for all features
      pred_matrix[,feat] <- bl_model$fitted.values
      # save the coefficients
      #spline_coeffs_temp[feat] <- bl_model$coefficients
    }
    
    # Choose model with smallest loss
    model_select <- which.min(spline_fit)
    
    # if(spline_coeffs_temp[model_select] < 0.05){
    #   model_select <- order(spline_fit)[2]
    # }
    
    # Create new fitted values by model
    best_model_fit <- lm.fit(x=as.matrix(X_scaled[,model_select]), y=u)$fitted.values
    
    # Update model parameters
    spline_coeffs[model_select] <- spline_coeffs[model_select] + nu * spline_coeffs_temp[model_select]
    fitted_values <- X_scaled %*% spline_coeffs
    
  }
  
  # Print the coefficients of the final model
  print(spline_coeffs)
}

# Execute function
linear_model_boost(y=y, X=X, nu=0.1, mstop=1000, family=Gaussian())

# COmpare to mboost
mb = mboost(formula=formula, data=data, baselearner = "bbs")


spline_fit_test = lm(Ozone ~ Solar.R + bs(Wind, df = 4) + Temp + Month + Day, data = data )
spline_fit_test
b