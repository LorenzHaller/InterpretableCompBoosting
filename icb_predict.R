icb_predict <- function(icb_object, newdata, target){
  # icb_object:     output object from the interpretable comp boosting function
  # newdata:        newdata that will be used to make predictions
  # target:         string that can specify the target variable; should be filled if newdata 
                    # contains the target variable -> inner risk will be computed
  
  # X <- model.matrix(formula, newdata)
  #X_new <- cbind(1, newdata)
  X_new <- newdata
  
  if(! is.null(target)){
    y <- newdata[,target]
    #X_new <- newdata[,-target]
    # Scaling for newdata according to scaling for training data
    X_new_lin <- scale(X_new)
  } else{
    # Scaling for newdata according to scaling for training data
    X_new_lin <- scale(X_new)
  }
  
  # Create an empty numeric vector for saving the test risk results
  test_risk <- numeric(length(micb_500$Risk))
  
  # Create an empty vector with the length of newdata
  prediction <- vector(mode = "numeric", length = dim(newdata)[1])
  
  # Fill every entry with the intercept of the model
  for (l in 1:length(prediction)){
    prediction[l] <- icb_object$Coefficients$Intercept[[1]]
  }
  
  test_risk[1] <- icb_object$Riskfunction(y = y, f = prediction)
  
  iteration <- 1
  nu <- icb_object$Input_Parameters[1]
  
  # For the linear part:
  
  ## Version 1: per iteration
  while(iteration <= (icb_object$`Transition Iterations`[1]+1)){
    
    # Get the name and position of the selected feature
    selected_feature <- names(which(icb_object$Prediction_Models[[iteration]] != 0))
    pos_feature <- which(icb_object$Prediction_Models[[iteration]] != 0)[[1]]
    
    # Multiply the values of the selected feature with its coefficient
    pred_iteration <- X_new_lin[,selected_feature] * icb_object$Prediction_Models[[iteration]][pos_feature]
    
    
    prediction <- prediction + nu * pred_iteration
    
    # Calculate the risk in this iteration
    test_risk[iteration+1] <- icb_object$Riskfunction(y = y, f = prediction)
    
    iteration <- iteration + 1
  
  }
  
  ## Version 2: use result of linear coefficients (uses one specific nu)
  #prediction <- as.matrix(cbind(1, X_new[,-1])) %*% icb_object$Coefficients$Linear_coefficients 
  
  
  
  # For the splines part:
  while(iteration <= (icb_object$`Transition Iterations`[2]+1)){
    
    mboost_spline_model <- icb_object$Prediction_Models[[iteration]]
    pred_iteration <- mboost_spline_model$predict(newdata = as.data.frame(X_new))
    
    prediction <- prediction + pred_iteration
    
    
    # Calculate the risk in this iteration
    test_risk[iteration+1] <- icb_object$Riskfunction(y = y, f = prediction)
    
    iteration <- iteration + 1
  }
  
  
  
  # For the tree part:
  while(iteration < length(icb_object$Risk)){
    
    mboost_tree_model <- icb_object$Prediction_Models[[iteration]]
    pred_iteration <- mboost_tree_model$predict(newdata = as.data.frame(X_new))
    
    prediction <- prediction + pred_iteration
    
    
    # Calculate the risk in this iteration
    test_risk[iteration+1] <- icb_object$Riskfunction(y = y, f = prediction)
    
    iteration <- iteration + 1
  }
  
  
  return_list <- list()
  return_list[["Predictions"]] <- prediction
  return_list[["TestRisk"]] <- test_risk
  
  return(return_list)
  
}

# ####
# 
# ## Test
# 
# icb_predict(icb_object = micb_500, newdata = test)
# 
# 
# # Compare to
# as.matrix(cbind(1, X_new[,-1])) %*% icb_object$Coefficients$Linear_coefficients 
