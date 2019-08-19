icb_predict <- function(icb_object, newdata){
  # icb_object:     output object from the interpretable comp boosting function
  # newdata:        newdata that will be used to make predictions
  
  icb_object <- micb_500
  newdata <- test
  
  # X <- model.matrix(formula, newdata)
  #X_new <- cbind(1, newdata)
  
  # Scaling for newdata according to scaling for training data
  X_new <- scale(newdata)
  
  # Create an empty vector with the length of newdata
  prediction <- vector(mode = "numeric", length = dim(newdata)[1])
  
  # Fill every entry with the intercept of the model
  for (l in 1:length(prediction)){
    prediction[l] <- icb_object$Coefficients$Intercept[[1]]
  }
  
  iteration <- 1
  nu <- icb_object$Input_Parameters[1]
  
  # For the linear part:
  while(iteration <= icb_object$`Transition Iterations`[1]){
    
    # Get the name and position of the selected feature
    selected_feature <- names(which(icb_object$Prediction_Models[[iteration]] != 0))
    pos_feature <- which(icb_object$Prediction_Models[[iteration]] != 0)[[1]]
    
    # Multiply the values of the selected feature with its coefficient
    pred_iteration <- X_new[,selected_feature] * icb_object$Prediction_Models[[iteration]][pos_feature]
    
    prediction <- prediction + nu * pred_iteration
    
    iteration <- iteration + 1
  
  }
  
  
  
  # For the splines part:
  while(iteration <= icb_object$`Transition Iterations`[2]){
    
    mboost_spline_model <- icb_object$Prediction_Models[[iteration]]
    pred_iteration <- mboost_spline_model$predict(newdata = as.data.frame(X_new))
    
    prediction <- prediction + nu * pred_iteration
    
    iteration <- iteration + 1
  }
  
  
}


# Compare to
as.matrix(cbind(1, X_new[,-1])) %*% icb_object$Coefficients$Linear_coefficients 
