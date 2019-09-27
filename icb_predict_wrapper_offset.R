icb_predict_wrapper <- function(icb_object, newdata, target = NULL){
  # icb_object:     output object from the interpretable comp boosting function
  # newdata:        newdata that will be used to make predictions
  # target:         string that can specify the target variable; should be filled if newdata 
                    # contains the target variable -> inner risk will be computed
  
  #formula <- terms.formula(icb_object$Input_Parameters[4])
  #X <- model.matrix(formula, newdata)
  #X_new <- cbind(1, newdata)
  X_new <- na.omit(newdata)
  
  target_class <- icb_object$Input_Parameters[[5]]
  
  if(!is.null(target)){
    y <- X_new[,target]
    if(target_class=="Binomial"){
      #y <- (c(-1, 1)[as.integer(y)])
      y_int <- numeric(length(y))
      for(l in 1:length(y)){
        if(y[l] == icb_object$Input_Parameters[[6]]){
          y_int[l] <- -1
        }else if(y[l] == icb_object$Input_Parameters[[7]]){
          y_int[l] <- 1
        }
      }
      #y_int
    }
  }
  
  target_class <- icb_object$Input_Parameters[[5]]
  
  # Create an empty numeric vector for saving the test risk results
  test_risk <- numeric(length(icb_object$Risk))
  test_risk_label <- numeric(length(icb_object$Risk))
  
  # Create an empty vector with the length of newdata
  prediction <- vector(mode = "numeric", length = dim(X_new)[1])
  
  # Fill every entry with the intercept of the model
  for (l in 1:length(prediction)){
    prediction[l] <- icb_object$Prediction_Models$Linear$offset[[1]]
  }
  
  # Offset prediction
  prediction_offset <- prediction
  
  # Calculate the risk if possible
  if(!is.null(target)){
    test_risk[1] <- icb_object$Riskfunction(y = y_int, f = prediction) / dim(X_new)[1]
    test_risk_label[1] <- icb_object$Riskfunction(y = y_int, f=pred_label_risk(prediction)) / dim(X_new)[1]
  }
  
  
  iteration <- 1
  
  
  # For the linear part:
  
  ## Version 1: per iteration
  while(iteration <= (icb_object$`Transition Iterations`[1])){
    
    pred_iteration <- icb_object$Prediction_Models$Linear[iteration]$predict(newdata = X_new)
    
    #prediction <- prediction_offset + pred_iteration
    
    # Calculate the risk in this iteration
    if(!is.null(target)){
      test_risk[iteration+1] <- icb_object$Riskfunction(y = y_int, f = prediction_offset+pred_iteration) / dim(X_new)[1]
      test_risk_label[iteration+1] <- icb_object$Riskfunction(y = y_int, f=pred_label_risk(prediction_offset+pred_iteration)) / dim(X_new)[1]
    }
    
    iteration <- iteration + 1
  
  }
  
  # Save the predictions after the linear part
  prediction_linear <- prediction_offset + pred_iteration
  
  ## Version 2: use result of linear coefficients (uses one specific nu)
  #prediction <- as.matrix(cbind(1, X_new[,-1])) %*% icb_object$Coefficients$Linear_coefficients 
  
  
  
  # For the splines part:
  while(iteration <= (icb_object$`Transition Iterations`[2])){
    
    pred_iteration <- icb_object$Prediction_Models$Spline[iteration - icb_object$`Transition Iterations`[1]]$predict(newdata = X_new)
    
    #prediction <- prediction_linear + pred_iteration
    
    if(!is.null(target)){
      # Calculate the risk in this iteration
      test_risk[iteration+1] <- icb_object$Riskfunction(y = y_int, f = prediction_linear+pred_iteration) / dim(X_new)[1]
      test_risk_label[iteration+1] <- icb_object$Riskfunction(y = y_int, f=pred_label_risk(prediction_linear+pred_iteration)) / dim(X_new)[1]
    }
    
    iteration <- iteration + 1
  }
  
  prediction_spline <- prediction_linear + pred_iteration
  
  
  
  # For the tree part:
  while(iteration < length(icb_object$Risk)){
    
    pred_iteration <- icb_object$Prediction_Models$Tree[iteration - icb_object$`Transition Iterations`[2]]$predict(newdata = X_new)
    
    #prediction <- prediction_spline + pred_iteration
    
    if(!is.null(target)){
      # Calculate the risk in this iteration
      test_risk[iteration+1] <- icb_object$Riskfunction(y = y_int, f = prediction_spline+pred_iteration) / dim(X_new)[1]
      test_risk_label[iteration+1] <- icb_object$Riskfunction(y = y_int, f=pred_label_risk(prediction_spline+pred_iteration)) / dim(X_new)[1]
    }
    
    iteration <- iteration + 1
  }
  
  prediction_tree <- prediction_spline + pred_iteration
  
  if(target_class=="Binomial"){
    predicted_labels <- numeric(length(prediction_tree))
    for(lp in 1:length(predicted_labels)){
      if(prediction_tree[lp] < 0){
        predicted_labels[lp] <- 0
      } else{
        predicted_labels[lp] <- 1
      }
    }
  }
  
  
  
  
  return_list <- list()
  return_list[["Predictions"]] <- prediction_tree
  if(target_class=="Binomial"){
    return_list[["Predicted Labels"]] <- predicted_labels
    return_list[["TestLabelRisk"]] <- test_risk_label
  }
  return_list[["TestRisk"]] <- test_risk
  
  return(return_list)
  
}

