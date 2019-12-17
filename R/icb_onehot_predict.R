icb_onehot_predict <- function(icb_object, newdata, target = NULL){
  # icb_object:     output object from the icb function
  # newdata:        test data
  # target:         string that can specify the target variable; should be filled if newdata contains the target variable to compute risk
  
  # Performing checks on the input parameters
  stopifnot(is.data.frame(newdata))
  
  # Check for NAs and exclude the rows with NAs
  if(anyNA(newdata)){
    X_new <- na.omit(newdata)
    warning("Rows with NAs were excluded.")
  } else{
    X_new <- newdata
  }
  
  
  
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  
  target_class <- icb_object$Input_Parameters[[5]]
  
  if(!is.null(target)){
    y <- X_new[,target]
    if(target_class=="Binomial"){
      #y <- (c(-1, 1)[as.integer(y)])
      y_int <- numeric(length(y))
      for(l in 1:length(y)){
        if(y[l] == icb_object$Input_Parameters[[8]]){
          y_int[l] <- -1
        }else if(y[l] == icb_object$Input_Parameters[[9]]){
          y_int[l] <- 1
        }
      }
      #y_int
    } else{
      y_int <- y 
    }
  }
  
  #X_new <- droplevels(X_new)
  
  # Make one-hot encoding for factor variables
  dummies <- dummyVars(" ~ .", data = X_new, fullRank = T)
  X_new <- data.frame(predict(dummies, newdata = X_new))
  
  # Only allow the feature which have been in the training data
  X_new <- X_new[,which(colnames(X_new) %in% icb_object$FeatureNames)]
  
  target_class <- icb_object$Input_Parameters[[5]]
  
  # Create an empty numeric vector for saving the test risk results
  test_risk <- numeric(length(icb_object$Risk))
  test_risk_label <- numeric(length(icb_object$Risk))
  individual_risk <- matrix(0, ncol = 5, nrow = dim(X_new)[1])
  colnames(individual_risk) <- c("Intercept","Linear","Non-linear",
                                 "Trees of depth 2", "Deeper Trees")
  # Create matrix to save predictions after every stage
  stage_predictions <- matrix(0, ncol = 5, nrow = dim(X_new)[1])
  colnames(stage_predictions) <- c("Intercept","Linear","Non-linear",
                                 "Trees of depth 2", "Deeper Trees")
  
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
    if(target_class=="Binomial"){
      test_risk_label[1] <- icb_object$Riskfunction(y = y_int, f=pred_label_risk(prediction)) / dim(X_new)[1]
    }
    for (j in 1:dim(X_new)[1]){
      individual_risk[j,1] = round(icb_object$Riskfunction(y = y_int[j], f = prediction[j]), digits = 2)
      stage_predictions[j,1] = prediction[j]
    }
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
      if(target_class=="Binomial"){
        test_risk_label[iteration+1] <- icb_object$Riskfunction(y = y_int, f=pred_label_risk(prediction_offset+pred_iteration)) / dim(X_new)[1]
      }
    }
    
    iteration <- iteration + 1
  
  }
  
  # Save the predictions after the linear part
  prediction_linear <- prediction_offset + pred_iteration
  
  # Save the individual risk after stage 1
  if(!is.null(target)){
    for (j in 1:dim(X_new)[1]){
      individual_risk[j,2] = round(icb_object$Riskfunction(y = y_int[j], f = prediction_linear[j]), digits = 2)
      stage_predictions[j,2] = prediction_linear[j]
    }
  }
  
  
  
  # For the splines part:
  while(iteration <= (icb_object$`Transition Iterations`[2])){
    
    pred_iteration <- icb_object$Prediction_Models$Spline[iteration - icb_object$`Transition Iterations`[1]]$predict(newdata = X_new)
    
    #prediction <- prediction_linear + pred_iteration
    
    if(!is.null(target)){
      # Calculate the risk in this iteration
      test_risk[iteration+1] <- icb_object$Riskfunction(y = y_int, f = prediction_linear+pred_iteration) / dim(X_new)[1]
      if(target_class=="Binomial"){
        test_risk_label[iteration+1] <- icb_object$Riskfunction(y = y_int, f=pred_label_risk(prediction_linear+pred_iteration)) / dim(X_new)[1]
      }
    }
    
    iteration <- iteration + 1
  }
  
  prediction_spline <- prediction_linear + pred_iteration
  
  # Save the individual risk after stage 2
  if(!is.null(target)){
    for (j in 1:dim(X_new)[1]){
      individual_risk[j,3] = round(icb_object$Riskfunction(y = y_int[j], f = prediction_spline[j]), digits = 2)
      stage_predictions[j,3] = prediction_spline[j]
    }
  }
  
  
  # For the tree (depth=2) part:
  while(iteration <= (icb_object$`Transition Iterations`[3])){
    
    pred_iteration <- icb_object$Prediction_Models$Tree[iteration - icb_object$`Transition Iterations`[2]]$predict(newdata = X_new)
    
    #prediction <- prediction_spline + pred_iteration
    
    if(!is.null(target)){
      # Calculate the risk in this iteration
      test_risk[iteration+1] <- icb_object$Riskfunction(y = y_int, f = prediction_spline+pred_iteration) / dim(X_new)[1]
      if(target_class=="Binomial"){
        test_risk_label[iteration+1] <- icb_object$Riskfunction(y = y_int, f=pred_label_risk(prediction_spline+pred_iteration)) / dim(X_new)[1]
      }
    }
    
    iteration <- iteration + 1
  }
  
  prediction_tree <- prediction_spline + pred_iteration
  
  # Save the individual risk after stage 3
  if(!is.null(target)){
    for (j in 1:dim(X_new)[1]){
      individual_risk[j,4] = round(icb_object$Riskfunction(y = y_int[j], f = prediction_tree[j]), digits = 2)
      stage_predictions[j,4] = prediction_tree[j]
    }
  }
  
  # For the tree (depth via user input) part:
  while(iteration < length(icb_object$Risk)){
    
    pred_iteration <- icb_object$Prediction_Models$TreeMax[iteration - icb_object$`Transition Iterations`[3]]$predict(newdata = X_new)
    
    #prediction <- prediction_spline + pred_iteration
    
    if(!is.null(target)){
      # Calculate the risk in this iteration
      test_risk[iteration+1] <- icb_object$Riskfunction(y = y_int, f = prediction_tree+pred_iteration) / dim(X_new)[1]
      if(target_class=="Binomial"){
        test_risk_label[iteration+1] <- icb_object$Riskfunction(y = y_int, f=pred_label_risk(prediction_tree+pred_iteration)) / dim(X_new)[1]
      }
    }
    
    iteration <- iteration + 1
  }
  
  
  prediction_tree_max <- prediction_tree + pred_iteration
  
  # Save the individual risk after the last stage
  if(!is.null(target)){
    for (j in 1:dim(X_new)[1]){
      individual_risk[j,5] = round(icb_object$Riskfunction(y = y_int[j], f = prediction_tree_max[j]), digits = 2)
      stage_predictions[j,5] = prediction_tree_max[j]
    }
  }
  
  
  
  
  
  
  
  
  if(target_class=="Binomial"){
    predicted_labels <- numeric(length(prediction_tree_max))
    for(lp in 1:length(predicted_labels)){
      if(prediction_tree_max[lp] < 0){
        predicted_labels[lp] <- 0
      } else{
        predicted_labels[lp] <- 1
      }
    }
  }
  
  
  
  
  return_list <- list()
  return_list[["Predictions"]] <- prediction_tree_max
  if(target_class=="Binomial"){
    return_list[["Predicted Labels"]] <- predicted_labels
    return_list[["TestLabelRisk"]] <- test_risk_label
  }
  return_list[["TestRisk"]] <- test_risk
  return_list[["Transition Iterations"]] <- c(icb_object$`Transition Iterations`,length(icb_object$Risk))
  return_list[["IndividualRisk"]] <- individual_risk
  return_list[["StagePredictions"]] <- stage_predictions
  
  options(warn = oldw)
  
  return(return_list)
  
}

