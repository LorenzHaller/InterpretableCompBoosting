# Combine linear models and splines (using mboost code)

interpretable_comp_boost_wrapper <- function(data, formula, nu=0.1, target_class="Gaussian",
                                     epsilon = 0.0025, bl2=c("bbs","btree")){
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
  data <- na.omit(data)
  formula_orig <- formula
  formula <- terms.formula(formula)
  X <- model.matrix(formula, data)
  target <- all.vars(formula)[1]
  levels <- c()
  
  # Create a target variable with {-1,1} encoding
  if(target_class=="Binomial"){
    data[,target] <- as.factor(data[,target])
    y <- data[, target]
    #y_int <- (c(-1, 1)[as.integer(y)])
    y_int <- numeric(length(y))
    for(l in 1:length(y)){
      if(y[l] == levels(y)[1]){
        y_int[l] <- -1
      }else if(y[l] == levels(y)[2]){
        y_int[l] <- 1
      }
    }
    levels <- levels(y)
  } else{
    y <- data[, target]
    y_int <- y
  }
  
  
  
  # # Load the gradient and risk function (using the mboost family.R code)
  if(target_class == "Gaussian"){
    family = Gaussian()
  } else if(target_class == "Binomial"){
    family = Binomial()
  } else{
    stop("No correct family for target!")
  }
  
  ngradient <- family@ngradient
  riskfct <- family@risk
    
    
  # Initialize with Intercept model (similar to family@offset(y))
  if(target_class == "Binomial"){
    intercept_model <- glm.fit(x=as.matrix(X[,1]), y=y, family = binomial(link = "logit"))
    fit_0 <- intercept_model$fitted.values
    fitted_values <- fit_0
  } else{
    intercept_model <- lm.fit(x=as.matrix(X[,1]), y=y)
    fit_0 <- intercept_model$fitted.values
    fitted_values <- fit_0
  }
  
  #Calculate the risk of the intercept model
  if(target_class == "Binomial"){
    risk_iter <- numeric()
    risk_iter[1] <- riskfct(y=y_int,pred_label_risk(fit_0))
  }
  
  risk_0 <- riskfct(y = y_int, f = fitted_values)
  
  # Initialize a vector to save the risk values for the labels
  # risk_iter <- numeric()
  # risk_iter[1] <- risk_0_labels
  
  ### Phase 1: Linear models as base learners
  
  # Initialize the current iteration number
  iteration <- 1
  
  # Start with an initial mboost iteration
  mb_linear <- mboost::mboost(formula = formula, data = data, family = family, offset = fit_0,
                             baselearner = "bols", control = boost_control(nu = nu, mstop = 1))
  
  # Save the risk for the predicted labels
  if(target_class == "Binomial"){
    risk_iter[iteration+1] <- riskfct(y=y_int,pred_label_risk(mb_linear$fitted()))
  }
  
  while((mb_linear$risk()[iteration] / mb_linear$risk()[iteration+1]) >= (1 + epsilon)){
      
      #Add one to the iteration number
      iteration <- iteration + 1
      
      # Create next iteration 
      mb_linear <- mb_linear[iteration]

      if(target_class == "Binomial"){
        risk_iter[iteration+1] <- riskfct(y=y_int,pred_label_risk(mb_linear$fitted()))
      }
      
    }
    
  transition_splines <- iteration
  
  # Save fitted values of the last linear model iteration to use them as offset
  fitted_values <- mb_linear$fitted()
  
  
  
  ### Phase 2: Splines / Tree Stumps
  
  if(target_class=="Binomial"){
  
    iteration <- iteration + 1 

    mb_spline = mboost::gamboost(formula = formula, data = data, family = family,
                                 baselearner = "btree", offset = mb_linear$fitted(),
                                 control = boost_control(nu = nu, mstop = 1))
    
    
    risk_iter[iteration+1] <- riskfct(y=y_int,pred_label_risk(mb_spline$fitted()))
    
    
    while((mb_spline$risk()[iteration-transition_splines] / mb_spline$risk()[iteration-transition_splines+1]) >= (1 + epsilon)){
      
      #Add one to the iteration number
      iteration <- iteration + 1
      
      # Create next iteration
      mb_spline <- mb_spline[iteration - transition_splines]
      
      # Save risk
      risk_iter[iteration+1] <- riskfct(y=y_int,pred_label_risk(mb_spline$fitted()))
      
    }
    
    transition_trees <- iteration
    
    # Save fitted values of the last linear model iteration to use them as offset
    fitted_values <- mb_spline$fitted()
    
    
  } else if(target_class=="Gaussian"){
    
    iteration <- iteration + 1 
    
    mb_spline = mboost::gamboost(formula = formula, data = data, family = family,
                                 baselearner = bl2, offset = mb_linear$fitted(),
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
    
  } 
  
  
  
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
  
  if(target_class == "Binomial"){
    risk_iter[iteration+1] <- riskfct(y=y_int,pred_label_risk(mb_tree$fitted()))
  }
  
  
  while((mb_tree$risk()[iteration-transition_trees] / mb_tree$risk()[iteration-transition_trees+1]) >= (1 + epsilon)){
    
    #Add one to the iteration number
    iteration <- iteration + 1
    
    mb_tree <- mb_tree[iteration - transition_trees]
    
    if(target_class == "Binomial"){
      risk_iter[iteration+1] <- riskfct(y=y_int,pred_label_risk(mb_tree$fitted()))
    }
  }
  
  
  fitted_values <- mb_tree$fitted()
  
  
  ### Create list for models of all three phases
  Prediction_Models <- list()
  Prediction_Models[["Linear"]] <- mb_linear
  Prediction_Models[["Spline"]] <- mb_spline
  Prediction_Models[["Tree"]] <- mb_tree
  
  ### Create a list to return 
  return_list <- list()
  return_list[["Coefficients"]] <- c(mb_linear$coef(), mb_spline$coef())
  return_list[["Fitted_Values"]] <- fitted_values
  return_list[["Transition Iterations"]] <-c(transition_splines,transition_trees)
  return_list[["Risk"]] <- c(mb_linear$risk(),mb_spline$risk(),mb_tree$risk())
  return_list[["Prediction_Models"]] <- Prediction_Models
  return_list[["Input_Parameters"]] <-c(nu, iteration, epsilon, formula_orig, target_class, levels)
  return_list[["Data"]] <- X
  return_list[["Riskfunction"]] <- riskfct
  if(target_class == "Binomial"){
    return_list[["LabelRisk"]] <- risk_iter
  }
  
  # Print the coefficients of the final model
  return(return_list)


}