icb <- function(data, formula, nu=0.1, target_class="Gaussian",
                                     epsilon = 0.001, bl2=c("bbs","btree"), df_spline = 4,
                                     max_depth = 8,
                                     min_split = 20, min_bucket = round(min_split/3)){
  # data:           a data frame containing target and features
  # formula:        a formula specifying the model
  # nu:             the step size or shrinkage parameter (default = 0.1)
  # target_class:   the family of the target variable (default = Gaussian)
  # epsilon:        necessary relative epsilon improvement for one iteration (default = 0.001)
  # bl2:            the non-linear model used in the second stage: P-splines ("bbs") or tree stumps ("btree)
  # df_spline:      the degree of the splines that should be fitted when P-splines are used
  # max_depth:      the maximum depth of the trees in the fourth stage
  # min_split:      minimum number of observations in a tree node for the node to be considered in splitting
  # min_bucket:     minimum number of observations allowed in a terminal tree node
  
  # Performing checks on the input parameters:
  stopifnot(is.data.frame(data))
  stopifnot(target_class %in% c("Gaussian","Binomial"))
  stopifnot(is.numeric(nu))
  stopifnot(is.numeric(epsilon))
  stopifnot(bl2 %in% c("bbs","btree"))
  stopifnot(is.numeric(max_depth))
  
  # Preparing the formula and data by seperating the target(y) and the features(X)
  data <- na.omit(data)
  
  # Get the target
  formula_orig <- formula
  formula <- terms.formula(formula)
  target <- all.vars(formula)[1]
  
  # Create a list of all features names as specified in the formula
  all_vars <- all.vars(formula)
  if(all_vars[2] != "."){
    data <- data[,colnames(data) %in% all_vars]
  }
  
  
  target_data <- data.frame(data[,target])
  colnames(target_data) <- target
  
  data <- droplevels(data)
  
  # # Make one-hot encoding for factor variables
  # if(target_class=="Binomial"){
  #   dummies <- dummyVars(" ~ .", data = data[,colnames(data) != target], fullRank = T)
  #   data <- data.frame(predict(dummies, newdata = data[,colnames(data) != target]))
  #   data <- data.frame(data,target_data)
  # } else if(target_class == "Gaussian"){
  #   dummies <- dummyVars(" ~ .", data = data, fullRank = T)
  #   data <- data.frame(predict(dummies, newdata = data))
  # }
  # 
  # Create a vector that contains the number of unique values for every feature
  #len_vector <- as.vector(sapply(sapply(data[,colnames(data) != target], unique), length))
  #len_boolean <- len_vector < 3
  #len_boolean <- len_boolean[!colnames(data) %in% target]
  # 
  # # Create formula
  # formula <- as.formula(paste(target,"~ ."))
  
  # Save feature names of one-hot-encoded data
  f_names <- colnames(data)[which(colnames(data) != target)]
  
  # Create the feature matrix
  X <- model.matrix(formula, data)
  
  levels <- "0"
  
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
  
  
  # Create a counter for how many features are in the model
  feature_counter <- numeric()
  feature_counter[1] <- 0
  feature_list <- c()
  
  # Initialize a vector to save the risk values for the labels
  # risk_iter <- numeric()
  # risk_iter[1] <- risk_0_labels
  
  ### Phase 1: Linear models as base learners
  
  # Initialize the current iteration number
  iteration <- 1
  
  
  # Start with an initial mboost iteration
  mb_linear <- mboost::mboost(formula = formula, data = data, family = family, offset = fit_0,
                             baselearner = "bols", control = boost_control(nu = nu, mstop = 1))
  
  # Check if feature added is new
  if(!mb_linear$xselect()[iteration] %in% feature_list){
    feature_list <- c(feature_list,as.character(mb_linear$xselect()[iteration]))
    feature_counter[iteration+1] <- feature_counter[iteration] + 1
  } else{
    feature_counter[iteration+1] <- feature_counter[iteration]
  }
  
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
      
      # Check if feature added is new
      if(!mb_linear$xselect()[iteration] %in% feature_list){
        feature_list <- c(feature_list,as.character(mb_linear$xselect()[iteration]))
        feature_counter[iteration+1] <- feature_counter[iteration] + 1
      } else{
        feature_counter[iteration+1] <- feature_counter[iteration]
      }
    }
    
  transition_splines <- iteration
  
  # Save fitted values of the last linear model iteration to use them as offset
  fitted_values <- mb_linear$fitted()
  
  
  ### Phase 2: Splines / Tree Stumps
  
  if(target_class=="Binomial"){
  
    iteration <- iteration + 1 

    
    mb_spline = mboost::gamboost(formula = formula, data = data, family = family,
                                 baselearner = bl2,
                                 offset = mb_linear$fitted(),
                                 control = boost_control(nu = nu, mstop = 1))
        
    # Check if feature added is new
    if(!mb_spline$xselect()[iteration-transition_splines] %in% feature_list){
      feature_list <- c(feature_list,as.character(mb_spline$xselect()[iteration-transition_splines]))
      feature_counter[iteration+1] <- feature_counter[iteration] + 1
    } else{
      feature_counter[iteration+1] <- feature_counter[iteration]
    }
    
    risk_iter[iteration+1] <- riskfct(y=y_int,pred_label_risk(mb_spline$fitted()))
    
    
    while((mb_spline$risk()[iteration-transition_splines] / mb_spline$risk()[iteration-transition_splines+1]) >= (1 + epsilon)){
      
      #Add one to the iteration number
      iteration <- iteration + 1
      
      # Create next iteration
      mb_spline <- mb_spline[iteration - transition_splines]
      
      # Save risk
      risk_iter[iteration+1] <- riskfct(y=y_int,pred_label_risk(mb_spline$fitted()))
      
      # Check if feature added is new
      if(!mb_spline$xselect()[iteration-transition_splines] %in% feature_list){
        feature_list <- c(feature_list,as.character(mb_spline$xselect()[iteration-transition_splines]))
        feature_counter[iteration+1] <- feature_counter[iteration] + 1
      } else{
        feature_counter[iteration+1] <- feature_counter[iteration]
      }
    }
    
    transition_trees <- iteration
    
    # Save fitted values of the last linear model iteration to use them as offset
    fitted_values <- mb_spline$fitted()
    
    
  } else if(target_class=="Gaussian"){
    
    iteration <- iteration + 1 
    
    mb_spline = mboost::gamboost(formula = formula, data = data, family = family,
                                   baselearner = bl2,
                                   offset = mb_linear$fitted(),
                                   control = boost_control(nu = nu, mstop = 1))
    
    # Check if feature added is new
    if(!mb_spline$xselect()[iteration-transition_splines] %in% feature_list){
      feature_list <- c(feature_list,as.character(mb_spline$xselect()[iteration-transition_splines]))
      feature_counter[iteration+1] <- feature_counter[iteration] + 1
    } else{
      feature_counter[iteration+1] <- feature_counter[iteration]
    }
    
    while((mb_spline$risk()[iteration-transition_splines] / mb_spline$risk()[iteration-transition_splines+1]) >= (1 + epsilon)){
      
      #Add one to the iteration number
      iteration <- iteration + 1
      
      # Create next iteration
      mb_spline <- mb_spline[iteration - transition_splines]
      
      # Check if feature added is new
      if(!mb_spline$xselect()[iteration-transition_splines] %in% feature_list){
        feature_list <- c(feature_list,as.character(mb_spline$xselect()[iteration-transition_splines]))
        feature_counter[iteration+1] <- feature_counter[iteration] + 1
      } else{
        feature_counter[iteration+1] <- feature_counter[iteration]
      }
    }
    
    transition_trees <- iteration
    
    # Save fitted values of the last linear model iteration to use them as offset
    fitted_values <- mb_spline$fitted()
    
  } 
  
  
  
  ### Phase 3: Trees with depth=2
  
  iteration <- iteration + 1 
  
  mb_tree = blackboost(formula = formula, data = data, offset = fitted_values,
                          control = boost_control(nu = nu, mstop = 1), family = family,
                           tree_controls = partykit::ctree_control(
                             teststat = "quad",
                             testtype = "Teststatistic",
                             mincriterion = 0,
                             minsplit = min_split, 
                             minbucket = min_bucket,
                             maxdepth = 2, 
                             saveinfo = TRUE))
  
  # Check if feature added is new
  if(!mb_tree$xselect()[iteration-transition_trees] %in% feature_list){
    feature_list <- c(feature_list,as.character(mb_tree$xselect()[iteration-transition_trees]))
    feature_counter[iteration+1] <- feature_counter[iteration] + 1
  } else{
    feature_counter[iteration+1] <- feature_counter[iteration]
  }
  
  if(target_class == "Binomial"){
    risk_iter[iteration+1] <- riskfct(y=y_int,pred_label_risk(mb_tree$fitted()))
  }
  
  
  while((mb_tree$risk()[iteration-transition_trees] / mb_tree$risk()[iteration-transition_trees+1]) >= (1 + epsilon)){
    
    #Add one to the iteration number
    iteration <- iteration + 1
    
    mb_tree <- mb_tree[iteration - transition_trees]
    
    # Check if feature added is new
    if(!mb_tree$xselect()[iteration-transition_trees] %in% feature_list){
      feature_list <- c(feature_list,as.character(mb_tree$xselect()[iteration-transition_trees]))
      feature_counter[iteration+1] <- feature_counter[iteration] + 1
    } else{
      feature_counter[iteration+1] <- feature_counter[iteration]
    }
    
    if(target_class == "Binomial"){
      risk_iter[iteration+1] <- riskfct(y=y_int,pred_label_risk(mb_tree$fitted()))
    }
  }
  
  
  fitted_values <- mb_tree$fitted()
  
  transition_trees_max <- iteration
  
  
  
  
  ### Phase 4: Trees with user-specified depth
  
  iteration <- iteration + 1 
  
  mb_tree_max = blackboost(formula = formula, data = data, offset = fitted_values,
                       control = boost_control(nu = nu, mstop = 1), family = family,
                       tree_controls = partykit::ctree_control(
                         teststat = "quad",
                         testtype = "Teststatistic",
                         mincriterion = 0,
                         minsplit = min_split, 
                         minbucket = min_bucket,
                         maxdepth = max_depth, 
                         saveinfo = TRUE))
  
  # Check if a feature added is new
  if(!mb_tree_max$xselect()[iteration-transition_trees_max] %in% feature_list){
    feature_list <- c(feature_list,as.character(mb_tree_max$xselect()[iteration-transition_trees_max]))
    feature_counter[iteration+1] <- feature_counter[iteration] + 1
  } else{
    feature_counter[iteration+1] <- feature_counter[iteration]
  }
  
  if(target_class == "Binomial"){
    risk_iter[iteration+1] <- riskfct(y=y_int,pred_label_risk(mb_tree_max$fitted()))
  }
  
  
  while((mb_tree_max$risk()[iteration-transition_trees_max] / mb_tree_max$risk()[iteration-transition_trees_max+1]) >= (1 + epsilon)){
    
    #Add one to the iteration number
    iteration <- iteration + 1
    
    mb_tree_max <- mb_tree_max[iteration - transition_trees_max]
    
    # Check if feature added is new
    if(!mb_tree_max$xselect()[iteration-transition_trees_max] %in% feature_list){
      feature_list <- c(feature_list,as.character(mb_tree_max$xselect()[iteration-transition_trees_max]))
      feature_counter[iteration+1] <- feature_counter[iteration] + 1
    } else{
      feature_counter[iteration+1] <- feature_counter[iteration]
    }
    
    if(target_class == "Binomial"){
      risk_iter[iteration+1] <- riskfct(y=y_int,pred_label_risk(mb_tree_max$fitted()))
    }
  }
  
  
  fitted_values <- mb_tree_max$fitted()
  
  
  
  
  
  
  ### Create list for models of all three phases
  Prediction_Models <- list()
  Prediction_Models[["Linear"]] <- mb_linear
  Prediction_Models[["Spline"]] <- mb_spline
  Prediction_Models[["Tree"]] <- mb_tree
  Prediction_Models[["TreeMax"]] <- mb_tree_max
  
  ### Create a list to return 
  return_list <- list()
  return_list[["Coefficients"]] <- c(mb_linear$coef(), mb_spline$coef())
  return_list[["Fitted_Values"]] <- fitted_values
  return_list[["Transition Iterations"]] <-c(transition_splines,transition_trees,transition_trees_max)
  return_list[["Risk"]] <- c(mb_linear$risk(),mb_spline$risk()[-1],mb_tree$risk()[-1],mb_tree_max$risk()[-1]) / dim(data)[1]
  return_list[["Prediction_Models"]] <- Prediction_Models
  return_list[["Input_Parameters"]] <-c(nu, iteration, epsilon, formula_orig, target_class, bl2, df_spline, levels)
  return_list[["Data"]] <- data[,!colnames(data) %in% target]
  return_list[["FeatureNames"]] <- f_names
  return_list[["FeatureLevels"]] <- lapply(data, levels.default)
  #return_list[["CATFeatures"]] <- len_boolean
  return_list[["Feature_Counter"]] <- feature_counter
  return_list[["Riskfunction"]] <- riskfct
  if(target_class == "Binomial"){
    return_list[["LabelRisk"]] <- risk_iter
  }
  
  # Print the coefficients of the final model
  return(return_list)


}