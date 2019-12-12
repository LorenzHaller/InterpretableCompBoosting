# Build an mlr wrapper around the function in order to make benchmarking easier

# Regression learner

makeRLearner.regr.icb = function() {
  makeRLearnerRegr(
    cl = "regr.icb",
    package = c("mboost","partykit","caret"),
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "nu", lower = 0, upper = 1, default = 0.1),
      makeNumericLearnerParam(id = "epsilon", lower = 0, upper = 1, default = 0.005),
      makeDiscreteLearnerParam(id = "bl2", default = "btree", values = c("bbs","btree")),
      makeIntegerLearnerParam(id = "max_depth", default = 8),
      makeIntegerLearnerParam(id = "min_split", default = 10),
      makeIntegerLearnerParam(id = "min_bucket", default = 4),
      makeIntegerLearnerParam(id = "df_spline", default = 2)
    ),
    properties = c("numerics", "factors"),
    name = "Gradually interpretable component-wise boosting",
    short.name = "icb",
    note = ""
  )
}


trainLearner.regr.icb = function (.learner, .task, .subset, .weights = NULL, ...) 
{
  formula = getTaskFormula(.task)
  data = getTaskData(.task, .subset)
  
  nu <- .learner$par.vals$nu
  epsilon <- .learner$par.vals$epsilon
  bl2 <- .learner$par.vals$bl2
  max_depth <- .learner$par.vals$max_depth
  min_split <- .learner$par.vals$min_split
  min_bucket <- .learner$par.vals$min_bucket
  df_spline <- .learner$par.vals$df_spline
  
  # Preparing the formula and data by seperating the target(y) and the features(X)
  data <- na.omit(data)
  
  # Get the target
  target <- all.vars(formula)[1]
  
  # Create a list of all features names as specified in the formula
  all_vars <- all.vars(formula)
  if(all_vars[2] != "."){
    data <- data[,colnames(data) %in% all_vars]
  }
  
  data <- droplevels(data)
  
  # # Make one-hot encoding for factor variables
  # dummies <- dummyVars(" ~ .", data = data, fullRank = T)
  # data <- data.frame(predict(dummies, newdata = data))
  
  # Create mlr task to get full formula
  formula <- as.formula(paste(target,"~ ."))
  
  # Save feature names of one-hot-encoded data
  f_names <- colnames(data)[which(colnames(data) != target)]
  
  # Create the feature matrix
  X <- model.matrix(formula, data)
  
  # # Create a vector that contains the number of unique values for every feature
  # len_vector <- as.vector(sapply(sapply(data[,colnames(data) != target], unique), length))
  # len_boolean <- len_vector < 3
  
  y <- data[, target]
  y_int <- y
  
  # Load the gradient and risk function (using the mboost family.R code)
  family = Gaussian()
  ngradient <- family@ngradient
  riskfct <- family@risk
  
  # Initialize with Intercept model (similar to family@offset(y))
  intercept_model <- lm.fit(x=as.matrix(X[,1]), y=y)
  fit_0 <- intercept_model$fitted.values
  fitted_values <- fit_0
  
  #Calculate the risk of the intercept model
  risk_0 <- riskfct(y = y_int, f = fitted_values)
  
  # Create a counter for how many features are in the model
  feature_counter <- numeric()
  feature_counter[1] <- 0
  feature_list <- c()
  
  
  ################ Phase 1: Linear models as base learners ########################################
  
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
  
  while((mb_linear$risk()[iteration] / mb_linear$risk()[iteration+1]) >= (1 + epsilon)){
    
    #Add one to the iteration number
    iteration <- iteration + 1
    
    # Create next iteration 
    mb_linear <- mb_linear[iteration]
    
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
  
  
  #################### Phase 2: Splines / Tree Stumps ###########################################
  
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
  
  
  ############################## Phase 3: Trees with depth=2 ##########################################
  
  iteration <- iteration + 1 
  
  mb_tree = blackboost(formula = formula, data = data, offset = fitted_values,
                       control = boost_control(nu = nu, mstop = 1),
                       tree_controls = partykit::ctree_control(
                         teststat = "quad",
                         testtype = "Teststatistic",
                         mincriterion = 0,
                         minsplit = 10, 
                         minbucket = 4,
                         maxdepth = 2, 
                         saveinfo = FALSE))
  
  # Check if feature added is new
  if(!mb_tree$xselect()[iteration-transition_trees] %in% feature_list){
    feature_list <- c(feature_list,as.character(mb_tree$xselect()[iteration-transition_trees]))
    feature_counter[iteration+1] <- feature_counter[iteration] + 1
  } else{
    feature_counter[iteration+1] <- feature_counter[iteration]
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
  }
  
  
  fitted_values <- mb_tree$fitted()
  
  transition_trees_max <- iteration
  
  
  ################### Phase 4: Trees with user-specified depth #######################################
  
  iteration <- iteration + 1 
  
  mb_tree_max = blackboost(formula = formula, data = data, offset = fitted_values,
                           control = boost_control(nu = nu, mstop = 1),
                           tree_controls = partykit::ctree_control(
                             teststat = "quad",
                             testtype = "Teststatistic",
                             mincriterion = 0,
                             minsplit = 10, 
                             minbucket = 4,
                             maxdepth = 2, 
                             saveinfo = FALSE))
  
  # Check if feature added is new
  if(!mb_tree_max$xselect()[iteration-transition_trees_max] %in% feature_list){
    feature_list <- c(feature_list,as.character(mb_tree_max$xselect()[iteration-transition_trees_max]))
    feature_counter[iteration+1] <- feature_counter[iteration] + 1
  } else{
    feature_counter[iteration+1] <- feature_counter[iteration]
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
  return_list[["Input_Parameters"]] <-c(nu, iteration, epsilon, formula, bl2)
  return_list[["Data"]] <- X
  return_list[["FeatureNames"]] <- f_names
  return_list[["FeatureLevels"]] <- lapply(data, levels.default)
  return_list[["Feature_Counter"]] <- feature_counter
  return_list[["Riskfunction"]] <- riskfct
  
  # Print the coefficients of the final model
  return(return_list)
}







predictLearner.regr.icb = function (.learner, .model, .newdata, ...) 
{
  icb_object <- .model$learner.model
  
  # Check for NAs and exclude the rows with NAs
  if(anyNA(.newdata)){
    X_new <- na.omit(.newdata)
    warning("Rows with NAs were excluded.")
  } else{
    X_new <- .newdata
  }
  
  
  # If new factor levels occur in X_new, convert them to NAs
  for (f in 1:dim(X_new)[2]){
    if(colnames(X_new)[f] %in% icb_object$FeatureNames & is.factor(X_new[,f])){
      f_levels = eval(parse(text = paste0("icb_object$FeatureLevels$",colnames(X_new)[f]) ))
      X_new[,f] <- factor(X_new[,f], levels = f_levels)
    }
  }
  
  # Exclude the NAs (new factor levels)
  if(anyNA(X_new)){
    X_new <- na.omit(X_new)
    warning("Rows with new factor levels excluded.")
  } else{
    X_new <- X_new
  }
  
  
  # Only allow the feature which have been in the training data
  #X_new <- X_new[,which(colnames(X_new) %in% icb_object$FeatureNames)]
  
  #X_new <- droplevels(X_new)
  
  # Create an empty vector with the length of newdata
  prediction <- vector(mode = "numeric", length = dim(X_new)[1])
  
  # Fill every entry with the intercept of the model
  for (l in 1:length(prediction)){
    prediction[l] <- icb_object$Prediction_Models$Linear$offset[[1]]
  }
  
  # Offset prediction
  prediction_offset <- prediction
  
  
  iteration <- 1
  
  
  ################# For the linear part: #####################################################
  
  while(iteration <= (icb_object$`Transition Iterations`[1])){
    
    pred_iteration <- icb_object$Prediction_Models$Linear[iteration]$predict(newdata = X_new)
    
    iteration <- iteration + 1
    
  }
  
  # Save the predictions after the linear part
  prediction_linear <- prediction_offset + pred_iteration
  
  
  ###################### For the splines part: ###############################################
  
  while(iteration <= (icb_object$`Transition Iterations`[2])){
    
    pred_iteration <- icb_object$Prediction_Models$Spline[iteration - icb_object$`Transition Iterations`[1]]$predict(newdata = X_new)
    
    iteration <- iteration + 1
  }
  
  prediction_spline <- prediction_linear + pred_iteration
  
  
  
  # #################### For the tree (depth=2) part: ##########################################
  
  while(iteration <= (icb_object$`Transition Iterations`[3])){
    
    pred_iteration <- icb_object$Prediction_Models$Tree[iteration - icb_object$`Transition Iterations`[2]]$predict(newdata = X_new)
    
    iteration <- iteration + 1
  }
  
  prediction_tree <- prediction_spline + pred_iteration
  
  
  #################  For the tree (depth via user input) part: ####################################
  
  while(iteration < length(icb_object$Risk)){
    
    pred_iteration <- icb_object$Prediction_Models$TreeMax[iteration - icb_object$`Transition Iterations`[3]]$predict(newdata = X_new)
    
    iteration <- iteration + 1
  }
  
  
  prediction_tree_max <- prediction_tree + pred_iteration
  
  return(as.numeric(prediction_tree_max))
}




#####

# Classification learner


makeRLearner.classif.icb = function() {
  makeRLearnerClassif(
    cl = "classif.icb",
    package = c("mboost","partykit","caret"),
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "nu", lower = 0, upper = 1, default = 0.1),
      makeNumericLearnerParam(id = "epsilon", lower = 0, upper = 1, default = 0.005),
      makeDiscreteLearnerParam(id = "bl2", default = "btree", values = c("bbs","btree")),
      makeIntegerLearnerParam(id = "max_depth", default = 8L),
      makeIntegerLearnerParam(id = "min_split", default = 20L),
      makeIntegerLearnerParam(id = "min_bucket", default = 7L),
      makeIntegerLearnerParam(id = "df_spline", default = 2L)
    ),
    properties = c("twoclass","numerics", "factors","prob","ordered"),
    name = "Gradually interpretable component-wise boosting",
    short.name = "icb.classif",
    note = ""
  )
}


trainLearner.classif.icb = function (.learner, .task, .subset, .weights = NULL, ...) 
{
  formula = getTaskFormula(.task)
  data = getTaskData(.task, .subset)
  
  nu <- .learner$par.vals$nu
  epsilon <- .learner$par.vals$epsilon
  bl2 <- .learner$par.vals$bl2
  max_depth <- .learner$par.vals$max_depth
  min_split <- .learner$par.vals$min_split
  min_bucket <- .learner$par.vals$min_bucket
  df_spline <- .learner$par.vals$df_spline
  
  # Preparing the formula and data by seperating the target(y) and the features(X)
  data <- na.omit(data)
  
  # Get the target
  target <- all.vars(formula)[1]
  
  # Create a list of all features names as specified in the formula
  all_vars <- all.vars(formula)
  if(all_vars[2] != "."){
    data <- data[,colnames(data) %in% all_vars]
  }
  
  target_data <- data.frame(data[,target])
  colnames(target_data) <- target
  
  data <- droplevels(data)
  
  # Make one-hot encoding for factor variables
  # dummies <- dummyVars(" ~ .", data = data[,colnames(data) != target], fullRank = T)
  # data <- data.frame(predict(dummies, newdata = data[,colnames(data) != target]))
  # data <- data.frame(data,target_data)
  
  # Create mlr task to get full formula
  formula <- as.formula(paste(target,"~ ."))
  
  # Save feature names of one-hot-encoded data
  f_names <- colnames(data)[which(colnames(data) != target)]
  
  # Create the feature matrix
  X <- model.matrix(formula, data)
  
  # Create a vector that contains the number of unique values for every feature
  #len_vector <- as.vector(sapply(sapply(data[,colnames(data) != target], unique), length))
  #len_boolean <- len_vector < 3
  
  # Create a counter for how many features are in the model
  feature_counter <- numeric()
  feature_counter[1] <- 0
  feature_list <- c()
  
  
  levels <- "0"
  
  # Create a target variable with {-1,1} encoding
  data[,target] <- as.factor(data[,target])
  y <- data[, target]
  y_int <- numeric(length(y))
  for(l in 1:length(y)){
    if(y[l] == levels(y)[1]){
      y_int[l] <- -1
    }else if(y[l] == levels(y)[2]){
      y_int[l] <- 1
    }
  }
  target_levels <- levels(y)
  
  # Load the gradient and risk function (using the mboost family.R code)
  family = Binomial()
  ngradient <- family@ngradient
  riskfct <- family@risk
  
  # Fit an intercept model
  intercept_model <- glm.fit(x=as.matrix(X[,1]), y=y, family = binomial(link = "logit"))
  fit_0 <- intercept_model$fitted.values
  fitted_values <- fit_0
  
  
  #Calculate the risk of the intercept model
  risk_0 <- riskfct(y = y_int, f = fitted_values)
  
  
  
  
  ################# Phase 1: Linear models as base learners #####################################
  
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
  
  
  while((mb_linear$risk()[iteration] / mb_linear$risk()[iteration+1]) >= (1 + epsilon)){
    
    #Add one to the iteration number
    iteration <- iteration + 1
    
    # Create next iteration 
    mb_linear <- mb_linear[iteration]
    
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
  
  
  ############################ Phase 2: Splines / Tree Stumps ##################################
  
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
  
  transition_trees <- iteration
  
  # Save fitted values of the last linear model iteration to use them as offset
  fitted_values <- mb_spline$fitted()
  
  

  ############## Phase 3: Trees with depth=2 ####################################################
  
  iteration <- iteration + 1 
  
  mb_tree = blackboost(formula = formula, data = data, offset = fitted_values,
                       control = boost_control(nu = nu, mstop = 1), family = Binomial(),
                       tree_controls = partykit::ctree_control(
                         teststat = "quad",
                         testtype = "Teststatistic",
                         mincriterion = 0,
                         minsplit = 10, 
                         minbucket = 4,
                         maxdepth = 2, 
                         saveinfo = FALSE))
  
  # Check if feature added is new
  if(!mb_tree$xselect()[iteration-transition_trees] %in% feature_list){
    feature_list <- c(feature_list,as.character(mb_tree$xselect()[iteration-transition_trees]))
    feature_counter[iteration+1] <- feature_counter[iteration] + 1
  } else{
    feature_counter[iteration+1] <- feature_counter[iteration]
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
    
  }
  
  
  fitted_values <- mb_tree$fitted()
  
  transition_trees_max <- iteration
  
  
  
  
  ################## Phase 4: Trees with user-specified depth #####################################
  
  iteration <- iteration + 1 
  
  mb_tree_max = blackboost(formula = formula, data = data, offset = fitted_values,
                           control = boost_control(nu = nu, mstop = 1), family = Binomial(),
                           tree_controls = partykit::ctree_control(
                             teststat = "quad",
                             testtype = "Teststatistic",
                             mincriterion = 0,
                             minsplit = 10, 
                             minbucket = 4,
                             maxdepth = max_depth, 
                             saveinfo = FALSE))
  
  # Check if feature added is new
  if(!mb_tree_max$xselect()[iteration-transition_trees_max] %in% feature_list){
    feature_list <- c(feature_list,as.character(mb_tree_max$xselect()[iteration-transition_trees_max]))
    feature_counter[iteration+1] <- feature_counter[iteration] + 1
  } else{
    feature_counter[iteration+1] <- feature_counter[iteration]
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
  return_list[["Input_Parameters"]] <-c(nu, iteration, epsilon, bl2, df_spline)
  return_list[["TargetLevels"]] <- target_levels
  return_list[["Data"]] <- X
  return_list[["FeatureNames"]] <- f_names
  return_list[["FeatureLevels"]] <- lapply(data, levels.default)
  return_list[["Feature_Counter"]] <- feature_counter
  return_list[["Riskfunction"]] <- riskfct
  
  # Print the coefficients of the final model
  return(return_list)
  

}







predictLearner.classif.icb = function (.learner, .model, .newdata, ...) 
{
  icb_object <- .model$learner.model
  
  # Check for NAs and exclude the rows with NAs
  if(anyNA(.newdata)){
    X_new <- na.omit(.newdata)
    warning("Rows with NAs were excluded.")
  } else{
    X_new <- .newdata
  }
  
  
  # If new factor levels occur in X_new, convert them to NAs
  for (f in 1:dim(X_new)[2]){
    if(colnames(X_new)[f] %in% icb_object$FeatureNames & is.factor(X_new[,f])){
      f_levels = eval(parse(text = paste0("icb_object$FeatureLevels$",colnames(X_new)[f]) ))
      X_new[,f] <- factor(X_new[,f], levels = f_levels)
    }
  }
  
  # Exclude the NAs (new factor levels)
  if(anyNA(X_new)){
    X_new <- na.omit(X_new)
    warning("Rows with new factor levels excluded.")
  } else{
    X_new <- X_new
  }
  
  
  # Only allow the feature which have been in the training data
  #X_new <- X_new[,which(colnames(X_new) %in% icb_object$FeatureNames)]
  
  #X_new <- droplevels(X_new)
  
  # Create an empty vector with the length of newdata
  prediction <- vector(mode = "numeric", length = dim(X_new)[1])
  
  # Fill every entry with the intercept of the model
  for (l in 1:length(prediction)){
    prediction[l] <- icb_object$Prediction_Models$Linear$offset[[1]]
  }
  
  # Offset prediction
  prediction_offset <- prediction
  
  
  iteration <- 1
  
  
  ################# For the linear part: #####################################################
  
  while(iteration <= (icb_object$`Transition Iterations`[1])){
    
    pred_iteration <- icb_object$Prediction_Models$Linear[iteration]$predict(newdata = X_new)
    
    iteration <- iteration + 1
    
  }
  
  # Save the predictions after the linear part
  prediction_linear <- prediction_offset + pred_iteration
  
  
  ###################### For the splines part: ###############################################
  
  while(iteration <= (icb_object$`Transition Iterations`[2])){
    
    pred_iteration <- icb_object$Prediction_Models$Spline[iteration - icb_object$`Transition Iterations`[1]]$predict(newdata = X_new)
    
    iteration <- iteration + 1
  }
  
  prediction_spline <- prediction_linear + pred_iteration
  
  
  # #################### For the tree (depth=2) part: ##########################################
  
  while(iteration <= (icb_object$`Transition Iterations`[3])){
    
    pred_iteration <- icb_object$Prediction_Models$Tree[iteration - icb_object$`Transition Iterations`[2]]$predict(newdata = X_new)
    
    iteration <- iteration + 1
  }
  
  prediction_tree <- prediction_spline + pred_iteration
  
  
  #################  For the tree (depth via user input) part: ####################################
  
  while(iteration < length(icb_object$Risk)){
    
    pred_iteration <- icb_object$Prediction_Models$TreeMax[iteration - icb_object$`Transition Iterations`[3]]$predict(newdata = X_new)
    
    iteration <- iteration + 1
  }
  
  
  prediction_tree_max <- prediction_tree + pred_iteration
  
  levels <- c(icb_object$TargetLevels[1],icb_object$TargetLevels[2])
  
  prediction_label <- numeric(length(prediction_tree_max))
  
  for(i in 1:length(prediction_tree_max)){
    if(prediction_tree_max[i] < 0){
      prediction_label[i] <- levels[1]
    } else{
      prediction_label[i] <- levels[2]
    }
  }
  
  
  return(as.factor(prediction_label))
  
  
}