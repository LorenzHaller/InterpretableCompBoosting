# Helper function to calculate the risk of the predicted labels

pred_label_risk <- function(fitted, target_class = target_class){
    
    predicted_train_labels <- numeric(length(fitted))
    for(lp in 1:length(predicted_train_labels)){
      if(fitted[lp] < 0){
        predicted_train_labels[lp] <- -1
      } else{
        predicted_train_labels[lp] <- 1
      }
    }
    return(predicted_train_labels)
    
}





# Create a function for risk / variance analysis for the different stages
library(formattable)

stage_risk <- function(micb_object = NULL, pred_object = NULL){
  
  if(!is.null(micb_object)){
    
    
    
    initial_risk <- round(micb_object$Risk[1],digits=2)
    risk_stage1 <- round(micb_object$Risk[micb_object$`Transition Iterations`[1]+1],digits=2)
    risk_stage2 <- round(micb_object$Risk[micb_object$`Transition Iterations`[2]+1],digits=2)
    risk_stage3 <- round(micb_object$Risk[micb_object$`Transition Iterations`[3]+1],digits=2)
    risk_stage4 <- round(micb_object$Risk[length(micb_object$Risk)],digits=2)
    
    perc_stage1 <- paste(round((1 - (risk_stage1/initial_risk)) * 100, digits=2),"%")
    perc_stage2 <- paste(round(((risk_stage1-risk_stage2)/initial_risk) * 100,digits=2),"%")
    perc_stage3 <- paste(round(((risk_stage2-risk_stage3)/initial_risk) * 100,digits=2),"%")
    perc_stage4 <- paste(round(((risk_stage3-risk_stage4)/initial_risk) * 100,digits=2),"%")
    
    cum_stage1 <- paste(round((1 - (risk_stage1/initial_risk))*100,digits=2),"%")
    cum_stage2 <- paste(round((1 - (risk_stage2/initial_risk))*100,digits=2),"%")
    cum_stage3 <- paste(round((1 - (risk_stage3/initial_risk))*100,digits=2),"%")
    cum_stage4 <- paste(round((1 - (risk_stage4/initial_risk))*100,digits=2),"%")
    
    col_names <- c("Risk left","%-Risk Explanation","Cumulative %-Risk Explanation",
                   "Number of iterations")
    
    table <- matrix("", nrow = 7, ncol = 4)
    
    row_names <- c("Intercept Model","Phase 1: Linear","Phase 2: Splines/Tree Stumps",
                   "Phase 3: Trees (depth=2)","Phase 4: Deeper Trees","","Interaction strength")
    
    interaction_strength = (risk_stage2-risk_stage4)/(initial_risk-risk_stage4)
    interaction_strength = round(interaction_strength,digits=3)
      
    table[,1] <- c(initial_risk,risk_stage1,risk_stage2,risk_stage3,risk_stage4,"",interaction_strength)
    table[,2] <- c("",perc_stage1,perc_stage2,perc_stage3,perc_stage4,"","")
    table[,3] <- c("",cum_stage1,cum_stage2,cum_stage3,cum_stage4,"","")
    table[,4] <- c(1, length(micb_object$Prediction_Models$Linear$risk()),
                   length(micb_object$Prediction_Models$Spline$risk()),
                   length(micb_object$Prediction_Models$Tree$risk()),
                   length(micb_object$Prediction_Models$TreeMax$risk()),"","")
    
    df <- as.data.frame(table)
    colnames(df) <- col_names
    rownames(df) <- row_names
    
    formattable(df)
  } else if(!is.null(pred_object)){
    
    initial_risk <- round(pred_object$TestRisk[1],digits=2)
    risk_stage1 <- round(pred_object$TestRisk[pred_object$`Transition Iterations`[1]+1],digits=2)
    risk_stage2 <- round(pred_object$TestRisk[pred_object$`Transition Iterations`[2]+1],digits=2)
    risk_stage3 <- round(pred_object$TestRisk[pred_object$`Transition Iterations`[3]+1],digits=2)
    risk_stage4 <- round(pred_object$TestRisk[length(pred_object$TestRisk)],digits=2)
    
    perc_stage1 <- paste(round((1 - (risk_stage1/initial_risk)) * 100, digits=2),"%")
    perc_stage2 <- paste(round(((risk_stage1-risk_stage2)/initial_risk) * 100,digits=2),"%")
    perc_stage3 <- paste(round(((risk_stage2-risk_stage3)/initial_risk) * 100,digits=2),"%")
    perc_stage4 <- paste(round(((risk_stage3-risk_stage4)/initial_risk) * 100,digits=2),"%")
    
    cum_stage1 <- paste(round((1 - (risk_stage1/initial_risk))*100,digits=2),"%")
    cum_stage2 <- paste(round((1 - (risk_stage2/initial_risk))*100,digits=2),"%")
    cum_stage3 <- paste(round((1 - (risk_stage3/initial_risk))*100,digits=2),"%")
    cum_stage4 <- paste(round((1 - (risk_stage4/initial_risk))*100,digits=2),"%")
    
    
    col_names <- c("Risk left","%-Risk Explanation","Cumulative %-Risk Explanation",
                   "Number of iterations")
    
    table <- matrix("", nrow = 7, ncol = 4)
    
    row_names <- c("Intercept Model","Phase 1: Linear","Phase 2: Splines/Tree Stumps",
                   "Phase 3: Trees (depth=2)","Phase 4: Deeper Trees","","Interaction strength")
    
    interaction_strength = (risk_stage2-risk_stage4)/(initial_risk-risk_stage4)
    interaction_strength = round(interaction_strength,digits=3)
    
    table[,1] <- c(initial_risk,risk_stage1,risk_stage2,risk_stage3,risk_stage4,"",interaction_strength)
    table[,2] <- c("",perc_stage1,perc_stage2,perc_stage3,perc_stage4,"","")
    table[,3] <- c("",cum_stage1,cum_stage2,cum_stage3,cum_stage4,"","")
    table[,4] <- c(1, pred_object$`Transition Iterations`[1]+1,
                   pred_object$`Transition Iterations`[2]-pred_object$`Transition Iterations`[1]+1,
                   pred_object$`Transition Iterations`[3]-pred_object$`Transition Iterations`[2]+1,
                   pred_object$`Transition Iterations`[4]-pred_object$`Transition Iterations`[3]
                   ,"","")
    
    df <- as.data.frame(table)
    colnames(df) <- col_names
    rownames(df) <- row_names
    
    formattable(df)
    
  }
}


# Create a function for defaults plots

plot.icb = function(micb_object = NULL, predict_object = NULL, fcount = FALSE,
                    col1 = "red", col2 = "blue", data_name = NULL){
  
  if(is.null(micb_object)){
    stop("No icb object to plot for!")
  }
  
  if(!is.null(data_name)){
    title <- paste("MSE vs Iterations for",data_name,"dataset")
  } else{
    title <- "MSE vs Iterations"
  }
  
  # Plot number of features over time
  par(mar = c(5, 5, 3, 5))
  plot(1:length(micb_object$Risk),micb_object$Risk, xlab="Iteration",
       ylab="Mean Squared Error",col=col1,type="l", 
       ylim=c(0,max(micb_object$Risk)),xlim=c(0,micb_object$Input_Parameters[[2]]),
       main=title, lwd = 2)
  
  # qplot(x=1:length(micb_object$Risk), y=micb_object$Risk, geom="line",
  #       xlab="Iteration",ylab="Mean Squared Error",colour=col1, 
  #       ylim=c(0,max(micb_object$Risk)),xlim=c(0,micb_object$Input_Parameters[[2]]),
  #       main=title)
  
  # Add vertical lines where the stages change
  abline(v = micb_object$`Transition Iterations`[1]+1, lty = 2, col = "darkgray")
  abline(v = micb_object$`Transition Iterations`[2]+1, lty = 2, col = "darkgray")
  abline(v = micb_object$`Transition Iterations`[3]+1, lty = 2, col = "darkgray")

  
  # Add line for predictions if available
  if(!is.null(predict_object)){
    points(1:length(predict_object$TestRisk),predict_object$TestRisk,
           type="l",col="blue",lwd=2)
  }
  
  # Add visualisation for the number of features used by the model
  if(fcount == TRUE){
    par(new = TRUE)
    plot(1:length(micb_object$Feature_Counter),micb_object$Feature_Counter,
         type = "l", xaxt = "n", yaxt = "n",
         ylab = "", xlab = "")
    axis(side = 4)
    mtext("Number of features", side = 4, line = 3)
    
  }
  
  # Add line for predictions if available
  if(!is.null(predict_object)){
    legend("topright", c("Train", "Test", "Stage Transitions"),
           col = c("red", "blue", "darkgray"), lty = c(1, 2),cex=0.7)
  } else{
    legend("topright", c("Train", "Stage Transitions"),
           col = c("red", "darkgray"), lty = c(1, 2),cex=0.7)
  }
  
}



data_risk_table <- function(icb_list, train = TRUE, data_names = NULL){
  # icb_list: a list of trained icb objects (train = TRUE) or prediction objects (train = FALSE)
  # for every data set in the list add a line in the table
  
  
  
  table <- matrix("", nrow = length(icb_list), ncol = 6)
  
  if(is.null(data_names)){
    data_names <- as.character(1:length(icb_list))
  }
  
  
  
  if(isTRUE(train)){
    
    col_names <- c("Training Data %-Risk Explanation","Stage 1 (Linear)","Stage 2 (Non-linear)",
                   "Stage 3 (Trees of depth 2)","Stage 4 (Deeper Trees)",
                   "Overall %-Risk Explanation")
    
    for(i in 1:length(icb_list)){
      
      initial_risk <-round(icb_list[[i]]$Risk[1],digits=2)
      risk_stage1 <- round(icb_list[[i]]$Risk[icb_list[[i]]$`Transition Iterations`[1]+1],digits=2)
      risk_stage2 <- round(icb_list[[i]]$Risk[icb_list[[i]]$`Transition Iterations`[2]+1],digits=2)
      risk_stage3 <- round(icb_list[[i]]$Risk[icb_list[[i]]$`Transition Iterations`[3]+1],digits=2)
      risk_stage4 <- round(icb_list[[i]]$Risk[length(icb_list[[i]]$Risk)],digits=2)
      
      perc_stage1 <- paste(round((1 - (risk_stage1/initial_risk)) * 100, digits=2),"%")
      perc_stage2 <- paste(round(((risk_stage1-risk_stage2)/initial_risk) * 100,digits=2),"%")
      perc_stage3 <- paste(round(((risk_stage2-risk_stage3)/initial_risk) * 100,digits=2),"%")
      perc_stage4 <- paste(round(((risk_stage3-risk_stage4)/initial_risk) * 100,digits=2),"%")
      
      perc_final <- paste(round((1 - (risk_stage4/initial_risk)) * 100, digits=2),"%")
      
      table[i,] <- c(data_names[i],perc_stage1,perc_stage2,perc_stage3,perc_stage4, perc_final)
      
    }
    
    
    df <- as.data.frame(table)
    colnames(df) <- col_names
    #rownames(df) <- data_names
    customGreen = "#71CA97"
    customGreen0 = "#DeF7E9"
    formattable(df, align =c("l","c","c","c","c","r"),
                    list( `Training Data %-Risk Explanation` = formatter("span", 
                                                       style = ~ style(color = "grey",font.weight = "bold")),
                      `Overall %-Risk Explanation`= color_tile(customGreen0, customGreen)))
    
  }
  
  else if(isFALSE(train)){
    
    col_names <- c("Test Data %-Risk Explanation","Stage 1 (Linear)","Stage 2 (Non-linear)",
                   "Stage 3 (Trees of depth 2)","Stage 4 (Deeper Trees)",
                   "Overall %-Risk Explanation")
    
    for(i in 1:length(icb_list)){
      
      initial_risk <-round(icb_list[[i]]$TestRisk[1],digits=2)
      risk_stage1 <- round(icb_list[[i]]$TestRisk[icb_list[[i]]$`Transition Iterations`[1]+1],digits=2)
      risk_stage2 <- round(icb_list[[i]]$TestRisk[icb_list[[i]]$`Transition Iterations`[2]+1],digits=2)
      risk_stage3 <- round(icb_list[[i]]$TestRisk[icb_list[[i]]$`Transition Iterations`[3]+1],digits=2)
      risk_stage4 <- round(icb_list[[i]]$TestRisk[length(icb_list[[i]]$TestRisk)],digits=2)
      
      perc_stage1 <- paste(round((1 - (risk_stage1/initial_risk)) * 100, digits=2),"%")
      perc_stage2 <- paste(round(((risk_stage1-risk_stage2)/initial_risk) * 100,digits=2),"%")
      perc_stage3 <- paste(round(((risk_stage2-risk_stage3)/initial_risk) * 100,digits=2),"%")
      perc_stage4 <- paste(round(((risk_stage3-risk_stage4)/initial_risk) * 100,digits=2),"%")
      
      perc_final <- paste(round((1 - (risk_stage4/initial_risk)) * 100, digits=2),"%")
      
      table[i,] <- c(data_names[i],perc_stage1,perc_stage2,perc_stage3,perc_stage4, perc_final)
      
    }
  
    df <- as.data.frame(table)
    colnames(df) <- col_names
    #rownames(df) <- data_names
    customGreen = "#71CA97"
    customGreen0 = "#DeF7E9"
    formattable(df, align =c("l","c","c","c","c","r"),
                list( `Test Data %-Risk Explanation` = formatter("span", 
                                                       style = ~ style(color = "grey",font.weight = "bold")),
                      `Overall %-Risk Explanation`= color_tile(customGreen0, customGreen)))
  
  }
  
}



individual_stage_risk <- function(pred_object, subset = NULL){
  
  if(is.null(subset)){
    ind_matrix <- pred_object$IndividualRisk
    ind_table <- ind_matrix[,2:5]
    row_names <- paste("Obs.", as.character(1:dim(ind_table)[1]))
  } else{
    subset <- sort(subset)
    ind_matrix <- pred_object$IndividualRisk[subset,]
    ind_table <- ind_matrix[,2:5]
    row_names <- paste("Obs.", subset)
  }
  
  ind_table[,1] <- paste(round((1 - (ind_matrix[,2] / ind_matrix[,1])) * 100, digits=2),"%")
  ind_table[,2] <- paste(round(((ind_matrix[,2]-ind_matrix[,3]) / ind_matrix[,1]) * 100, digits=2),"%")
  ind_table[,3] <- paste(round(((ind_matrix[,3]-ind_matrix[,4]) / ind_matrix[,1]) * 100, digits=2),"%")
  ind_table[,4] <- paste(round(((ind_matrix[,4]-ind_matrix[,5]) / ind_matrix[,1]) * 100, digits=2),"%")
  
  ind_table <- cbind(row_names,ind_table,paste(round((1 - (ind_matrix[,5]/ind_matrix[,1])) * 100, digits=2),"%"))
  
  ind_df <- as.data.frame(ind_table)
  #rownames(ind_df) <- row_names
  colnames(ind_df) <- c("Individual %-Loss Explanation","Stage 1 (Linear)","Stage 2 (Non-linear)",
                        "Stage 3 (Trees of depth 2)","Stage 4 (Deeper Trees)",
                        "Overall %-Loss Explanation")
  
  customGreen = "#71CA97"
  customGreen0 = "#DeF7E9"
  formattable(ind_df, align =c("l","c","c","c","c","r"),
              list( `Individual %-Loss Explanation` = formatter("span", 
                                                               style = ~ style(color = "grey",font.weight = "bold")),
                    `Overall %-Loss Explanation`= color_tile(customGreen0, customGreen)))
  
}


individual_barplot <- function(pred_object, subset = NULL, plot.which = c("Loss","Prediction")){
  
  if(is.null(subset)){
    if(plot.which == "Loss"){
      ind_matrix <- pred_object$IndividualRisk
    } else if(plot.which == "Prediction"){
      ind_matrix <- pred_object$StagePredictions
    }
    
    ind_table <- ind_matrix[,2:5]
    row_names <- paste("Obs.", as.character(1:dim(ind_table)[1]))
  } else{
    subset <- sort(subset)
    if(plot.which == "Loss"){
      ind_matrix <- pred_object$IndividualRisk[subset,]
    } else if(plot.which == "Prediction"){
      ind_matrix <- pred_object$StagePredictions[subset,]
    }
    ind_table <- ind_matrix[,2:5]
    row_names <- paste("Obs.", subset)
  }
  
  
  if(plot.which == "Loss"){
    
    abs_data_frame = data.frame(Observation = character(),
                                Stage = character(),
                                Loss = numeric())
    
    for(o in 1:length(row_names)){
      abs_matrix_temp = matrix(0, nrow = 4, ncol = 3)
      abs_matrix_temp[1,] = c(row_names[o], "Linear", ind_matrix[o,1] - ind_matrix[o,2])
      abs_matrix_temp[2,] = c(row_names[o], "Non Linear", ind_matrix[o,2] - ind_matrix[o,3])
      abs_matrix_temp[3,] = c(row_names[o], "Trees of depth 2", ind_matrix[o,3] - ind_matrix[o,4])
      abs_matrix_temp[4,] = c(row_names[o], "Deeper Trees", ind_matrix[o,4] - ind_matrix[o,5])
      abs_df_temp = as.data.frame(abs_matrix_temp)
      colnames(abs_df_temp) <- c("Observation","Stage","Loss")
      
      abs_data_frame <- rbind(abs_data_frame,abs_df_temp)
    }
    
    abs_data_frame$Stage <- factor(abs_data_frame$Stage, 
                                   levels = c('Deeper Trees','Trees of depth 2','Non Linear','Linear'))
    
    abs_data_frame$Loss <- as.numeric(as.character(abs_data_frame$Loss))
    
    ggplot(abs_data_frame, aes(x = Observation, y = Loss, fill = Stage))  + 
      ggtitle("Absolute loss improvement per stage") + 
      geom_bar(stat='identity',position=position_dodge()) + 
      # geom_text(aes(y=Loss, label=Loss), vjust=1.6, 
      #           color="black", size=3.5) +
      coord_flip()
    
    
  } else if(plot.which == "Prediction"){
    
    abs_data_frame = data.frame(Observation = character(),
                                Stage = character(),
                                Prediction = numeric())
    
    for(o in 1:length(row_names)){
      abs_matrix_temp = matrix(0, nrow = 4, ncol = 3)
      abs_matrix_temp[1,] = c(row_names[o], "Linear", ind_matrix[o,2])
      abs_matrix_temp[2,] = c(row_names[o], "Non Linear", ind_matrix[o,3])
      abs_matrix_temp[3,] = c(row_names[o], "Trees of depth 2", ind_matrix[o,4])
      abs_matrix_temp[4,] = c(row_names[o], "Deeper Trees", ind_matrix[o,5])
      abs_df_temp = as.data.frame(abs_matrix_temp)
      colnames(abs_df_temp) <- c("Observation","Stage","Prediction")
      
      abs_data_frame <- rbind(abs_data_frame,abs_df_temp)
    }
    
    abs_data_frame$Stage <- factor(abs_data_frame$Stage, 
                                   levels = c('Deeper Trees','Trees of depth 2','Non Linear','Linear'))
    abs_data_frame$Observation <- factor(abs_data_frame$Observation, 
                                   levels = levels(abs_data_frame$Observation))
    
    abs_data_frame$Prediction <- as.numeric(as.character(abs_data_frame$Prediction))
    
    ggplot(abs_data_frame, aes(x = Observation, y = Prediction, fill = Stage))  + 
      ggtitle("Prediction per stage") + 
      geom_hline(yintercept = as.numeric(ind_matrix[[1,1]]), linetype = "dotted", size = 0.75) +
      geom_bar(stat='identity',position=position_dodge()) + 
      annotate(geom="text", y=as.numeric(ind_matrix[[1,1]]), x=row_names[1], label="Intercept",
               color="black", size = 4, family = "sans") +
      # geom_text(aes(y=Loss, label=Loss), vjust=1.6, 
      #           color="black", size=3.5) +
      coord_flip()
  }
  
  
  
  
  
  # # Table for absolute differences in risk between stage and previous stage
  # abs_table <- ind_table
  # abs_table[,4] <- ind_matrix[,1] - ind_matrix[,2]
  # abs_table[,3] <- ind_matrix[,2] - ind_matrix[,3]
  # abs_table[,2] <- ind_matrix[,3] - ind_matrix[,4]
  # abs_table[,1] <- ind_matrix[,4] - ind_matrix[,5]
  # 
  # abs_data <- as.data.frame(abs_table)
  # colnames(abs_data) <- c("Lin","Nonlin","Trees","DeepTrees")
  # abs_data$index <- 1:dim(abs_data)[1]
  # 
  # 
  # rel_table <- ind_table
  # rel_table[,1] <- paste(round((1 - (ind_matrix[,2] / ind_matrix[,1])) * 100, digits=2),"%")
  # rel_table[,2] <- paste(round(((ind_matrix[,2]-ind_matrix[,3]) / ind_matrix[,1]) * 100, digits=2),"%")
  # rel_table[,3] <- paste(round(((ind_matrix[,3]-ind_matrix[,4]) / ind_matrix[,1]) * 100, digits=2),"%")
  # rel_table[,4] <- paste(round(((ind_matrix[,4]-ind_matrix[,5]) / ind_matrix[,1]) * 100, digits=2),"%")
  # 
  
  # barplot(t(abs_table), horiz = TRUE, beside = T, names.arg = row_names,
  #         main = "Absolute loss improvement per stage"
  #         #,legend.text = c("Deeper trees","Trees of depth 2","Non-linear","Linear")
  #         )
  
  
  
}


main_effect_plot  <- function(icb_object, data, feature = NULL,
                              grid.size = 100, epsilon = 0.01){
  
  if(is.null(feature)){
    stop("No feature selected!")
  }
  
  if(is.factor(eval(parse(text = paste("data$",feature,sep=""))))){
    stop("Stop. Factors are not supported.")
  }
  
  w = which(icb_object$FeatureNames == feature)
  
  d = which(colnames(data) == feature)
  
  if(length(w) != 1 | length(d) != 1){
    stop("Selected feature not available!")
  }
  
  #pr_linear = predict(icb_object$Prediction_Models$Linear, newdata = data, which = w)
  pr_spline = predict(icb_object$Prediction_Models$Spline, newdata = data, which = w)
  
  
  coeff_string = paste("icb_object$Prediction_Models$Linear$coef()$`bols(",feature,")`",sep="")
  linear_coefficients = eval(parse(text = coeff_string))
  
  if(pr_spline[1] != 0){
    
    mod = icb_object$Prediction_Models$Spline
    
    pred.icb = iml::Predictor$new(mod, data)
    
    fc = FunComplexity$new(pred.icb, epsilon = epsilon, grid.size = grid.size)
    
    plot(fc$approx_models[[d]])
    
  } else if(pr_spline[1] == 0 & !is.null(linear_coefficients[[1]])){
    
    mod = icb_object$Prediction_Models$Linear
    
    pred.icb = iml::Predictor$new(mod, data)
    
    fc = FunComplexity$new(pred.icb, epsilon = epsilon, grid.size = grid.size)
    
    plot(fc$approx_models[[d]])
    
  } else{
    
    stop("Feature not included in first and second stage of the model.")
  }
    
  
}