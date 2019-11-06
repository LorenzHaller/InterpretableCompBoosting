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
    
    table <- matrix("", nrow = 5, ncol = 4)
    
    row_names <- c("Intercept Model","Phase 1: Linear","Phase 2: Splines/Tree Stumps",
                   "Phase 3: Trees (depth=2)","Phase 4: Deeper Trees")
    
    table[,1] <- c(initial_risk,risk_stage1,risk_stage2,risk_stage3,risk_stage4)
    table[,2] <- c("",perc_stage1,perc_stage2,perc_stage3,perc_stage4)
    table[,3] <- c("",cum_stage1,cum_stage2,cum_stage3,cum_stage4)
    table[,4] <- c(1, length(micb_object$Prediction_Models$Linear$risk()),
                   length(micb_object$Prediction_Models$Spline$risk()),
                   length(micb_object$Prediction_Models$Tree$risk()),
                   length(micb_object$Prediction_Models$TreeMax$risk()))
    
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
    
    table <- matrix("", nrow = 5, ncol = 4)
    
    row_names <- c("Intercept Model","Phase 1: Linear","Phase 2: Splines/Tree Stumps",
                   "Phase 3: Trees (depth=2)","Phase 4: Deeper Trees")
    
    table[,1] <- c(initial_risk,risk_stage1,risk_stage2,risk_stage3,risk_stage4)
    table[,2] <- c("",perc_stage1,perc_stage2,perc_stage3,perc_stage4)
    table[,3] <- c("",cum_stage1,cum_stage2,cum_stage3,cum_stage4)
    table[,4] <- c(1, pred_object$`Transition Iterations`[1]+1,
                   pred_object$`Transition Iterations`[2]-pred_object$`Transition Iterations`[1]+1,
                   pred_object$`Transition Iterations`[3]-pred_object$`Transition Iterations`[2]+1,
                   pred_object$`Transition Iterations`[4]-pred_object$`Transition Iterations`[3]
                   )
    
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
  
  # Add vertical lines where the stages change
  abline(v = micb_object$`Transition Iterations`[1]+1, lty = 2, col = "darkgray")
  abline(v = micb_object$`Transition Iterations`[2]+1, lty = 2, col = "darkgray")
  abline(v = micb_object$`Transition Iterations`[3]+1, lty = 2, col = "darkgray")
  
  # Add line for predictions if available
  if(!is.null(predict_object)){
    points(1:length(predict_object$TestRisk),predict_object$TestRisk,
           type="l",col="blue",lwd=2)
    legend("top", c("Train", "Test", "Stage Transitions"),
           col = c("red", "blue", "darkgray"), lty = c(1, 2),cex=0.7)
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
  
}



data_risk_table <- function(icb_list, train = TRUE, data_names){
  # icb_list: a list of trained icb objects (train = TRUE) or prediction objects (train = FALSE)
  # for every data set in the list add a line in the table
  
  col_names <- c("%-Risk Explanation in stage 1 (Linear)","%-Risk Explanation in stage 2 (Non-linear)",
                 "%-Risk Explanation in stage 3 (Trees of depth 2)","%-Risk Explanation in stage 4 (Deeper Trees)")
  
  table <- matrix("", nrow = length(icb_list), ncol = 4)
  
  row_names <- 1:length(icb_list)
  
  
  if(isTRUE(train)){
    
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
      
      table[i,] <- c(perc_stage1,perc_stage2,perc_stage3,perc_stage4)
      
    }
    
    
    df <- as.data.frame(table)
    colnames(df) <- col_names
    rownames(df) <- data_names
    formattable(df)
    
  }
  
  else if(isFALSE(train)){
    
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
      
      table[i,] <- c(perc_stage1,perc_stage2,perc_stage3,perc_stage4)
      
    }
  
    df <- as.data.frame(table)
    colnames(df) <- col_names
    rownames(df) <- data_names
    formattable(df)
  
  }
  
}



individual_stage_risk <- function(pred_object){
  
  
  
  
}