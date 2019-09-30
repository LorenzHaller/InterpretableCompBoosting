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

stage_risk <- function(micb_object){
  
  initial_risk <- round(micb_object$Risk[1],digits=1)
  risk_stage1 <- round(micb_object$Risk[micb_object$`Transition Iterations`[1]+1],digits=1)
  risk_stage2 <- round(micb_object$Risk[micb_object$`Transition Iterations`[2]+1],digits=1)
  risk_stage3 <- round(micb_object$Risk[length(micb_object$Risk)],digits=1)
  
  perc_stage1 <- paste(round((1 - (risk_stage1/initial_risk)) * 100, digits=2),"%")
  perc_stage2 <- paste(round(((risk_stage1-risk_stage2)/initial_risk) * 100,digits=2),"%")
  perc_stage3 <- paste(round(((risk_stage2-risk_stage3)/initial_risk) * 100,digits=2),"%")
  
  cum_stage1 <- paste(round((1 - (risk_stage1/initial_risk))*100,digits=2),"%")
  cum_stage2 <- paste(round((1 - (risk_stage2/initial_risk))*100,digits=2),"%")
  cum_stage3 <- paste(round((1 - (risk_stage3/initial_risk))*100,digits=2),"%")
  
  col_names <- c("Risk left","%-Risk Explanation","Cummulative %-Risk Explanation")
  
  table <- matrix("", nrow = 4, ncol = 3)
  
  row_names <- c("Intercept Model","Phase 1: Linear","Phase 2: Splines/Tree Stumps","Phase 3: Trees (depth=2)")
  
  table[,1] <- c(initial_risk,risk_stage1,risk_stage2,risk_stage3)
  table[,2] <- c("",perc_stage1,perc_stage2,perc_stage3)
  table[,3] <- c("",cum_stage1,cum_stage2,cum_stage3)
  
  df <- as.data.frame(table)
  colnames(df) <- col_names
  rownames(df) <- row_names
  
  formattable(df)
}


