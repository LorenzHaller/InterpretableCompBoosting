# Helper function to calculate the risk of the predicted labels

pred_label_risk <- function(fitted, y, target_class = target_class){
    
    predicted_train_labels <- numeric(length(fitted))
    for(lp in 1:length(predicted_train_labels)){
      if(fitted[lp] < 0){
        predicted_train_labels[lp] <- 0
      } else{
        predicted_train_labels[lp] <- 1
      }
    }
    return(riskfct(y = y_int, f = predicted_train_labels))
    
}