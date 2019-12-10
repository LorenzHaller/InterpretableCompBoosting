pdp_function <- function(icb_object, newdata = NULL, ylim = NULL){
  
  data = icb_object$Data
  feature_names = icb_object$FeatureNames
  
  method <- icb_object$Input_Parameters[[6]]
  df_spline <- icb_object$Input_Parameters[[7]]
  
  which = as.integer(seq(1,dim(data)[2],by=1))
  
  
  
  for(w in which){
    
    ylim = NULL
    
    data_temp = data[,w,drop=FALSE]
    feature_name = feature_names[w]
    
    coeff_string = paste("icb_object$Prediction_Models$Linear$coef()$`bols(",feature_name,")`",sep="")
    linear_coefficients = eval(parse(text = coeff_string))
    
    ## If feature is a factor
    if(is.factor(data[,w])){
      if(!is.null(linear_coefficients[[1]])){
        plot.mboost(icb_object$Prediction_Models$Linear, w, type = "l")
      }
    }
    
    ## If feature is not a factor
    if(!is.factor(data[,w])){
      
      pr_linear = predict(icb_object$Prediction_Models$Linear, newdata = data, which = w)
      pr_spline = predict(icb_object$Prediction_Models$Spline, newdata = data, which = w)
      
      if(!is.null(linear_coefficients[[1]])){
        plot.mboost(icb_object$Prediction_Models$Linear, w, type = "l")
        
        if(pr_spline[1] != 0){
          pr = pr_linear + pr_spline
          
          points(sort(data_temp[,1]), pr[order(data_temp[,1]),1],type="b")
        }
        
      } else if(pr_spline[1] != 0){
        plot.mboost(icb_object$Prediction_Models$Spline, w)
        
      } 
    }
  }
}