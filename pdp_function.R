pdp_function <- function(icb_object, newdata = NULL, ylim = NULL){
  
  data = icb_object$Data[,-1]
  feature_names = colnames(data)
  
  which = as.integer(seq(1,dim(data)[2],by=1))
  
  
  for(w in which){
    
    ylim = NULL
    
    data_temp = data[,w,drop=FALSE]
    feature_name = feature_names[w]
    
    linear_coefficients = icb_object$Prediction_Models$Linear$coef()[w]
    intercept = linear_coefficients[[1]][[1]]
    slope = linear_coefficients[[1]][[2]]
    spline_coefficients = icb_object$Prediction_Models$Spline$coef()[w]
    #mixed_coefficients = spline_coefficients + intercept + slope + 
    
    ## Extract the spline design matrix for the feature
    iteration <- icb_object$`Transition Iterations`[1]+1
    
    object <- icb_object$Prediction_Models$Spline
    
    
    if(names(object$coef())[w] == paste("bbs(",feature_name,", df = dfbase)",sep="")){
      basis <- extract(object,"design")[[w]]
      knots <- attr(basis,"knots")
      } else{
      iteration <- iteration + 1
    }
    
    
    
    prod = basis %*% spline_coefficients[[1]]
                 
    
    pr = prod
    #pr = data_temp * rescaled_linear_coefficients + prod
    if (is.null(ylim)) ylim <- range(pr, na.rm = TRUE)
    
    if(!is.factor(data_temp[,1])){
      plot(sort(data_temp[,1]), pr[order(data_temp[,1]),1],type="b",
           ylab="",xlab=colnames(data_temp)[1], ylim = ylim)
    }
    
  }
  
  
}