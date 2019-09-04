pdp_function <- function(icb_object, newdata = NULL){
  
  data = icb_object$Data[,-1]
  feature_names = colnames(data)
  feature_means = icb_object$`Feature Statistics`$Means[-1]
  feature_stdevs = icb_object$`Feature Statistics`$StdDevs[-1]
  
  which = as.integer(seq(1,dim(data)[2],by=1))
  
  icb_object$Coefficients
  
  for(w in which){
    
    data_temp = data[,w,drop=FALSE]
    feature_name = feature_names[w]
    
    linear_coefficients = icb_object$Coefficients$Linear_coefficients[w+1]
    rescaled_linear_coefficients = linear_coefficients / feature_stdevs[w]
    spline_coefficients = icb_object$Coefficients[w+2]
    
    
    
    ## Extract the spline design matrix for the feature
    design_matrix_found <- FALSE
    iteration <- icb_object$`Transition Iterations`[1]+1
    while(design_matrix_found == FALSE){
      object <- icb_object$Prediction_Models[[iteration]]
      if(names(object$coef()) == paste("bbs(",feature_name,", df = dfbase)",sep="")){
        basis <- extract(icb_object$Prediction_Models[iteration][[1]],"design")[[1]]
        knots <- attr(basis,"knots")
        design_matrix_found <- TRUE
      } else{
        iteration <- iteration + 1
      }
    }
    
    
    
    
    prod = basis %*% mixed_coefficients
  }
  
  
  
  
  
  
  
  
  
}