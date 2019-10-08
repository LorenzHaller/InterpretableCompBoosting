pdp_function <- function(icb_object, newdata = NULL, ylim = NULL){
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  data = icb_object$Data[,-1]
  feature_names = colnames(data)
  
  method <- icb_object$Input_Parameters[[7]]
  
  which = as.integer(seq(1,dim(data)[2],by=1))
  
  
  for(w in which){
    
    ylim = NULL
    
    data_temp = data[,w,drop=FALSE]
    feature_name = feature_names[w]
    
    linear_coefficients = icb_object$Prediction_Models$Linear$coef()[w]
    
    if(!is.null(linear_coefficients[[1]])){
      intercept = linear_coefficients[[1]][[1]]
      slope = linear_coefficients[[1]][[2]]
    }
    
    spline_coefficients = icb_object$Prediction_Models$Spline$coef()[w]
    
    
    if(method == "bbs"){
      ## Extract the spline design matrix for the feature
      iteration <- icb_object$`Transition Iterations`[1]+1
      
      object <- icb_object$Prediction_Models$Spline
      basis <- extract(object,"design")[[w]]
      knots <- attr(basis,"knots")
      
      
      # if(names(object$coef())[w] == paste("bbs(",feature_name,", df = dfbase)",sep="")){
      #   basis <- extract(object,"design")[[w]]
      #   knots <- attr(basis,"knots")
      #   } else{
      #   iteration <- iteration + 1
      # }
      
      
      if(!is.null(linear_coefficients[[1]])){
        prod = basis %*% spline_coefficients[[1]] + intercept + slope * data_temp[,1]
        sum_linear = intercept + slope * data_temp[,1]
        sum_linear = as.matrix(sum_linear)
      } else{
        prod = basis %*% spline_coefficients[[1]]
      }            
      
      pr = prod
      #pr = data_temp * rescaled_linear_coefficients + prod
      if (is.null(ylim)) ylim <- range(pr, na.rm = TRUE)
      
      # if(!is.factor(data_temp[,1])){
      #   plot(sort(data_temp[,1]), pr[order(data_temp[,1]),1],type="b",
      #        ylab="",xlab=colnames(data_temp)[1], ylim = ylim)
      #   if(!is.null(linear_coefficients[[1]])){
      #     points(sort(data_temp[,1]), sum_linear[order(data_temp[,1]),1],type="l")
      #   }
      # }
    } else if(method == "btree"){
      
      if(!is.null(linear_coefficients[[1]])){
        pr = predict(icb_object$Prediction_Models$Spline, newdata = data, which = w) + intercept + slope * data_temp[,1]
        pr = as.matrix(pr)
        sum_linear = intercept + slope * data_temp[,1]
        sum_linear = as.matrix(sum_linear)
      } else{
        pr = as.matrix(predict(icb_object$Prediction_Models$Spline, newdata = data, which = w))
      }  
      
      # if(!is.factor(data_temp[,1])){
      #   plot(sort(data_temp[,1]), pr[order(data_temp[,1]),1],type="b",
      #        ylab="",xlab=colnames(data_temp)[1], ylim = ylim)
      #   if(!is.null(linear_coefficients[[1]])){
      #     points(sort(data_temp[,1]), sum_linear[order(data_temp[,1]),1],type="l")
      #   }
      # }
      
    }
    
    if(!is.factor(data_temp[,1])){
      if(!(pr[1] == 0)){
        plot(sort(data_temp[,1]), pr[order(data_temp[,1]),1],type="b",
             ylab="",xlab=colnames(data_temp)[1], ylim = ylim)
      }
      if(!is.null(linear_coefficients[[1]])){
        points(sort(data_temp[,1]), sum_linear[order(data_temp[,1]),1],type="l")
      }
    }
    
    if(is.factor(data_temp[,1])){
      
      xVals <- unique(sort(data_temp[,1]))
      xValsN <- as.numeric(xVals)
      ## make sure that we get the same number of values as in
      ## x; this is only a problem if pr is equal for
      ## different x values.
      yVals <- unique(cbind(pr[order(data_temp[,1], na.last = NA)],
                            sort(data_temp[,1])))[, 1]
      
      if (length(pr) == 1 && pr == 0) {
        yVals <- rep(0, length(xVals))
      }
      plot(xValsN, yVals,
           type = "p", xaxt = "n",
           xlim = range(as.numeric(xVals)) + c(-0.5, 0.5)
           ,xlab = colnames(data_temp)[1] 
           #,ylab = yl, ylim = ylim
           )
      ## make sure that xaxt / axes is respected
      print.xaxis <- TRUE
      # if (length(list(...)) >= 1 && any(xaxis <- c("xaxt", "axes") %in% names(list(...)))) {
      #   if (xaxis[1] && list(...)$xaxt == "n")
      #     print.xaxis <- FALSE
      #   if (xaxis[2] && list(...)$axes == FALSE)
      #     print.xaxis <- FALSE
      # }
      if (print.xaxis)
        axis(1, at = xValsN, labels = levels(xVals))
      for (i in 1:length(xVals)) {
        lines(x = rep(xValsN[i], 2) + c(-0.35, 0.35),
              y = rep(yVals[i], 2))
      }
      
    }
    
  }
  
  options(warn = oldw)
}