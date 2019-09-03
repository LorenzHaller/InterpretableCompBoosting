## Check out mboost code for PDPs


xVals <- unique(sort(data[[1]]))
xValsN <- as.numeric(xVals)
## make sure that we get the same number of values as in
## x; this is only a problem if pr is equal for
## different x values.
yVals <- unique(cbind(pr[order(data[[1]], na.last = NA)],
                      sort(data[[1]])))[, 1]

if (length(pr) == 1 && pr == 0) {
  yVals <- rep(0, length(xVals))
  
plot(xValsN, yVals,
       type = "n", xaxt = "n",
       xlim = range(as.numeric(xVals)) + c(-0.5, 0.5),
       xlab = xl, ylab = yl, ylim = ylim)
  ## make sure that xaxt / axes is respected
  print.xaxis <- TRUE
  
  
  if (print.xaxis)
    axis(1, at = xValsN, labels = levels(xVals))
  for (i in 1:length(xVals)) {
    lines(x = rep(xValsN[i], 2) + c(-0.35, 0.35),
          y = rep(yVals[i], 2), ...)
    
    
    
    plot(sort(data[[1]]), pr[order(data[[1]], na.last = NA)], type = type,
         xlab = xl, ylab = yl, ylim = ylim, ...)
    
    
    
plot.glmboost <- function(x, main = deparse(x$call), col = NULL,
                          off2int = FALSE, ...) {
  
  cp <- coef(x, aggregate = "cumsum", off2int = off2int)
  ncp <- names(cp)
  cp <- matrix(unlist(cp), nrow = length(cp), byrow = TRUE)
  cf <- cp[, ncol(cp)]
  if (is.null(col))
    col <- hcl(h = 40, l = 50, c= abs(cf) / max(abs(cf)) * 490)
  matplot(t(cp), type = "l", lty = 1, xlab = "Number of boosting iterations",
          ylab = "Coefficients", main = main, col = col, ...)
  abline(h = 0, lty = 1, col = "lightgray")
  axis(4, at = cf, labels = ncp, las = 1)
} 

    
##############
    
# x: mboost object

test_x <- test[,-1]
test_x <- model.frame(test_x)
    data <- test_x
newdata = test[,-1]
x<-mboost_bols_bs
which <- mboost_bols_bs$which(NULL,usedonly = is.null(which))
userspec.xlab <- FALSE
xlab <- variable.names(mboost_bols_bs)

w <- 1

for(w in which){
  # Extract the original data from the model
  data <- model.frame(x, which = w)[[1]]
  # Oder verwende newdata
  if (!is.null(newdata)) {
    data <- newdata[colnames(data)]
    if (is.list(data))
      data <- as.data.frame(data)
  }
  # 
  plot_helper <- function(xl, yl){
    # Erstelle predictions die nur einen baselearner (also ein Feature) aus dem Modell verwenden
    pr <- predict(x, newdata = data, which = w)
    # Lege einen range für y fest basierend auf den predictions
    if (is.null(ylim)) ylim <- range(pr, na.rm = TRUE)
    # Für mehr als zwei Features:
    if (ncol(data) > 2) {
      for (v in colnames(data)) {
        tmp <- data
        pardep <- sapply(data[[v]], function(vv) {
          tmp[[v]] <- vv
          mean(predict(mboost_bols_bs, newdata = tmp, which = w))
        })
        if (!userspec.xlab)
          xl <- v
        plot(sort(data[[v]]), pardep[order(data[[v]], na.last = NA)],ylim=ylim)
      }
    }
  }
}