# Airquality Data Example

setwd("C:/Users/halle/Downloads/Uni/Interpretable Machine Learning")

# Load data

data("airquality")
attach(airquality)
data <- na.omit(airquality)


# Get model formula and prepare data

formula <- Ozone ~ Solar.R + Wind + Temp + Month + Day
formula <- terms.formula(formula)
X <- model.matrix(formula, data)
y <- data[, as.character(formula)[2]]



linear_model_boost <- function(y=y, X=X, nu=0.1, mstop=100000, family=Gaussian()){
  
  # Load Gradient- and Loss/Riskfunction
  
  source("family.R")
  ngradient <- family@ngradient
  riskfct <- family@risk
  
  # Standardize features
  
  # if all features are numeric
  X_scaled <- X
  X_scaled[,2:dim(X)[2]] <- scale(X)[,2:dim(X)[2]]
  
  # only numeric features
  # numeric_features = c()
  # X <- X %>% mutate_each_(funs(scale(.) %>% as.vector), 
  #                             vars=numeric_features)
  

  # Initialize with Intercept model (similar to family@offset(y))
  
  fit_0 <- numeric(dim(X_scaled)[1])
  intercept_model <- lm.fit(x=as.matrix(X_scaled[,1]), y=y)
  fit_0 <- intercept_model$fitted.values
  
  # init_zero <- rep(0, dim(X)[1])
  # init_zero
  
  fitted_values <- fit_0
  
  # Specify set of base learners: For now just lm
  
  
  # Set up vectors
  
  lm_fit = numeric(dim(X_scaled)[2])
  lm_coeffs = numeric(dim(X_scaled)[2])
  pred_matrix = matrix(0, nrow = dim(X_scaled)[1], ncol = dim(X_scaled)[2])
  names(lm_coeffs) <- names(lm_fit) <- colnames(X_scaled)
  lm_coeffs[1] <- intercept_model$coefficients[1]
  
  
  for(i in 1:mstop){
    # Calculate the negative gradient 
  
    u <- ngradient(y = y, f = fitted_values)
    
    risk <- riskfct(y = y, f = fitted_values)
    
    # Fit base learners to the negative gradient
    
    lm_coeffs_temp = numeric(dim(X_scaled)[2])
    
    for(feat in 1:dim(X)[2]){
      bl_model <- lm.fit(x=as.matrix(X_scaled[,feat]), y=u)
      lm_fit[feat] <- riskfct(y=u, f=bl_model$fitted.values)
      pred_matrix[,feat] <- bl_model$fitted.values
      lm_coeffs_temp[feat] <- bl_model$coefficients
    }
    
    # Choose model with smallest loss
    model_select <- which.min(lm_fit)
    
    # if(lm_coeffs_temp[model_select] < 0.05){
    #   model_select <- order(lm_fit)[2]
    # }
      
    # Create new fitted values by model
    best_model_fit <- lm.fit(x=as.matrix(X_scaled[,model_select]), y=u)$fitted.values
    
    # Update model parameters
    lm_coeffs[model_select] <- lm_coeffs[model_select] + nu * lm_coeffs_temp[model_select]
    fitted_values <- X_scaled %*% lm_coeffs
    
  }
  
  # Print the coefficients of the final model
  print(lm_coeffs)
}

# Execute function
linear_model_boost(y=y, X=X, nu=0.1, mstop=1000, family=Gaussian())

# Compare to other methods
lm.fit(x=X_scaled, y=y)$coefficients
mboost::mboost(formula = formula, data = data, baselearner = "bols")

mboost_bols_bs = mboost::mboost(formula = formula, data = data, baselearner = c("btree"),
                           control = boost_control(nu = 0.1, mstop = 500))
mboost_bols_bs$risk()

str(mboost_bs)
mboost_bs$baselearner$`bbs(Wind)`
mboost_bs$basemodel$`bbs(Wind)`
mboost_bs$coef()


riskfct(y=y,f=mboost_bs$fitted())
riskfct(y=y,f=fitted_values)
