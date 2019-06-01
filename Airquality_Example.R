# Airquality Data Example

setwd("C:/Users/halle/Downloads/Uni/Interpretable Machine Learning")

# Load data

data("airquality")
attach(airquality)
data <- na.omit(airquality)

# Load Gradient- and Loss/Riskfunction

source("family.R")
family = Gaussian()
ngradient <- family@ngradient
riskfct <- family@risk

# Get model formula and prepare data

formula <- Ozone ~ Solar.R + Wind + Temp + Month + Day
formula <- terms.formula(formula)
X <- model.matrix(formula, data)
y <- data[, as.character(formula)[2]]

# Set parameters

nu <- 0.1


###########################################################################################

# Initialize with Intercept model

fit_0 <- numeric(dim(X)[1])
intercept_model <- lm.fit(x=as.matrix(X[,1]), y=y)
fit_0 <- intercept_model$fitted.values

init_zero <- rep(0, dim(X)[1])
init_zero

fitted_values <- fit_0

# Specify set of base learners: For now just lm


# Set up vectors

lm_fit = numeric(dim(X)[2])
lm_coeffs = numeric(dim(X)[2])
pred_matrix = matrix(0, nrow = dim(X)[1], ncol = dim(X)[2])
names(lm_coeffs) <- names(lm_fit) <- colnames(X)
lm_coeffs[1] <- intercept_model$coefficients[1]


for(i in 1:100000){
  # Calculate the negative gradient 

  u <- ngradient(y = y, f = fitted_values)
  
  
  # Fit base learners to the negative gradient
  
  lm_coeffs_temp = numeric(dim(X)[2])
  
  for(feat in 1:dim(X)[2]){
    bl_model <- lm.fit(x=as.matrix(X[,feat]), y=u)
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
  best_model_fit <- lm.fit(x=as.matrix(X[,model_select]), y=u)$fitted.values
  
  # Update model parameters
  lm_coeffs[model_select] <- lm_coeffs[model_select] + nu * lm_coeffs_temp[model_select]
  fitted_values <- X %*% lm_coeffs
  
  lm_fit
  lm_coeffs
}

lm_coeffs

