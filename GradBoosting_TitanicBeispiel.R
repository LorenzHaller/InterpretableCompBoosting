setwd("C:/Users/halle/Downloads/Uni/Interpretable Machine Learning")

# Laden der Daten

library(titanic)
data("titanic_train")
data("titanic_test")
data_train = titanic_train
data_validation = titanic_test

data <- as.data.frame(data_train)
data <- na.omit(data)

# Define hyper parameters

nu <- 0.05
stop <- 50


# Gradient- und Loss/Riskfunktion

source("family.R")
family = Binomial()
ngradient <- family@ngradient
riskfct <- family@risk
family@charloss

# Define own loss function
loss_binomial = function(y, f){
  f <- pmin(abs(f), 36) * sign(f)
  p <- exp(f)/(exp(f) + exp(-f))     
  y <- (y + 1)/2                    
  -y * log(p) - (1 - y) * log(1 - p) 
}



# Define the set of base learners (not needed at the moment)

# base_learners = ("lm")
# model_list <- vector(mode = "list", length = stop)


# Modellformel und Aufbereitung Daten

formula <- Survived ~ Pclass + Sex + Age + Fare
formula <- terms.formula(formula)

X <- model.matrix(formula, data)

y <- data[, as.character(formula)[2]]

# Initialisierung

#Initial fit can be set to zero
#yhat.init = 0
#fit <- yhat.init

# Take intercept model as initial fit
intercept_model <- lm.fit(x=as.matrix(X[,1]), y=y)
fit_0 <- intercept_model$fitted.values
f0 <- intercept_model$coefficients[[1]]
fit <- fit_0
  

u <- ngradient(y = y, f = fit_0)
u <- replace(u, u>1, 1)

theta <- rep(0, ncol(X))
theta[1] <- f0

# track loss
risk <- c()
#loss <- c()


for (iter in 1:stop) {
  
  # Fit base learner on the pseudo-residuals / gradient
  #base_prod <- glm.fit(x = X, y = u, family = binomial(link = "logit"))
  
  
  # Is the condition really necessary? --> first phase
  if ("lm" %in% base_learners){
    
    lm_fit = numeric(dim(X)[2])
    lm_coeffs = numeric(dim(X)[2])
    names(lm_coeffs) <- names(lm_fit) <- colnames(X)
    
    for(feat in 1:dim(X)[2]){
      bl_model <- lm.fit(x=as.matrix(X[,feat]), y=u)
      lm_fit[feat] <- riskfct(y=u, f=bl_model$residuals)
      lm_coeffs[feat] <- bl_model$coefficients
    }
    
    model_select <- which.min(lm_fit)
    best_model_fit <- lm.fit(x=as.matrix(X[,model_select]), y=u)$fitted.values
    
    
    best_fit <- lm_fit[model_select]
    best_model <- lm_coeffs[model_select]
    best_feature <- colnames(X)[model_select]
    best_output <- c(best_feature, best_model,best_fit)
  }
  
  model_list[[iter]] <- best_output
  
  #Extract coefficients
  theta_iter <- rep(0, ncol(X))
  theta_iter[model_select] <- best_model
  
  # update our theta
  theta <- theta + nu*as.vector(theta_iter)
  
  # new fitted values
  fit <- fit + nu * best_model_fit
  
  # update gradient
  u <- ngradient(y = y, f = fit)
  
  #u <- replace(u, u>1, 1)
  
  # update loss 
  risk <- append(risk, riskfct(y = y, f = fit))
  # loss <- append(loss, loss_binomial(y = y, f = fit))

}

risk
model_list


# 
# glm.fit(x=X,y=y,binomial(link = "logit"))
