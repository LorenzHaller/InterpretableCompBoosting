

# Load data
# Specify family (type of target variable)



my_comp_boosting = function(formula, data, family, stop, nu, base_learners){
  
  # Check and prepare the data, bring into right format
  # Extract gradient and risk functions 
  

  # Define all base learners (also use input for base learners)
  # Define different sets of base learners
  # Define a condition when to switch to the next set


  # Define the boosting function
    ### Define initial fit
    ### Define a base learner function: Combine every base learner with every feature
       # and check where the fit to the residuals is the best -> select that one
    ### Sa´tart with the first set of base learners
  
  # Boosting iterations: In each iteration ...
    ### Fit base learners to current residuals and return best one
    ### Add the fit (??) to the fit function
    ### Compute the new residuals (negative gradient) from y and the fit
    ### Evaluate the risk/loss for y and the fit and save value
    ### Check improvement and switch to the next phase of base learners if necessary
    ### Save the current model
  
  # Stop the iterations if there's no improvement any more in the last phase
    # of base learners
  # Return final model, fit etc.


}