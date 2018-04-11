ID <- c(12345, 678910) # write students ID 

# Functions used for mnist data training and testing

# Train Model
TrainModel <- function(X, y)
{
  n <- length(y) # get number of examples   
  model <- c()
  lab <- sort(unique(y)) # get all labels 
  model$w <- matrix(0, length(lab), 1) # set weights (parameters)
  
  for(i in 1:length(lab))
  {
    model$w[i] <- mean( X[which(y == lab[i]),]  )
  }    
  return(model)
}
  
# predict labels for test set using learned parameters w   
PredictModel <- function(X, model)
{
  n <- dim(X)[1] # get number of examples   
  y_hat <- rep(0, n)
  # example: predict by average darkness   
  for(i in 1:n)
  {
    y_hat[i] <- which.min(abs(model$w - mean(X[i,])))-1    
  }
  return(y_hat)
}