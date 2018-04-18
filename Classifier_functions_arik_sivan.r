ID <- 203667464

TrainModel <- function(X, y){
  # converting y to categorical classes
  y <- to_categorical(y)
  # initializing model with with two fully connected (dense) layers,
  # first layer having rectified linear unit as the activation function
  # which is basically max(x, 0). This is the most popular activation
  # function for deep neural networks. The second layer has softmax as
  # its activation function which returns an array of 10 probability
  # scores summing up to one. Each score will be the probability that the
  # current digit image belongs to one of the 10 digit classes in y.
  model <- keras_model_sequential() %>% 
    layer_dense(units = 512, activation = 'relu', input_shape = c(784)) %>% 
    layer_dense(units = 10, activation = 'softmax')
  
  # compiling the network with an optimizer and loss function. Since it
  # is a classification problem, we use categorical crossentropy as the loss
  # function. RMSprop is the optimizer function, i.e. it defines
  # the exact rules governing the use of gradient descent here to reduce the
  # cost.
  model %>% compile(optimizer = 'rmsprop', loss = 'categorical_crossentropy', metrics = c('accuracy'))
  model %>% fit(X, y, epochs = 5, batch_size = 128)
  
  # Once the model is compiled it is returned to be stored in the 'm'
  # variable defined in the MNIST_run.r script
  
  return(model)
}

PredictModel <- function(X, model){
  y <- model %>% predict_classes(X)
}
