# Analyze mnist dataset   
library(keras)

rm(list=ls())
setwd('/media/umair/F00E03AB0E0369C4/projects/Arik')
# setwd('C:/Users/Or Zuk/Google Drive/HUJI/Teaching/Project_52500/2017_18/Code/Ex1/')
source('MNIST_general_functions.r')


mnist <- dataset_mnist()   # Load mnist dataset (keras)
x_train <- array_reshape(mnist$train$x, c(nrow(mnist$train$x), 784)) # reshape 
x_test <- array_reshape(mnist$test$x, c(nrow(mnist$test$x), 784))
y_train <- mnist$train$y
y_test <- mnist$test$y
z_map <- t(rbind(c(0:9), c(48:57))) # map digits to Asci values

# rescale
x_train <- x_train / 255 # keep only 0-1. Lose gray-scale information
x_test <- x_test / 255

out_name <- paste('MNIST_Results', Sys.Date(), 'txt', sep='.')

# Go over all solutions in directory
solution_files <- list.files(pattern = "^[C]")
n_sol <- length(solution_files)
time_train <- rep(0, n_sol)
accuracy_test <- rep(0, n_sol)
classifier_names <- rep('', n_sol)
ID_vec <- myls <- vector("list", length = n_sol)
for(i in 1:n_sol)
{
  source(solution_files[i])
  ID_vec[[i]] <- ID; rm(ID)
  classifier_names[i] <- strsplit(strsplit( solution_files[i], '_')[[1]][3], '\\.')[[1]][1]
  print(paste('Run', as.character(i)))
  start_time <- proc.time()
  m <- TrainModel(x_train, y_train)
  time_train[i] <- round(proc.time() - start_time, 2)[3]
  y_hat <- PredictModel(x_test, m)
  accuracy_test[i] <- round(100*EvaluateModel(y_test, y_hat), 2)
}

# Save running time and accuracy of different classifiers to file
sink(out_name) 
print(cbind(classifier_names, accuracy_test, time_train))
sink()
  
DisplayImage(x_train, y_train, z_map, 1) # Example: Show one image 
DisplayErrorImages(x_test, y_test, y_hat, z_map, 8) # Example: show 8 images with wrong classification 
