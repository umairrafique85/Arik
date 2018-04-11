# General helper functions used for mnist data analysis
asc <- function(x) { strtoi(charToRaw(x),16L) } # character to ascii code
asci <- function(n) { rawToChar(as.raw(n)) } # ascii code to charcater 

# Show one image with label
DisplayImage <- function(X, y, z_map, i)
{
  if(dim(z_map)[1]==10) # digits
  {
    I <- array_reshape((X[i,]), c(28, 28), order = "C")
    I <- t(I[c(28:1),])
  }
  else # transposed data 
  {
    I <- array_reshape((rev((X[i,]))), c(28, 28), order = "C")
    I <- I[c(28:1),]
  }
  image(I, # c("C", "F"))), 
        col = grey(seq(1, 0, length = 256)), 
        useRaster=TRUE, axes=FALSE, main= asci(z_map[y[i]+1,2]))
  return(0)
}

# Display random k images with wrong classification. 
# Can be useful when understanding classification errors 
# Display random k images with wrong classification. 
# Can be useful when understanding classification errors 
DisplayErrorImages <- function(X, y, y_hat, z_mat, k) 
{
  if(k<0)
  {
    I <- which(y == y_hat)
    k <- abs(k)
  }
  else
  {
    I <- which(y != y_hat)
  }
  
  n <- length(I)
  I <- I[sample(c(1:n), k, replace = FALSE, prob = NULL)]
  k_row <- floor(sqrt(k))
  k_col <- ceiling(k / k_row)
  attach(mtcars)
  par(mfrow=c(k_row,k_col), mar=c(1,1,1,1))
  ctr <- 1 
  for(i in 1:k_row)
  {
    for(j in 1:k_col)
    {
      print(ctr)
      if(ctr <= k)
      {
        DisplayImage(X[I,], y_hat[I], z_map, ctr)
      }
      ctr <- ctr+1
    }
  }  
}


# Compute prediction accuracy
EvaluateModel <- function(y, y_hat)  
{
  accuracy <- mean(y == y_hat)
}  
  
# Show time and accuracy of classifiers   
PlotClassifiersResults <- function(time, accuracy, labels, output_file = NULL)
{
  while (!is.null(dev.list()))  dev.off()
  if(!is.null(output_file))
  {
    jpeg(output_file);
  }
  plot(time, accuracy, xlab='Run Time (sec.)', ylab="Accuracy (%)", xlim=c(0, max(time)), ylim=c(min(accuracy)-1, 100))
  text(time, accuracy, labels=labels, cex= 0.9, pos=1)
  if(!is.null(output_file))
  {
    dev.off()
  }
}


# Nextfunctions for loading MNIST data is from: https://gist.github.com/brendano/39760 (MIT License)
# Load the MNIST digit recognition dataset into R
# http://yann.lecun.com/exdb/mnist/
# assume you have all 4 files and gunzip'd them in the ../Data/ directory (relative to src path)
# creates train$n, train$x, train$y  and test$n, test$x, test$y
# e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
# call:  show_digit(train$x[5,])   to see a digit.
# brendan o'connor - gist.github.com/39760 - anyall.org
#
load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    return(ret)
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    return(y)
  }
  mnist <- c()
  mnist$train <- c()
  mnist$test <- c()
  mnist$train$x <- load_image_file('../Data/MNIST/train-images-idx3-ubyte')
  mnist$test$x <- load_image_file('../Data/MNIST/t10k-images-idx3-ubyte')
  mnist$train$y <- load_label_file('../Data/MNIST/train-labels-idx1-ubyte')
  mnist$test$y <- load_label_file('../Data/MNIST/t10k-labels-idx1-ubyte')  
  return(mnist)
}