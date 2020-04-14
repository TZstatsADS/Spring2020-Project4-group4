########################
### Cross Validation ###
########################

### Author: Chengliang Tang
### Project 3

cv.functionYQ <- function(dat_train, K, f, lambda, maxIter){
  ### Input:
  ### - train data frame
  ### - K: a number stands for K-fold CV
  ### - tuning parameters 
  
  n <- dim(dat_train)[1]
  n.fold <- round(n/K, 0)
  set.seed(0)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  train_rmse <- matrix(NA, ncol = (maxIter%/%3), nrow = K)
  test_rmse <- matrix(NA, ncol = (maxIter%/%3), nrow = K)
  
  for (i in 1:K){
    train.data <- dat_train[s != i,]
    test.data <- dat_train[s == i,]
    
    result <- ALS(data, train, test, f = f, maxIters = maxIter, lambda = lambda)
  
    train_rmse[i,] <-  result$TrainRMSE
    test_rmse[i,] <-   result$TestRMSE
    
  }		
  return(list(mean_train_rmse = apply(train_rmse, 2, mean), mean_test_rmse = apply(test_rmse, 2, mean),
         sd_train_rmse = apply(train_rmse, 2, sd), sd_test_rmse = apply(test_rmse, 2, sd)))
}