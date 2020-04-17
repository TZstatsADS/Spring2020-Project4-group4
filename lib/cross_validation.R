########################
### Cross Validation ###
########################


cv.functionYQ <- function(dat_train, K, f, maxIter,lambdas_als, lambdas_p,sigmas){
  ### Input:   
  ### - train data frame
  ### - K: a number stands for K-fold CV
  ### - tuning parameters 
  
  n <- dim(dat_train)[1]
  n.fold <- round(n/K, 0)
  set.seed(0)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  # train_rmse <- matrix(NA, ncol = (maxIter), nrow = K)
  # test_rmse <- matrix(NA, ncol = (maxIter), nrow = K)
  train_rmse <- NULL
  test_rmse <- NULL
  
  for (i in 1:K){
    train.data <- dat_train[s != i,]
    test.data <- dat_train[s == i,]
    
    # result <- ALS(data, train, test, f = f, maxIters = maxIter, lambda = lambda)
    result <- ALS_KRR(dat_train,train.data,test.data,f=f,maxIters =maxIter,lambdas_als,lambdas_p, sigmas)
    # 
    # train_rmse[i,] <-  result$TrainRMSE
    # test_rmse[i,] <-   result$TestRMSE
    train_rmse <- c(train_rmse, result$train_RMSE)
    test_rmse <-  c(test_rmse, result$test_RMSE)
    
  }		
  return(list(mean_train_rmse = mean(train_rmse), mean_test_rmse = mean(test_rmse),
         sd_train_rmse = sd(train_rmse), sd_test_rmse = sd(test_rmse)))
}



cv.functionA3R3P3 <- function(dat_train, K, f, maxIter,lambdas_als, lambdas_p,sigmas,betas){
  ### Input:   
  ### - train data frame
  ### - K: a number stands for K-fold CV
  ### - tuning parameters 
  
    n <- dim(dat_train)[1]
  n.fold <- round(n/K, 0)
  set.seed(0)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  # train_rmse <- matrix(NA, ncol = (maxIter), nrow = K)
  # test_rmse <- matrix(NA, ncol = (maxIter), nrow = K)
  train_rmse <- NULL
  test_rmse <- NULL
  
  for (i in 1:K){
    train.data <- dat_train[s != i,]
    test.data <- dat_train[s == i,]
    
    
    
    
    # result <- ALS(data, train, test, f = f, maxIters = maxIter, lambda = lambda)
    result <- ALS_R_KRR(data=dat_train,train=train.data,test=test.data,f=f,maxIters =maxIter,
                        lambda_als = lambdas_als,lambda_p = lambdas_p, sigma = sigmas,beta = betas)
    
    # data=dat_train
    # train=train.data
    # test=test.data
    # f=f
    # maxIters =maxIter
    # lambda_als = lambdas_als
    # lambda_p = lambdas_p
    # sigma = sigmas
    # beta = betas
    
    
    
    
    # 
    # train_rmse[i,] <-  result$TrainRMSE
    # test_rmse[i,] <-   result$TestRMSE
    train_rmse <- c(train_rmse, result$train_RMSE)
    test_rmse <-  c(test_rmse, result$test_RMSE)
    
  }		
  return(list(mean_train_rmse = mean(train_rmse), mean_test_rmse = mean(test_rmse),
              sd_train_rmse = sd(train_rmse), sd_test_rmse = sd(test_rmse)))
}
