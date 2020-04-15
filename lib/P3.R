
#installed.packages("remotes")
remotes::install_github("TimothyKBook/krr")
#installed.packages("krr")
library(krr)
library(dplyr)
library(caret)

## Function to Normalize Each Row
norm.row <- function (m) {
  std <- function (vec){
    return (vec/sqrt(sum(vec^2)))
  }
  return (t(apply(m, 1,std)))
}

## Kernel Ridge Regression
## a function returns a list containing estimated rating matrix, training and testing RMSEs.

KRR.Post <- function (lambda = 10, data, train, test) {
  U=data$userId%>%unique()%>%length
  I=data$movieId%>%unique()%>%length
  
  ## Identify Movie Matrix (X), Normalized Movie Matrix (norm.X), and ratings (r) for each user, save in lists
  X <- vector(mode = "list", length = U)
  norm.X <- vector(mode = "list", length = U)
  r <- vector(mode = "list", length = U)
  
  for (u in 1:U) {
    ## get movie numbers rated by user u
    i.rated.by.u <- as.character(train[train$userId==u,]$movieId)
    
    X[[u]] <- result$Movie[,i.rated.by.u]
    norm.X[[u]] <- norm.row(t(X[[u]]))
    norm.X[[u]][is.na(norm.X[[u]])] <- 0
    r[[u]] <- train[train$userId==u,]$rating
  }
  
  ## save krr model for each user
  model <- vector(mode = "list", length = U)
  
  for (u in 1:U) {
    model[[u]] <- krr(norm.X[[u]],r[[u]], lambda = lambda)
  }
  
  ## get estimating matrix
  est_rating <- matrix(NA, ncol = I, nrow=U)
  colnames(est_rating) <- levels(as.factor(data$movieId))
  rownames(est_rating) <- levels(as.factor(data$userId))
  
  for (u in 1:U) {
    est_rating[u,] <- predict(model[[u]], norm.row(t(result$Movie))) 
    est_rating[u,][is.na(est_rating[u, ])] <- 0
    est_rating[u,] <- est_rating[u,]
  }
  
  # Summerize
  train_RMSE <- RMSE(train, est_rating)
  cat("training RMSE:", train_RMSE, "\t")
  
  
  test_RMSE <- RMSE(test, est_rating)
  cat("test RMSE:",test_RMSE, "\n")
  
  return(list(krr.rating=est_rating, train_RMSE = train_RMSE, test_RMSE = test_RMSE))
  
}




krr.cv <- function(dat_train, K.fold, lambda){
  ### Input:
  ### - train data frame
  ### - K.fold: a number stands for K-fold CV
  ### - tuning parameters 
  
  n <- dim(dat_train)[1]
  n.fold <- round(n/K.fold, 0)
  set.seed(0)
  s <- sample(rep(1:K.fold, c(rep(n.fold, K.fold-1), n-(K.fold-1)*n.fold)))  
  cv.train.error <- rep(NA, K.fold)
  cv.test.error <- rep(NA, K.fold)
  
  for (i in 1:K.fold){
    train.data <- dat_train[s != i,]
    test.data <- dat_train[s == i,]
    
    krr.result <- KRR.Post(lambda = lambda, data = dat_train, train = train.data, test = test.data)
    
    cv.train.error[i] <- krr.result$train_RMSE
    cv.test.error[i] <- krr.result$test_RMSE
    
  }			
  return(c(mean_train_rmse = mean(cv.train.error), mean_test_rmse = mean(cv.test.error),
           sd_train_rmse = sd(cv.train.error), sd_test_rmse = sd(cv.test.error)))
}