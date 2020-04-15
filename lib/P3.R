
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
  return ((apply(m, 2,std)))
}

## Kernel Ridge Regression
## a function returns a list containing estimated rating matrix, training and testing RMSEs.

rating_krr<-function(u){
  u=as.numeric(u)
  norm.X<-NULL
  r <-NULL
  ## get movie numbers rated by user u
  i.rated.by.u <- as.character(train[train$userId==u,]$movieId)
  norm.X=norm.X_full[i.rated.by.u,]
  r<- train[train$userId==u,]$rating
  model <- krr(norm.X,r, lambda = lambda,sigma=sigma)
  est_rating <- predict(model, norm.X_full) 
  est_rating[is.na(est_rating)] <- 0
  return(est_rating)
}

KRR.Post <- function (lambda = 10,sigma=1.5, data, train, test) {
  U=data$userId%>%unique()%>%length
  I=data$movieId%>%unique()%>%length
  
  ## Identify Movie Matrix (X), Normalized Movie Matrix (norm.X), and ratings (r) for each user, save in lists

  ## get estimating matrix
  est_rating <- matrix(NA, ncol = I, nrow=U)
  colnames(est_rating) <- levels(as.factor(data$movieId))
  rownames(est_rating) <- levels(as.factor(data$userId))
  
  X_full <- result$Movie
  norm.X_full <- t(norm.row(X_full))
  norm.X_full[is.na(norm.X_full)] <- 0
  

  
  cl <- makeCluster(4)
  
  clusterExport(cl, "train", envir = environment())
  clusterExport(cl, "norm.X_full", envir = environment())
  clusterExport(cl, "X_full", envir = environment())
  clusterExport(cl, "krr", envir = environment())
  clusterExport(cl, "lambda", envir = environment())
  clusterExport(cl, "sigma", envir = environment())
  est_rating=parSapply(cl, as.character(1:U),rating_krr, USE.NAMES = T)
  est_rating=t(est_rating)
  colnames(est_rating)<-resultALS$Rating%>%colnames
  # Summerize
  train_RMSE <- RMSE(train, est_rating)
  cat("training RMSE:", train_RMSE, "\t")
  
  
  test_RMSE <- RMSE(test, est_rating)
  cat("test RMSE:",test_RMSE, "\n")
  
  return(list(krr.rating=est_rating, train_RMSE = train_RMSE, test_RMSE = test_RMSE))
  
}




krr.cv <- function(dat_train, K.fold, lambda,sigma){
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
    
    krr.result <- KRR.Post(lambda = lambda,sigma=sigma, data = dat_train, train = train.data, test = test.data)
    
    cv.train.error[i] <- krr.result$train_RMSE
    cv.test.error[i] <- krr.result$test_RMSE
    
  }			
  return(c(mean_train_rmse = mean(cv.train.error), mean_test_rmse = mean(cv.test.error),
           sd_train_rmse = sd(cv.train.error), sd_test_rmse = sd(cv.test.error)))
}