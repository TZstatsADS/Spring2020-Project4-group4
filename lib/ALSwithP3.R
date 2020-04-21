# Solve RMSE
RMSE <- function(rating, est_rating){
  sqr_err <- function(obs){
    sqr_error <- (obs[3] - est_rating[as.character(obs[1]), as.character(obs[2])])^2
    return(sqr_error)
  }
  return(sqrt(mean(apply(rating, 1, sqr_err))))  
}

minFunc <- function(rating, matSolv, lambda){
  set.seed(1)
  solve(matSolv %*% t(matSolv) + lambda * diag(f)) %*% matSolv %*% rating
}

# The Function used to Find the 
findSolve <- function(id, solveBy, train, lambda){
  set.seed(1)
  id <- as.integer(id)
  # Fix Movies, solve User
  if(solveBy=="Movies"){
    movId <- train[train$userId==id, ]$movieId
    movSolv <- Movies[, as.character(movId)]
    rating <- train[train$userId==id, ]$rating
    
    minFunc(rating = rating, matSolv = movSolv, lambda = lambda)
    
  }
  # Fix User, solve Movie
  else if(solveBy=="Users"){
    userId <- train[train$movieId==id, ]$userId
    userSolv <- Users[, as.character(userId)]
    rating <- train[train$movieId==id, ]$rating
    
    minFunc(rating = rating, matSolv = userSolv, lambda = lambda)
  }
  else return("Please let matSolv be in right way")
}


ALS <- function(data, train, test, f, maxIters, lambda=5){
  # Factorized the Movies and User matrices
  set.seed(1)

  UserId <- unique(data$userId)
  U <- length(UserId)
  
  MovieId <- unique(data$movieId)
  M <- length(MovieId)
  
  avgRatingByUser <- data %>% 
    group_by(userId) %>% 
    summarise(avgRating = mean(rating))
  
  avgRatingByMovie <- data %>% 
    group_by(movieId) %>% 
    summarise(avgRating = mean(rating))
  
  Users <- matrix(c(avgRatingByUser$avgRating, runif((f-1)*U, -1, 1)), nrow=f, byrow = T)
  colnames(Users) <- UserId
  
  Movies <- matrix(c(avgRatingByMovie$avgRating, rnorm((f-1)*M, -1, 1)), nrow=f, byrow = T)
  colnames(Movies) <- MovieId
  
  clusterExport(cl, "minFunc", envir = environment())
  clusterExport(cl, "f", envir = environment())
  clusterExport(cl, "UserId", envir = environment())
  clusterExport(cl, "MovieId", envir = environment())
  
  trainRMSE <- rep(NA, maxIters%/%3)
  testRMSE <- rep(NA, maxIters%/%3)
  
  iter <- 1
  while(iter <= maxIters){
    st <- Sys.time()
    # Fix Movie, solve User
    clusterExport(cl, "Movies", envir = environment())
    clusterExport(cl, "Users", envir = environment())
    Users <- parSapply(cl, as.character(UserId), findSolve, solveBy="Movies", train = train, lambda = lambda, USE.NAMES = T)
    # Fix User, solve Movie
    
    clusterExport(cl, "Movies", envir = environment())
    clusterExport(cl, "Users", envir = environment())
    Movies <- parSapply(cl, as.character(MovieId), findSolve, solveBy="Users", train = train, lambda = lambda, USE.NAMES = T)
    # cat("Iter:", iter,  "\t Time spent:", round(Sys.time()-st, 3), "s\n")
    
    # if(iter%%3==1){
    #   est_rating <- t(Users) %*% Movies
    #   
    #   trainRMSE[iter%/%3+1] <- RMSE(train, est_rating)
    #   testRMSE[iter%/%3+1] <- RMSE(test, est_rating)
    # }
    
    cat(".")
    if(iter==maxIters) cat("\n")
    iter <- iter + 1
  }
  # RMSE
  est_rating <- t(Users) %*% Movies
  trainRMSE <- RMSE(train, est_rating)
  testRMSE <- RMSE(test, est_rating)
  
  
  
  return(list("User" = Users, 
              "Movie" = Movies, 
              "Rating" = est_rating, 
              "TrainRMSE" = trainRMSE, 
              "TestRMSE" = testRMSE))
}


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
  set.seed(1)
  
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

KRR.Post <- function (result_ALS, lambda = 10,sigma=1.5, data, train, test) {
  U=data$userId%>%unique()%>%length
  I=data$movieId%>%unique()%>%length
  
  ## Identify Movie Matrix (X), Normalized Movie Matrix (norm.X), and ratings (r) for each user, save in lists
  
  ## get estimating matrix
  est_rating <- matrix(NA, ncol = I, nrow=U)
  colnames(est_rating) <- levels(as.factor(data$movieId))
  rownames(est_rating) <- levels(as.factor(data$userId))
  
  X_full <- result_ALS$Movie
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
  colnames(est_rating)<-result_ALS$Rating%>%colnames
  # Summerize
  train_RMSE <- RMSE(train, est_rating)
  cat("training RMSE:", train_RMSE, "\t")
  
  
  test_RMSE <- RMSE(test, est_rating)
  cat("test RMSE:",test_RMSE, "\n")
  
  return(list(krr.rating=est_rating, train_RMSE = train_RMSE, test_RMSE = test_RMSE))
  
}

# Func of ALS with P3
ALS_KRR <- function(data, train, test, f, maxIters, lambda_als, lambda_p, sigma){
  set.seed(1)
  
  # data=dat_train
  # train=train.data
  # test=test.data
  # f=f
  # maxIters=maxIter
  # lambda=lambdas_als
  # lambda_p=lambdas_p
  # sigma=sigmas
  result_ALS <- ALS(data, train, test, f, maxIters, lambda_als)
  KRR.Post(result_ALS=result_ALS,lambda=lambda_p, sigma=sigma, data, train, test)
}


KRR.Post_R <- function (result_ALS, lambda = 10,sigma=1.5, data, train, test) {
  U=data$userId%>%unique()%>%length
  I=data$movieId%>%unique()%>%length
  
  ## Identify Movie Matrix (X), Normalized Movie Matrix (norm.X), and ratings (r) for each user, save in lists
  
  ## get estimating matrix
  est_rating <- matrix(NA, ncol = I, nrow=U)
  colnames(est_rating) <- levels(as.factor(data$movieId))
  rownames(est_rating) <- levels(as.factor(data$userId))
  
  X_full <- result_ALS$Movie
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
  colnames(est_rating)<-result_ALS$Rating%>%colnames
  # Summerize
  train_RMSE <- RMSE_R(train, est_rating)
  cat("training RMSE:", train_RMSE, "\t")
  
  
  test_RMSE <- RMSE_R(test, est_rating)
  cat("test RMSE:",test_RMSE, "\n")
  
  return(list(krr.rating=est_rating, train_RMSE = train_RMSE, test_RMSE = test_RMSE))
  
}
