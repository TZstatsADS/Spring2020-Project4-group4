RMSE <- function(rating, est_rating){
  sqr_err <- function(obs){
    sqr_error <- (obs[3] - est_rating[as.character(obs[1]), as.character(obs[2])])^2
    return(sqr_error)
  }
  return(sqrt(mean(apply(rating, 1, sqr_err))))  
}

minFunc <- function(rating, matSolv, lambda){
  solve(matSolv %*% t(matSolv) + lambda * diag(f)) %*% matSolv %*% rating
}

# The Function used to Find the 
findSolve <- function(id, solveBy, train, lambda){
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