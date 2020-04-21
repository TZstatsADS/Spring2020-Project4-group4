######################################## RMSE ########################################
RMSE_R <- function(rating, est_rating){
  sqr_err <- function(id){
    sqr_error <- (as.numeric(rating[id,3]) - est_rating[as.character(rating[id,1]), as.character(rating[id,2])])^2
    return(sqr_error)
  }
  error = sapply(1:nrow(rating), sqr_err)
  return(sqrt(mean(error)))  
}


##################################### Matrix Iteration #####################################
############## solve function for new user or movies matrix in each iteration ##############
findSolve_R <- function(id, solveBy, train, lambda, user_mat, movie_mat, au, bu, bi, mu, f){
  set.seed(1)
  
  id <- as.integer(id)
  # Fix Movies, solve User
  if(solveBy == "Movies"){
    # find all the users who rate movie i
    U_set <- as.character(train[train$movieId == id,]$userId)
    dev <- train[train$movieId == id,]$dev
    user_mat_p = user_mat[,U_set]
    Result = solve(user_mat_p %*% t(user_mat_p) + lambda* diag(f+1)) %*% user_mat_p %*% (train[train$movieId == id,]$rating - mu - bu[,U_set] - au[,U_set]*dev)
    return(Result)
  }
  
  # Fix User, solve Movie
  else if(solveBy == "Users"){
    # find all the moives rated by user u
    M_set <- as.character(train[train$userId == id,]$movieId)
    x <- train[train$userId == id,]$rating
    dev <- train[train$userId == id,]$dev
    R <- matrix(x, ncol = length(x), nrow = 1)
    
    cname <- colnames(movie_mat[,M_set])
    movie_mat_p <- rbind(dev, movie_mat[,M_set])
    colnames(movie_mat_p) <- cname
    
    Result = solve(movie_mat_p %*% t(movie_mat_p) + lambda * diag(f+2)) %*% movie_mat_p %*% t(R - mu - bi[,M_set])
    return(Result)
  }
  else return("Please let matSolv be in right way")
}

######################################## ALS + R1R3 ########################################
############## Return user and movie matrix, ratings, training and test RMSE ##############
ALS.R1R3 <- function(f = 10, lambda = 5, beta = 0.4, maxIters = 5, data = data,
                     train = data_train, test = data_test){
  #
  # f: Dimension of the feature space
  # lambda: Regularization coefficient
  # beta: Dev coefficient
  # maxIters: Number of iterations
  # data: All data
  # train: Training data
  # test: Test data
  #
  set.seed(1)
  
  # Calculate the dev of data
  train = train %>% mutate(dev = ifelse(timediff>0, 1, -1)*abs(timediff)^beta)
  test = test %>% mutate(dev = ifelse(timediff>0, 1, -1)*abs(timediff)^beta)
  all_data = rbind(train, test)
  
  # Factorized the Movies and User matrices
  UserId <- levels(as.factor(data$userId))
  U <- length(UserId)
  
  MovieId <- levels(as.factor(data$movieId))
  M <- length(MovieId)
  
  # Calculate the average value of each user or each movie
  avgRatingByUser <- data %>% 
    group_by(userId) %>% 
    summarise(avgRating = mean(rating))
  
  avgRatingByMovie <- data %>% 
    group_by(movieId) %>% 
    summarise(avgRating = mean(rating))
  
  # Set initial values for users and movies
  Users <- matrix(c(avgRatingByUser$avgRating, runif((f-1)*U, -10, 10)), nrow=f,byrow = T)
  colnames(Users) <- UserId
  
  Movies <- matrix(c(avgRatingByMovie$avgRating, rnorm((f-1)*M, -10, 10)), nrow=f,byrow = T)
  colnames(Movies) <- MovieId
  
  # Initialize bu, bi, R, au
  bu <- matrix(rep(0, U), ncol = U)
  colnames(bu) <- UserId
  
  bi <- matrix(rep(0, M), ncol = M)
  colnames(bi) <- MovieId
  
  R <- matrix(rep(0, U*M), ncol = M)
  colnames(R) <- MovieId
  
  au <- matrix(rep(0, U), ncol = U)
  colnames(au) <- UserId
  
  # Calculate mean of all the ratings in train data set.
  Ave_rating <- mean(train$rating)
  trainRMSE <- rep(NA, maxIters)
  testRMSE <- rep(NA, maxIters)
  
  clusterExport(cl, "findSolve_R", envir = environment())
  clusterExport(cl, "f", envir = environment())
  clusterExport(cl, "UserId", envir = environment())
  clusterExport(cl, "MovieId", envir = environment())
  
  iter <- 1
  while(iter <= maxIters){
    S = Sys.time()
    
    # Expansion matrix for operation. However dev needs to be selected according to different users, so it shoule be added inside the function
    Movies_cal <- rbind(rep(1,M), Movies)
    colnames(Movies_cal) <- MovieId
    Users_cal <- rbind(au, bu, Users)
    
    # Fix Movie, solve User
    Users_cal <- parSapply(cl, as.character(UserId), findSolve_R, solveBy = "Users", train = train, lambda = lambda, user_mat = Users_cal, movie_mat = Movies_cal, au = au, bu = bu, bi = bi, mu = Ave_rating, f = f, USE.NAMES = T)
    
    # Update au, bu and Users
    au[1,] <- Users_cal[1, ]
    bu[1,] <- Users_cal[2, ]
    Users <- Users_cal[-c(1,2), ]
    
    # Expansion matrix for operation.
    Users_cal <- rbind(rep(1,U), Users)
    colnames(Users_cal) <- UserId
    Movies_cal <- rbind(bi, Movies)
    
    # Fix User, solve Movie
    Movies_cal <- parSapply(cl, as.character(MovieId), findSolve_R, solveBy = "Movies", train = train, lambda = lambda, user_mat = Users_cal, movie_mat = Movies_cal, au = au, bu = bu, bi = bi, mu = Ave_rating, f = f, USE.NAMES = T)
    
    # Update bi and Movies
    bi[1,] <- Movies_cal[1,]
    Movies <- Movies_cal[-1,]
    
    # Rating Matrix
    mat <- t(Users) %*% Movies
    
    # Represent bu, bi, au*dev as a matrix
    bu_ui <- matrix(rep(NA, U*M), ncol = M)
    for (i in 1:M)
      bu_ui[,i] <- t(bu)
    
    bi_ui <- matrix(rep(NA, U*M), ncol = M)
    for (u in 1:U)
      bi_ui[u, ] <- bi
    
    # Use tibble to speed up the tranformation of au*dev matrix.
    au_mat <- matrix(rep(au,M),nrow=U,byrow = F)
    dev_mat <- all_data %>%
      unique() %>%
      select(userId, movieId, dev) %>%
      arrange(userId, movieId) %>%
      pivot_wider(userId, names_from = movieId, values_from = dev, values_fill = list(dev = 0)) %>%
      select(-1)
    colnames(dev_mat) <- colnames(dev_mat) %>% as.numeric()
    dev_mat <- dev_mat %>% select(as.character(MovieId))
    au_dev_ui <- au_mat*dev_mat
    
    au_dev_ui <- as.matrix(au_dev_ui)
    
    mu_ui <- matrix(rep(Ave_rating, U*M), ncol = M)
    
    # Calculate the rating matrix
    R <- mat + mu_ui + bu_ui + bi_ui + au_dev_ui
    
    # Calculate the RMSE 
    est_rating <- as.matrix(R)
    colnames(est_rating)<-MovieId
    rownames(est_rating)<-UserId
    train_mat <- train[, 1:3] %>% as.matrix()
    test_mat <- test[,1:3] %>% as.matrix()
    trainRMSE[iter] <- RMSE_R(train_mat, est_rating)
    testRMSE[iter] <- RMSE_R(test_mat, est_rating)
    
    E = Sys.time()
    
    cat("Iter", iter, "  RMSE for train set:", round(trainRMSE[iter], 4), "  RMSE for test set:", round(testRMSE[iter], 4), "  Time cost:", round(E-S, 2), "s\n")
    
    iter <- iter + 1
  }
  # Result
  ratings_est <- est_rating
  return(list("User" = Users, 
              "Movie" = Movies, 
              "Rating" = ratings_est, 
              "Train RMSE" = trainRMSE, 
              "Test RMSE" = testRMSE) 
  )
}


