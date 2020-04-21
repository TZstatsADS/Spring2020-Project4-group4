############################# KRR for ALS with Regularization ##############################
ALS_R_KRR <- function(data, train, test, f, maxIters, lambda_als, lambda_p, sigma,beta){
  
  # f = f
  # lambda = lambda_als
  # beta = beta
  # maxIters = maxIters
  # data = data
  #   train = train
  #   test = test
  set.seed(1)
  result_ALS <- ALS.R1R3(f = f, lambda = lambda_als, beta = beta, maxIters = maxIters , data = data,
                         train = train, test = test)
  
  KRR.Post_R(result_ALS=result_ALS,lambda=lambda_p, sigma=sigma, data, train, test)
}
