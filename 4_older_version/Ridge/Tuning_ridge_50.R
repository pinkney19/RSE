# queue jobs on STORM -----------------------------------------------------

k <- as.numeric(commandArgs(trailingOnly=TRUE)[1]) #needed to queue jobs on storm

# Model 1 -----------------------------------------------------------------


# p=12 --------------------------------------------------------------------

if(k==1){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(-5,1,100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = list_of_pairs, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                  "psych", "pracma", "hawkes")) %dopar%{
                                                    
                                                    source("Functions_Section_4.R")
                                                    
                                                    Max_iter = 1000;
                                                    n.trials = 50;n.stream = 12 ; Big_T = 200;freq = 0.0628
                                                    
                                                   
                                                    lambda = as.numeric(k[2])
                                                    
                                                    # get data
                                                    model1_times <- readRDS("model1_times_50.RDS")
                                                    model1_times = model1_times[[1]] #for p = 12
                                                    
                                                    data_idx = as.numeric(k[1])
                                                    data = model1_times[[data_idx]]
                                                    
                                                    # Ridge estimator
                                                    res = Ridge( n.stream, n.trials, Big_T, freq, lambda, data)
                                                    
                                                    theta = res$theta
                                                    S_hat = periodogram(data, freq, Big_T)
                                                    
                                                    return(list(theta, S_hat))
                                                  }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}

# p=48 --------------------------------------------------------------------


if(k==2){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(-5,1,100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = list_of_pairs, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                  "psych", "pracma", "hawkes")) %dopar%{
                                                    
                                                    source("Functions_Section_4.R")
                                                    
                                                    Max_iter = 1000;
                                                    n.trials = 50;n.stream = 48 ; Big_T = 200;freq = 0.0628
                                                    
                                                    #lambdas = logspace(-5,1,100) #define grid to search over
                                                    lambda = as.numeric(k[2])
                                                    
                                                    # get data
                                                    model1_times <- readRDS("model1_times_50.RDS")
                                                    model1_times = model1_times[[2]] #for p = 48
                                                    
                                                    data_idx = as.numeric(k[1])
                                                    data = model1_times[[data_idx]]
                                                    
                                                    # Ridge estimator
                                                    res = Ridge( n.stream, n.trials, Big_T, freq, lambda, data)
                                                    
                                                    theta = res$theta
                                                    S_hat = periodogram(data, freq, Big_T)
                                                    
                                                    return(list(theta, S_hat))
                                                  }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}


# p=96 --------------------------------------------------------------------


if(k==3){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(-5,1,100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = list_of_pairs, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                  "psych", "pracma", "hawkes")) %dopar%{
                                                    
                                                    source("Functions_Section_4.R")
                                                    
                                                    Max_iter = 1000;
                                                    n.trials = 50;n.stream = 96 ; Big_T = 200;freq = 0.0628
                                                    
                                                    
                                                    lambda = as.numeric(k[2])
                                                    
                                                    # get data
                                                    model1_times <- readRDS("model1_times_50.RDS")
                                                    model1_times = model1_times[[3]] #for p = 48
                                                    
                                                    data_idx = as.numeric(k[1])
                                                    data = model1_times[[data_idx]]
                                                    
                                                    # Ridge estimator
                                                    res = Ridge(n.stream, n.trials, Big_T, freq, lambda, data)
                                                    
                                                    theta = res$theta
                                                    S_hat = periodogram(data, freq, Big_T)
                                                    
                                                    return(list(theta, S_hat))
                                                  }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}




# Model 2 -----------------------------------------------------------------


# p=12 --------------------------------------------------------------------

if(k==4){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(-5,1,100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = list_of_pairs, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                  "psych", "pracma", "hawkes")) %dopar%{
                                                    
                                                    source("Functions_Section_4.R")
                                                    
                                                    Max_iter = 1000;
                                                    n.trials = 50;n.stream = 12 ; Big_T = 200;freq = 0.0628
                                                    
                                                    #lambdas = logspace(-5,1,100) #define grid to search over
                                                    lambda = as.numeric(k[2])
                                                    
                                                    # get data
                                                    model2_times <- readRDS("model2_times_50.RDS")
                                                    model2_times = model2_times[[1]] #for p = 12
                                                    
                                                    data_idx = as.numeric(k[1])
                                                    data = model2_times[[data_idx]]
                                                    
                                                    # Ridge estimator
                                                    res = Ridge(n.stream, n.trials, Big_T, freq, lambda, data)
                                                    
                                                    theta = res$theta
                                                    S_hat = periodogram(data, freq, Big_T)
                                                    
                                                    return(list(theta, S_hat))
                                                  }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}

# p=48 --------------------------------------------------------------------


if(k==5){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(-5,1,100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = list_of_pairs, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                  "psych", "pracma", "hawkes")) %dopar%{
                                                    
                                                    source("Functions_Section_4.R")
                                                    
                                                    Max_iter = 1000;
                                                    n.trials = 50;n.stream = 48 ; Big_T = 200;freq = 0.0628
                                                    
                                                    #lambdas = logspace(-5,1,100) #define grid to search over
                                                    lambda = as.numeric(k[2])
                                                    
                                                    # get data
                                                    model2_times <- readRDS("model2_times_50.RDS")
                                                    model2_times = model2_times[[2]] #for p = 48
                                                    
                                                    data_idx = as.numeric(k[1])
                                                    data = model2_times[[data_idx]]
                                                    
                                                    # Ridge estimator
                                                    res = Ridge(n.stream, n.trials, Big_T, freq, lambda, data)
                                                    
                                                    theta = res$theta
                                                    S_hat = periodogram(data, freq, Big_T)
                                                    
                                                    return(list(theta, S_hat))
                                                  }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}


# p=96 --------------------------------------------------------------------


if(k==6){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(-5,1,100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = list_of_pairs, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                  "psych", "pracma", "hawkes")) %dopar%{
                                                    
                                                    source("Functions_Section_4.R")
                                                    
                                                    Max_iter = 1000;
                                                    n.trials = 50;n.stream = 96 ; Big_T = 200;freq = 0.0628
                                                    
                                                    
                                                    lambda = as.numeric(k[2])
                                                    
                                                    # get data
                                                    model2_times <- readRDS("model2_times_50.RDS")
                                                    model2_times = model2_times[[3]] #for p = 48
                                                    
                                                    data_idx = as.numeric(k[1])
                                                    data = model2_times[[data_idx]]
                                                    
                                                    # Ridge estimator
                                                    res = Ridge(n.stream, n.trials, Big_T, freq, lambda, data)
                                                    
                                                    theta = res$theta
                                                    S_hat = periodogram(data, freq, Big_T)
                                                    
                                                    return(list(theta, S_hat))
                                                  }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}


# Model 3 -----------------------------------------------------------------


# p=12 --------------------------------------------------------------------

if(k==7){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(-5,1,100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = list_of_pairs, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                  "psych", "pracma", "hawkes")) %dopar%{
                                    
                                    source("Functions_Section_4.R")
                                   
                                    Max_iter = 1000;
                                    n.trials = 50;n.stream = 12 ; Big_T = 200;freq = 0.0628
                                    
                                    #lambdas = logspace(-5,1,100) #define grid to search over
                                    lambda = as.numeric(k[2])
                                    
                                    # get data
                                    model3_times <- readRDS("model3_times_50.RDS")
                                    model3_times = model3_times[[1]] #for p = 12
                                    
                                    data_idx = as.numeric(k[1])
                                    data = model3_times[[data_idx]]
                                    
                                    # Ridge estimator
                                    res = Ridge(n.stream, n.trials, Big_T, freq, lambda, data)
                                    
                                    theta = res$theta
                                    S_hat = periodogram(data, freq, Big_T)
                                    
                                    return(list(theta, S_hat))
                                  }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}

# p=48 --------------------------------------------------------------------


if(k==8){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(-5,1,100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = list_of_pairs, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                  "psych", "pracma", "hawkes")) %dopar%{
                                                    
                                                    source("Functions_Section_4.R")
                                                    
                                                    Max_iter = 1000;
                                                    n.trials = 50;n.stream = 48 ; Big_T = 200;freq = 0.0628
                                                    
                                                    #lambdas = logspace(-5,1,100) #define grid to search over
                                                    lambda = as.numeric(k[2])
                                                    
                                                    # get data
                                                    model3_times <- readRDS("model3_times_50.RDS")
                                                    model3_times = model3_times[[2]] #for p = 48
                                                    
                                                    data_idx = as.numeric(k[1])
                                                    data = model3_times[[data_idx]]
                                                    
                                                    # Ridge estimator
                                                    res = Ridge(n.stream, n.trials, Big_T, freq, lambda, data)
                                                    
                                                    theta = res$theta
                                                    S_hat = periodogram(data, freq, Big_T)
                                                    
                                                    return(list(theta, S_hat))
                                                  }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}


# p=96 --------------------------------------------------------------------


if(k==9){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(-5,1,100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = list_of_pairs, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                  "psych", "pracma", "hawkes")) %dopar%{
                                                    
                                                    source("Functions_Section_4.R")
                                                    
                                                    Max_iter = 1000;
                                                    n.trials = 50;n.stream = 96 ; Big_T = 200;freq = 0.0628
                                                    
                                                    
                                                    lambda = as.numeric(k[2])
                                                    
                                                    # get data
                                                    model3_times <- readRDS("model3_times_50.RDS")
                                                    model3_times = model3_times[[3]] #for p = 48
                                                    
                                                    data_idx = as.numeric(k[1])
                                                    data = model3_times[[data_idx]]
                                                    
                                                    # Ridge estimator
                                                    res = Ridge(n.stream, n.trials, Big_T, freq, lambda, data)
                                                    
                                                    theta = res$theta
                                                    S_hat = periodogram(data, freq, Big_T)
                                                    
                                                    return(list(theta, S_hat))
                                                  }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}
