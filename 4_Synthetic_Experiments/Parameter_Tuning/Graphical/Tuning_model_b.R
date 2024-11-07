# queue jobs on STORM -----------------------------------------------------

k <- as.numeric(commandArgs(trailingOnly=TRUE)[1]) #needed to queue jobs on storm

# 10 trials ---------------------------------------------------------------

# p=12

if(k==1){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(log10(0.001),log10(10),100)
  data_seq = seq(1, 10) #10 samples of training data
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(30) 
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start = Sys.time()
  res <- foreach(k = list_of_pairs, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                  "psych", "pracma", "hawkes")) %dopar%{
                                    
                                    source("Functions_Section_4.R")
                                   
                                    Max_iter = 1000;
                                    n.trials = 10;n.stream = 12 ; Big_T = 200;freq = 0.0628
                                    
                                    lambda = as.numeric(k[2])
                                    
                                    # get data
                                    model_times <- readRDS("res2.RDS") # for model b with 10 trials
                                    model_times = map(model_times,1) #for p = 12
                                    
                                    data_idx = as.numeric(k[1])
                                    data = model1_times[[data_idx]]
                                    
                                    S_hat = periodogram(data, freq, Big_T)
                                    res = glasso(n.stream, lambda, S_hat, Max_iter)
                                    
                                    
                                    theta_list = res$theta #get theta estimates
                                    z_list = res$z #gets Z estimate
                                    
                                    return(list(theta_list, z_list)) 
                                  }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("out", k, ".RDS"))
}

# p=48
if(k==2){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(log10(0.001),log10(10),100)
  data_seq = seq(1, 10) #10 samples of training data
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(30) 
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start = Sys.time()
  res <- foreach(k = list_of_pairs, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                  "psych", "pracma", "hawkes")) %dopar%{
                                                    
                                                    source("Functions_Section_4.R")
                                                    
                                                    Max_iter = 1000;
                                                    n.trials = 10;n.stream = 48 ; Big_T = 200;freq = 0.0628
                                                    
                                                    lambda = as.numeric(k[2])
                                                    
                                                    # get data
                                                    model_times <- readRDS("res2.RDS")
                                                    model_times = map(model_times,2) #for p = 48 
                                                    
                                                    data_idx = as.numeric(k[1])
                                                    data = model1_times[[data_idx]]
                                                    
                                                    S_hat = periodogram(data, freq, Big_T)
                                                    res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                    
                                                    
                                                    theta_list = res$theta #get theta estimates
                                                    z_list = res$z #gets Z estimate
                                                    
                                                    return(list(theta_list, z_list)) 
                                                  }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("out", k, ".RDS"))
}

# p = 96

if(k==3){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(log10(0.001),log10(10),100)
  data_seq = seq(1, 10) #10 samples of training data
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(30) 
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start = Sys.time()
  res <- foreach(k = list_of_pairs, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                  "psych", "pracma", "hawkes")) %dopar%{
                                                    
                                                    source("Functions_Section_4.R")
                                                    
                                                    Max_iter = 1000;
                                                    n.trials = 10;n.stream = 96 ; Big_T = 200;freq = 0.0628
                                                    
                                                    lambda = as.numeric(k[2])
                                                    
                                                    # get data
                                                    model_times <- readRDS("res2.RDS")
                                                    model_times = map(model_times,3) #for p = 96 
                                                    
                                                    data_idx = as.numeric(k[1])
                                                    data = model1_times[[data_idx]]
                                                    
                                                    S_hat = periodogram(data, freq, Big_T)
                                                    res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                    
                                                    
                                                    theta_list = res$theta #get theta estimates
                                                    z_list = res$z #gets Z estimate
                                                    
                                                    return(list(theta_list, z_list)) 
                                                  }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("out", k, ".RDS"))
}


# 50 Trials ---------------------------------------------------------------

# p = 12
if(k==4){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(log10(0.001),log10(10),100)
  data_seq = seq(1, 10) #10 samples of training data
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(30) 
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start = Sys.time()
  res <- foreach(k = list_of_pairs, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                  "psych", "pracma", "hawkes")) %dopar%{
                                                    
                                                    source("Functions_Section_4.R")
                                                    
                                                    Max_iter = 1000;
                                                    n.trials = 50;n.stream = 12 ; Big_T = 200;freq = 0.0628
                                                    
                                                    lambda = as.numeric(k[2])
                                                    
                                                    # get data
                                                    model_times <- readRDS("res5.RDS") # for model a 50 trials
                                                    model_times = map(model_times,1) #for p = 12 
                                                    
                                                    data_idx = as.numeric(k[1])
                                                    data = model1_times[[data_idx]]
                                                    
                                                    S_hat = periodogram(data, freq, Big_T)
                                                    res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                    
                                                    
                                                    theta_list = res$theta #get theta estimates
                                                    z_list = res$z #gets Z estimate
                                                    
                                                    return(list(theta_list, z_list)) 
                                                  }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("out", k, ".RDS"))
}

# p = 48
if(k==5){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(log10(0.001),log10(10),100)
  data_seq = seq(1, 10) #10 samples of training data
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(30) 
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start = Sys.time()
  res <- foreach(k = list_of_pairs, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                  "psych", "pracma", "hawkes")) %dopar%{
                                                    
                                                    source("Functions_Section_4.R")
                                                    
                                                    Max_iter = 1000;
                                                    n.trials = 50;n.stream = 48 ; Big_T = 200;freq = 0.0628
                                                    
                                                    lambda = as.numeric(k[2])
                                                    
                                                    # get data
                                                    model_times <- readRDS("res5.RDS") # for model a 50 trials
                                                    model_times = map(model_times,2) #for p = 48 
                                                    
                                                    data_idx = as.numeric(k[1])
                                                    data = model1_times[[data_idx]]
                                                    
                                                    S_hat = periodogram(data, freq, Big_T)
                                                    res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                    
                                                    
                                                    theta_list = res$theta #get theta estimates
                                                    z_list = res$z #gets Z estimate
                                                    
                                                    return(list(theta_list, z_list)) 
                                                  }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("out", k, ".RDS"))
}

# p = 96

if(k==6){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(log10(0.001),log10(10),100)
  data_seq = seq(1, 10) #10 samples of training data
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(30) 
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start = Sys.time()
  res <- foreach(k = list_of_pairs, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                  "psych", "pracma", "hawkes")) %dopar%{
                                                    
                                                    source("Functions_Section_4.R")
                                                    
                                                    Max_iter = 1000;
                                                    n.trials = 50;n.stream = 96 ; Big_T = 200;freq = 0.0628
                                                    
                                                    lambda = as.numeric(k[2])
                                                    
                                                    # get data
                                                    model_times <- readRDS("res5.RDS")
                                                    model_times = map(model_times,3) #for p = 96 
                                                    
                                                    data_idx = as.numeric(k[1])
                                                    data = model1_times[[data_idx]]
                                                    
                                                    S_hat = periodogram(data, freq, Big_T)
                                                    res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                    
                                                    
                                                    theta_list = res$theta #get theta estimates
                                                    z_list = res$z #gets Z estimate
                                                    
                                                    return(list(theta_list, z_list)) 
                                                  }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("out", k, ".RDS"))
}


