
# queue jobs on STORM -----------------------------------------------------

k <- as.numeric(commandArgs(trailingOnly=TRUE)[1]) #needed to queue jobs on storm

# Model a -----------------------------------------------------------------


# 10 Trials ---------------------------------------------------------------

# p = 12

if(k==1){
  library(doParallel)
  library(doRNG)
  library(purrr) 
  # get data
  model_times <- readRDS("res3.RDS")
  model_times = map(model_times,1) #for p = 12
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = model_times, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                  "psych", "pracma", "hawkes")) %dopar%{
                                                    
                                                    source("Functions_Section_4.R")
                                                    
                                                    Max_iter = 1000;
                                                    n.trials = 10;n.stream = 12 ; Big_T = 200;freq = 0.0628
                                                    
                                                    
                                                    lambda = 0.03492325
                                                    
                                                   
                                                    S_hat = periodogram(k, freq, Big_T)
                                                    res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                    
                                                    
                                                    theta_list = res$theta #get theta estimates
                                                    z_list = res$z #gets Z estimate
                                                    r = res$r
                                                    
                                                    return(list(theta = theta_list, z = z_list, r = r))
                                                  }
  
  end <-Sys.time()
  print(end-start)
  saveRDS(res, file = paste0("out", k, ".RDS"))
  stopCluster(cl) 
}

# p = 48


if(k==2){
  library(doParallel)
  library(doRNG)
  library(purrr) 
  # get data
  model_times <- readRDS("res3.RDS")
  model_times = map(model_times,2) #for p = 48
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = model_times, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                 "psych", "pracma", "hawkes")) %dopar%{
                                                   
                                                   source("Functions_Section_4.R")
                                                   
                                                   Max_iter = 1000;
                                                   n.trials = 10;n.stream = 48 ; Big_T = 200;freq = 0.0628
                                                   
                                                   
                                                   lambda = 0.1791334
                                                   
                                                   
                                                   S_hat = periodogram(k, freq, Big_T)
                                                   res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                   
                                                   
                                                   theta_list = res$theta #get theta estimates
                                                   z_list = res$z #gets Z estimate
                                                   r = res$r
                                                   
                                                   return(list(theta = theta_list, z = z_list, r = r))
                                                 }
  
  end <-Sys.time()
  print(end-start)
  saveRDS(res, file = paste0("out", k, ".RDS"))
  stopCluster(cl) 
}

# p = 96


if(k==3){
  library(doParallel)
  library(doRNG)
  library(purrr) 
  # get data
  model_times <- readRDS("res3.RDS")
  model_times = map(model_times,3) #for p = 96
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = model_times, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                 "psych", "pracma", "hawkes")) %dopar%{
                                                   
                                                   source("Functions_Section_4.R")
                                                   
                                                   Max_iter = 1000;
                                                   n.trials = 10;n.stream = 96 ; Big_T = 200;freq = 0.0628
                                                   
                                                   
                                                   lambda = 0.2459049
                                                   
                                                   
                                                   S_hat = periodogram(k, freq, Big_T)
                                                   res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                   
                                                   
                                                   theta_list = res$theta #get theta estimates
                                                   z_list = res$z #gets Z estimate
                                                   r = res$r
                                                   
                                                   return(list(theta = theta_list, z = z_list, r = r))
                                                 }
  
  end <-Sys.time()
  print(end-start)
  saveRDS(res, file = paste0("out", k, ".RDS"))
  stopCluster(cl) 
}



# 50 Trials ---------------------------------------------------------------

# p = 12
if(k==4){
  library(doParallel)
  library(doRNG)
  library(purrr) 
  # get data
  model_times <- readRDS("res6.RDS")
  model_times = map(model_times,1) #for p = 12
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = model_times, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                "psych", "pracma", "hawkes")) %dopar%{
                                                  
                                                  source("Functions_Section_4.R")
                                                  
                                                  Max_iter = 1000;
                                                  n.trials = 50;n.stream = 12 ; Big_T = 200;freq = 0.0628
                                                  
                                                  
                                                  lambda = 0.01135336
                                                  
                                                  
                                                  S_hat = periodogram(k, freq, Big_T)
                                                  res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                  
                                                  
                                                  theta_list = res$theta #get theta estimates
                                                  z_list = res$z #gets Z estimate
                                                  r = res$r
                                                  
                                                  return(list(theta = theta_list, z = z_list, r = r))
                                                }
  
  end <-Sys.time()
  print(end-start)
  saveRDS(res, file = paste0("out", k, ".RDS"))
  stopCluster(cl) 
}

# p = 48
if(k==5){
  library(doParallel)
  library(doRNG)
  library(purrr) 
  # get data
  model_times <- readRDS("res6.RDS")
  model_times = map(model_times,2) #for p = 48
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = model_times, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                "psych", "pracma", "hawkes")) %dopar%{
                                                  
                                                  source("Functions_Section_4.R")
                                                  
                                                  Max_iter = 1000;
                                                  n.trials = 50;n.stream = 48 ; Big_T = 200;freq = 0.0628
                                                  
                                                  
                                                  lambda = 0.03559899
                                                  
                                                  
                                                  S_hat = periodogram(k, freq, Big_T)
                                                  res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                  
                                                  
                                                  theta_list = res$theta #get theta estimates
                                                  z_list = res$z #gets Z estimate
                                                  r = res$r
                                                  
                                                  return(list(theta = theta_list, z = z_list, r = r))
                                                }
  
  end <-Sys.time()
  print(end-start)
  saveRDS(res, file = paste0("out", k, ".RDS"))
  stopCluster(cl) 
}


# p = 96

if(k==6){
  library(doParallel)
  library(doRNG)
  library(purrr) 
  # get data
  model_times <- readRDS("res6.RDS")
  model_times = map(model_times,3) #for p = 96
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = model_times, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                "psych", "pracma", "hawkes")) %dopar%{
                                                  
                                                  source("Functions_Section_4.R")
                                                  
                                                  Max_iter = 1000;
                                                  n.trials = 50;n.stream = 96 ; Big_T = 200;freq = 0.0628
                                                  
                                                  
                                                  lambda = 0.08627757
                                                  
                                                  
                                                  S_hat = periodogram(k, freq, Big_T)
                                                  res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                  
                                                  
                                                  theta_list = res$theta #get theta estimates
                                                  z_list = res$z #gets Z estimate
                                                  r = res$r
                                                  
                                                  return(list(theta = theta_list, z = z_list, r = r))
                                                }
  
  end <-Sys.time()
  print(end-start)
  saveRDS(res, file = paste0("out", k, ".RDS"))
  stopCluster(cl) 
}

