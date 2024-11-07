
# queue jobs on STORM -----------------------------------------------------

k <- as.numeric(commandArgs(trailingOnly=TRUE)[1]) #needed to queue jobs on storm

# Model a -----------------------------------------------------------------


# p=12 --------------------------------------------------------------------

if(k==1){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  # get data
  model1_times <- readRDS("model1_times_10.RDS")
  model1_times = model1_times[[1]] #for p = 12
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = model1_times, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                  "psych", "pracma", "hawkes")) %dopar%{
                                                    
                                                    source("Functions_Section_4.R")
                                                    
                                                    Max_iter = 1000;
                                                    n.trials = 10;n.stream = 12 ; Big_T = 200;freq = 0.0628
                                                    
                                                    
                                                    lambda = 0.1142133
                                                    
                                                   
                                                    S_hat = periodogram(k, freq, Big_T)
                                                    res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                    
                                                    
                                                    theta_list = res$theta #get theta estimates
                                                    z_list = res$z #gets Z estimate
                                                    r = res$r
                                                    
                                                    return(list(theta = theta_list, z = z_list, r = r))
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
  # get data
  model1_times <- readRDS("model1_times_10.RDS")
  model1_times = model1_times[[2]] #for p = 48
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = model1_times, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                 "psych", "pracma", "hawkes")) %dopar%{
                                                   
                                                   source("Functions_Section_4.R")
                                                   
                                                   Max_iter = 1000;
                                                   n.trials = 10;n.stream = 48 ; Big_T = 200;freq = 0.0628
                                                   
                                                   
                                                   lambda = 0.1705414
                                                   
                                                   
                                                   S_hat = periodogram(k, freq, Big_T)
                                                   res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                   
                                                   
                                                   theta_list = res$theta #get theta estimates
                                                   z_list = res$z #gets Z estimate
                                                   r = res$r
                                                   
                                                   return(list(theta = theta_list, z = z_list, r = r))
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
  # get data
  model1_times <- readRDS("model1_times_10.RDS")
  model1_times = model1_times[[3]] #for p = 96
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = model1_times, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                 "psych", "pracma", "hawkes")) %dopar%{
                                                   
                                                   source("Functions_Section_4.R")
                                                   
                                                   Max_iter = 1000;
                                                   n.trials = 10;n.stream = 96 ; Big_T = 200;freq = 0.0628
                                                   
                                                   
                                                   lambda = 0.196422
                                                   
                                                   
                                                   S_hat = periodogram(k, freq, Big_T)
                                                   res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                   
                                                   
                                                   theta_list = res$theta #get theta estimates
                                                   z_list = res$z #gets Z estimate
                                                   r = res$r
                                                   
                                                   return(list(theta = theta_list, z = z_list, r = r))
                                                 }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}

# Model b -----------------------------------------------------------------


# p=12 --------------------------------------------------------------------

if(k==4){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  # get data
  model2_times <- readRDS("model2_times_10.RDS")
  model2_times = model2_times[[1]] #for p = 12
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = model1_times, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                 "psych", "pracma", "hawkes")) %dopar%{
                                                   
                                                   source("Functions_Section_4.R")
                                                   
                                                   Max_iter = 1000;
                                                   n.trials = 10;n.stream = 12 ; Big_T = 200;freq = 0.0628
                                                   
                                                   
                                                   lambda = 0.0914544
                                                   
                                                   
                                                   S_hat = periodogram(k, freq, Big_T)
                                                   res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                   
                                                   
                                                   theta_list = res$theta #get theta estimates
                                                   z_list = res$z #gets Z estimate
                                                   r = res$r
                                                   
                                                   return(list(theta = theta_list, z = z_list, r = r))
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
  # get data
  model2_times <- readRDS("model2_times_10.RDS")
  model2_times = model2_times[[2]] #for p = 48
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = model1_times, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                 "psych", "pracma", "hawkes")) %dopar%{
                                                   
                                                   source("Functions_Section_4.R")
                                                   
                                                   Max_iter = 1000;
                                                   n.trials = 10;n.stream = 48 ; Big_T = 200;freq = 0.0628
                                                   
                                                   
                                                   lambda = 2.528668 
                                                   
                                                   
                                                   S_hat = periodogram(k, freq, Big_T)
                                                   res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                   
                                                   
                                                   theta_list = res$theta #get theta estimates
                                                   z_list = res$z #gets Z estimate
                                                   r = res$r
                                                   
                                                   return(list(theta = theta_list, z = z_list, r = r))
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
  # get data
  model2_times <- readRDS("model2_times_10.RDS")
  model2_times = model2_times[[3]] #for p = 96
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = model1_times, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                 "psych", "pracma", "hawkes")) %dopar%{
                                                   
                                                   source("Functions_Section_4.R")
                                                   
                                                   Max_iter = 1000;
                                                   n.trials = 10;n.stream = 96 ; Big_T = 200;freq = 0.0628
                                                   
                                                   
                                                   lambda = 2.710039
                                                   
                                                   
                                                   S_hat = periodogram(k, freq, Big_T)
                                                   res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                   
                                                   
                                                   theta_list = res$theta #get theta estimates
                                                   z_list = res$z #gets Z estimate
                                                   r = res$r
                                                   
                                                   return(list(theta = theta_list, z = z_list, r = r))
                                                 }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}

# Model c -----------------------------------------------------------------


# p=12 --------------------------------------------------------------------

if(k==7){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  # get data
  model3_times <- readRDS("model3_times_10.RDS")
  model3_times = model3_times[[1]] #for p = 12
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = model1_times, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                 "psych", "pracma", "hawkes")) %dopar%{
                                                   
                                                   source("Functions_Section_4.R")
                                                   
                                                   Max_iter = 1000;
                                                   n.trials = 10;n.stream = 12 ; Big_T = 200;freq = 0.0628
                                                   
                                                   
                                                   lambda = 0.03589009
                                                   
                                                   
                                                   S_hat = periodogram(k, freq, Big_T)
                                                   res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                   
                                                   
                                                   theta_list = res$theta #get theta estimates
                                                   z_list = res$z #gets Z estimate
                                                   r = res$r
                                                   
                                                   return(list(theta = theta_list, z = z_list, r = r))
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
  # get data
  model3_times <- readRDS("model3_times_10.RDS")
  model3_times = model3_times[[2]] #for p = 48
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = model1_times, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                 "psych", "pracma", "hawkes")) %dopar%{
                                                   
                                                   source("Functions_Section_4.R")
                                                   
                                                   Max_iter = 1000;
                                                   n.trials = 10;n.stream = 48 ; Big_T = 200;freq = 0.0628
                                                   
                                                   
                                                   lambda = 0.1777108 
                                                   
                                                   
                                                   S_hat = periodogram(k, freq, Big_T)
                                                   res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                   
                                                   
                                                   theta_list = res$theta #get theta estimates
                                                   z_list = res$z #gets Z estimate
                                                   r = res$r
                                                   
                                                   return(list(theta = theta_list, z = z_list, r = r))
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
  # get data
  model3_times <- readRDS("model3_times_10.RDS")
  model3_times = model3_times[[3]] #for p = 96
  cl <- makeCluster(30) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  start2 = Sys.time()
  res <- foreach(k = model1_times, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                 "psych", "pracma", "hawkes")) %dopar%{
                                                   
                                                   source("Functions_Section_4.R")
                                                   
                                                   Max_iter = 1000;
                                                   n.trials = 10;n.stream = 96 ; Big_T = 200;freq = 0.0628
                                                   
                                                   
                                                   lambda = 0.2671631
                                                   
                                                   
                                                   S_hat = periodogram(k, freq, Big_T)
                                                   res = glasso(n.stream, lambda, S_hat, Max_iter)
                                                   
                                                   
                                                   theta_list = res$theta #get theta estimates
                                                   z_list = res$z #gets Z estimate
                                                   r = res$r
                                                   
                                                   return(list(theta = theta_list, z = z_list, r = r))
                                                 }
  
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}