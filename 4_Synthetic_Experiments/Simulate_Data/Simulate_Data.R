# queue jobs on STORM -----------------------------------------------------

k <- as.numeric(commandArgs(trailingOnly=TRUE)[1]) #needed to queue jobs on storm


# 10 Trials ---------------------------------------------------------------



# Model (a)

if(k==1){
  library(doParallel)
  library(doRNG)
  # Set number of training samples required
  N_reps = seq(1, 100)
  cl <- makeCluster(30) 
  registerDoParallel(cl)
  # set seed for reproducibility
  registerDoRNG(seed = 456)
  start = Sys.time()
  res <- foreach(k = N_reps, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                                  "psych", "pracma", "hawkes")) %dopar%{
                                                    
                                                    source("Functions_Section_4.R")
                                                    
                                                    P_vec = c(12, 48, 96); n.trials = 10; Big_T = 200;
                                                    params = Get_model_parameters(1, P_vec)
                                                    
                                                    HD_A_list = params$HD_A_list
                                                    HD_B_list = params$HD_B_list
                                                    nu_list = params$nu_list
                                                    
                                                    times_list_12 = simulate_times(n.trials, P_vec[1], Big_T, HD_A_list[[1]], HD_B_list[[1]], nu_list[[1]])$times
                                                    times_list_48 = simulate_times(n.trials, P_vec[2], Big_T, HD_A_list[[2]], HD_B_list[[2]], nu_list[[2]])$times
                                                    times_list_96 = simulate_times(n.trials, P_vec[3], Big_T, HD_A_list[[3]], HD_B_list[[3]], nu_list[[3]])$times
                                                    
                                                    return(list(t12 = times_list_12, t48 = times_list_48, t96 = times_list_96)) 
                                                  
                                                  }
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}

# Model (b)

if(k==2){
  library(doParallel)
  library(doRNG)
  # Set number of training samples required
  N_reps = seq(1, 100)
  cl <- makeCluster(30) 
  registerDoParallel(cl)
  # set seed for reproducibility
  registerDoRNG(seed = 456)
  start = Sys.time()
  res <- foreach(k = N_reps, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                           "psych", "pracma", "hawkes")) %dopar%{
                                             
                                             source("Functions_Section_4.R")
                                             
                                             P_vec = c(12, 48, 96); n.trials = 10; Big_T = 200;
                                             params = Get_model_parameters(2, P_vec)
                                             
                                             HD_A_list = params$HD_A_list
                                             HD_B_list = params$HD_B_list
                                             nu_list = params$nu_list
                                             
                                             times_list_12 = simulate_times(n.trials, P_vec[1], Big_T, HD_A_list[[1]], HD_B_list[[1]], nu_list[[1]])$times
                                             times_list_48 = simulate_times(n.trials, P_vec[2], Big_T, HD_A_list[[2]], HD_B_list[[2]], nu_list[[2]])$times
                                             times_list_96 = simulate_times(n.trials, P_vec[3], Big_T, HD_A_list[[3]], HD_B_list[[3]], nu_list[[3]])$times
                                             
                                             return(list(t12 = times_list_12, t48 = times_list_48, t96 = times_list_96)) 
                                             
                                           }
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}

# Model (c)

if(k==3){
  library(doParallel)
  library(doRNG)
  # Set number of training samples required
  N_reps = seq(1, 100)
  cl <- makeCluster(30) 
  registerDoParallel(cl)
  # set seed for reproducibility
  registerDoRNG(seed = 456)
  start = Sys.time()
  res <- foreach(k = N_reps, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                           "psych", "pracma", "hawkes")) %dopar%{
                                             
                                             source("Functions_Section_4.R")
                                             
                                             P_vec = c(12, 48, 96); n.trials = 10; Big_T = 200;
                                             params = Get_model_parameters(3, P_vec)
                                             
                                             HD_A_list = params$HD_A_list
                                             HD_B_list = params$HD_B_list
                                             nu_list = params$nu_list
                                             
                                             times_list_12 = simulate_times(n.trials, P_vec[1], Big_T, HD_A_list[[1]], HD_B_list[[1]], nu_list[[1]])$times
                                             times_list_48 = simulate_times(n.trials, P_vec[2], Big_T, HD_A_list[[2]], HD_B_list[[2]], nu_list[[2]])$times
                                             times_list_96 = simulate_times(n.trials, P_vec[3], Big_T, HD_A_list[[3]], HD_B_list[[3]], nu_list[[3]])$times
                                             
                                             return(list(t12 = times_list_12, t48 = times_list_48, t96 = times_list_96)) 
                                             
                                           }
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}


# 50 Trials ---------------------------------------------------------------

# model (a)
if(k==4){
  library(doParallel)
  library(doRNG)
  # Set number of training samples required
  N_reps = seq(1, 100)
  cl <- makeCluster(30) 
  registerDoParallel(cl)
  # set seed for reproducibility
  registerDoRNG(seed = 456)
  start = Sys.time()
  res <- foreach(k = N_reps, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                           "psych", "pracma", "hawkes")) %dopar%{
                                             
                                             source("Functions_Section_4.R")
                                             
                                             P_vec = c(12, 48, 96); n.trials = 50; Big_T = 200;
                                             params = Get_model_parameters(1, P_vec)
                                             
                                             HD_A_list = params$HD_A_list
                                             HD_B_list = params$HD_B_list
                                             nu_list = params$nu_list
                                             
                                             times_list_12 = simulate_times(n.trials, P_vec[1], Big_T, HD_A_list[[1]], HD_B_list[[1]], nu_list[[1]])$times
                                             times_list_48 = simulate_times(n.trials, P_vec[2], Big_T, HD_A_list[[2]], HD_B_list[[2]], nu_list[[2]])$times
                                             times_list_96 = simulate_times(n.trials, P_vec[3], Big_T, HD_A_list[[3]], HD_B_list[[3]], nu_list[[3]])$times
                                             
                                             return(list(t12 = times_list_12, t48 = times_list_48, t96 = times_list_96)) 
                                             
                                           }
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}

# Model (b)

if(k==5){
  library(doParallel)
  library(doRNG)
  # Set number of training samples required
  N_reps = seq(1, 100)
  cl <- makeCluster(30) 
  registerDoParallel(cl)
  # set seed for reproducibility
  registerDoRNG(seed = 456)
  start = Sys.time()
  res <- foreach(k = N_reps, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                           "psych", "pracma", "hawkes")) %dopar%{
                                             
                                             source("Functions_Section_4.R")
                                             
                                             P_vec = c(12, 48, 96); n.trials = 50; Big_T = 200;
                                             params = Get_model_parameters(2, P_vec)
                                             
                                             HD_A_list = params$HD_A_list
                                             HD_B_list = params$HD_B_list
                                             nu_list = params$nu_list
                                             
                                             times_list_12 = simulate_times(n.trials, P_vec[1], Big_T, HD_A_list[[1]], HD_B_list[[1]], nu_list[[1]])$times
                                             times_list_48 = simulate_times(n.trials, P_vec[2], Big_T, HD_A_list[[2]], HD_B_list[[2]], nu_list[[2]])$times
                                             times_list_96 = simulate_times(n.trials, P_vec[3], Big_T, HD_A_list[[3]], HD_B_list[[3]], nu_list[[3]])$times
                                             
                                             return(list(t12 = times_list_12, t48 = times_list_48, t96 = times_list_96)) 
                                             
                                           }
  end <-Sys.time() 
  stopCluster(cl) 
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}

# Model (c)

if(k==6){
  library(doParallel)
  library(doRNG)
  # Set number of training samples required
  N_reps = seq(1, 100)
  cl <- makeCluster(30) 
  registerDoParallel(cl)
  # set seed for reproducibility
  registerDoRNG(seed = 456)
  start = Sys.time()
  res <- foreach(k = N_reps, .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                           "psych", "pracma", "hawkes")) %dopar%{
                                             
                                             source("Functions_Section_4.R")
                                             
                                             P_vec = c(12, 48, 96); n.trials = 50; Big_T = 200;
                                             params = Get_model_parameters(3, P_vec)
                                             
                                             HD_A_list = params$HD_A_list
                                             HD_B_list = params$HD_B_list
                                             nu_list = params$nu_list
                                             
                                             times_list_12 = simulate_times(n.trials, P_vec[1], Big_T, HD_A_list[[1]], HD_B_list[[1]], nu_list[[1]])$times
                                             times_list_48 = simulate_times(n.trials, P_vec[2], Big_T, HD_A_list[[2]], HD_B_list[[2]], nu_list[[2]])$times
                                             times_list_96 = simulate_times(n.trials, P_vec[3], Big_T, HD_A_list[[3]], HD_B_list[[3]], nu_list[[3]])$times
                                             
                                             return(list(t12 = times_list_12, t48 = times_list_48, t96 = times_list_96)) 
                                             
                                           }
  end <-Sys.time() 
  stopCluster(cl) #need this or it breaks -> will just continue running.
  print(end-start)
  saveRDS(res, file = paste0("res", k, ".RDS"))
}
