# Libraries ---------------------------------------------------------------

library(Matrix)
library(phonTools) #for sinc function
library(QZ) #for H - conjugate transpose
library(purrr) #for map function
library(matrixStats) #for colQuantiles
library(complexplus) #for deterimnan of complex matrix
library(psych) #for trace
library(pracma) #for logspace
library(hawkes) #for simulating hawkes process via Ogata's method


setwd("~/Downloads/RSE/5_Neuronal_Synchronicity")
source("Functions_Section_5.R")


# Parameter Tuning for Delta Band -----------------------------------------


# 0 mW/mm -----------------------------------------------------------------


library(pracma)
lambdas = logspace(log10(0.001), log10(10), 100)
library(doParallel)
library(doRNG)
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
res <- foreach(j=lambdas,  .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                         "psych", "pracma", "hawkes")) %dopar%{
                                           setwd("~/Downloads/RSE/5_Neuronal_Synchronicity")
                                           source("Functions_Section_5.R")
                                           
                                           n.trials = 10
                                           n.stream = 26
                                           
                                           S_hat <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Multi_trial_periodogram/Laser_on_estimates/S_OB_0.RDS")
                                           
                                           freqs <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Multi_trial_periodogram/freqs.RDS")
                                           
                                           S_hat = S_hat[[1]]
                                           f = freqs[1]
                                           
                                           Max_iter = 1000
                                           
                                           out = glasso(n.stream, j, S_hat, Max_iter)
                                           
                                           theta = out$theta 
                                           zs = out$z
                                           
                                           gam = 0.5
                                           
                                           lik = eBIC(theta, S_hat, zs, gam, n.stream, f, n.trials)
                                           
                                           return(list(eBIC = lik, theta = theta, z = zs, r = out$r ) )
                                           
                                         }
end <-Sys.time() 
stopCluster(cl) 
print(end-start)
setwd("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Delta_band/Laser_on")
saveRDS(res, file ="est_0.RDS")


# 10 mW/mm -----------------------------------------------------------------

library(pracma)
lambdas = logspace(log10(0.001), log10(10), 100)
library(doParallel)
library(doRNG)
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
res <- foreach(j=lambdas,  .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                         "psych", "pracma", "hawkes")) %dopar%{
                                           
                                           setwd("~/Downloads/RSE/5_Neuronal_Synchronicity")
                                           source("Functions_Section_5.R")
                                           
                                           n.trials = 10
                                           n.stream = 26
                                           
                                           S_hat <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Multi_trial_periodogram/Laser_on_estimates/S_OB_10.RDS")
                                           
                                           freqs <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Multi_trial_periodogram/freqs.RDS")
                                           
                                           S_hat = S_hat[[1]] # 1 indicates delta band
                                           f = freqs[1]
                                           
                                           Max_iter = 1000
                                           
                                           out = glasso(n.stream, j, S_hat, Max_iter)
                                           
                                           theta = out$theta 
                                           zs = out$z
                                           
                                           gam = 0.5
                                           
                                           lik = eBIC(theta, S_hat, zs, gam, n.stream, f, n.trials)
                                           
                                           return(list(eBIC = lik, theta = theta, z = zs, r = out$r ) )
                                           
                                         }
end <-Sys.time() 
stopCluster(cl) 
print(end-start)
setwd("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Delta_band/Laser_on")
saveRDS(res, file ="est_10.RDS")

# 50 mW/mm -----------------------------------------------------------------

library(pracma)
lambdas = logspace(log10(0.001), log10(10), 100)
library(doParallel)
library(doRNG)
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
res <- foreach(j=lambdas,  .packages = c("Matrix","phonTools", "QZ", "purrr", "matrixStats", "complexplus", 
                                         "psych", "pracma", "hawkes")) %dopar%{
                                          
                                           setwd("~/Downloads/RSE/5_Neuronal_Synchronicity")
                                           source("Functions_Section_5.R")
                                           
                                           n.trials = 10
                                           n.stream = 26
                                           
                                           S_hat <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Multi_trial_periodogram/Laser_on_estimates/S_OB_50.RDS")
                                           
                                           freqs <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Multi_trial_periodogram/freqs.RDS")
                                           
                                           S_hat = S_hat[[1]]
                                           f = freqs[1]
                                           
                                           Max_iter = 1000
                                           
                                           out = glasso(n.stream, j, S_hat, Max_iter)
                                           
                                           theta = out$theta 
                                           zs = out$z
                                           
                                           gam = 0.5
                                           
                                           lik = eBIC(theta, S_hat, zs, gam, n.stream, f, n.trials)
                                           
                                           return(list(eBIC = lik, theta = theta, z = zs, r = out$r ) )
                                         }
end <-Sys.time() 
stopCluster(cl) 
print(end-start)
setwd("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Delta_band/Laser_on")
saveRDS(res, file ="est_50.RDS")



