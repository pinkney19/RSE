
# No laser investigation --------------------------------------------------

library(Matrix)
library(phonTools) 
library(QZ) 
library(purrr) 
library(matrixStats) 
library(complexplus) 
library(psych) 
library(pracma) 
library(hawkes) 
library(igraph)

setwd("~/Downloads/RSE/5_Neuronal_Synchronicity")
source("Functions_Section_5.R")

OB_0 <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Pre_processing/OB_0.RDS")
OB_0 <- pre_processing_function(OB_0)


Extract_data = function(data){
  stim1 = stim2 = bet_stim = list();
  for(i in 1:10){ #i in 1:10
    stim1[[i]] = data[[i]][data[[i]]>0 & data[[i]]<=1]#data on interval (0,1)
    stim2[[i]] = data[[i]][data[[i]]>9 & data[[i]]<=10] #data on interval (9,10)
    bet_stim[[i]] = data[[i]][data[[i]]>4 & data[[i]]<=5] #data on interval (1,2)
  }
  return(list(stim1 = stim1, stim2 = stim2, bet_stim = bet_stim))
}


Get_stim_data = function(data){
  res = lapply(data, Extract_data)
  stim1 = map(res,1) #data for (0,1)  interval stimulus
  stim2 = map(res,2) #data for  (9,10) interval - i.e. after stimulus
  bet_stim = map(res, 3) #data for (1,9) interval - i.e. before stimulus
  
  return(list(x = stim1, y = stim2, z = bet_stim))
}

OB_0 <- Get_stim_data(OB_0)
OB_0_stim <- OB_0$x
OB_0_no_stim <- OB_0$z


# periodogram for no laser condition --------------------------------------
om= seq(1,50) *2*pi
S_hat = Trial_freq_Estimator(om, OB_0_no_stim, 1)$pooled_s
setwd("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/zero_investigations")
saveRDS(S_hat, "S_hat.RDS")

# Estimation --------------------------------------------------------------


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
                                           
                                           S_hat <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/zero_investigations/S_hat.RDS")
                                           
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


# get results -------------------------------------------------------------

out = calc_result(res)
edges = function(mat){
  u = mat[upper.tri(mat, diag=F)]
  edges = length(u[u!=0])
  return(edges)
}

edges(out$pc)


# load previous results
est1_0 <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Delta_band/Laser_on/est_0.RDS")
res1 = calc_result(est1_0)

library(igraph)
zero = list(out$pc, res1$pc)
zero_points = c(("AvgPoints = "), expression("AvgPoints="))
plot_UGM_new(zero, title_list = c(expression(0~mW/mm^2), expression(0~mW/mm^2)), zero_points)
