
# Results from simulations -------------------------------------------
setwd("~/Downloads/RSE/4_Synthetic_Experiments")
source("Functions_Section_4.R")
library(purrr)
library(pracma) #for logspace
library(Matrix)
library(phonTools)
library(QZ) 
library(purrr) 
library(matrixStats) 
library(complexplus) 
library(psych) 
library(pracma) 
library(hawkes) 
library(pROC)


# Parameters --------------------------------------------------------------

P_vec = c(12,48,96)
alpha_list <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Parameters/alpha_list.RDS")
beta_list <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Parameters/beta_list.RDS")


# Required functions -------------------------------------------------------

metrics = function(res, gt){
  
  av_mse = mean( unlist(lapply(res, function(x) performance_measures(x, gt$theta, F)$l2)) )
  se = std_err(unlist(lapply(res, function(x) performance_measures(x, gt$theta, F)$l2)))
  
  return(list(av_mse = av_mse, se = se))
}

get_ground_truth_lr = function(P_vec, freq, alpha_list, beta_list){
  true_theta = list(); true_theta = rep(list(true_theta), length(P_vec));
  true_s = list(); true_s = rep(list(true_s), length(P_vec))
  true_pc = list(); true_pc = rep(list(true_pc), length(P_vec))
  for(i in 1:length(P_vec)){
    HD_A = alpha_list[[i]]; HD_B =beta_list[[i]]
    nu = rep(0.2, P_vec[i])
    gt = MV_spectra(freq, HD_A, HD_B, nu, P_vec[i])
    true_s[[i]] = gt$spectra[[1]]
    true_pc[[i]] = gt$pc[[1]]
    true_theta[[i]] = gt$inv[[1]]
  }
  return(list(theta=true_theta, s = true_s, pc = true_pc))
}


# get ground truth --------------------------------------------------------

ground_truth = get_ground_truth_lr(P_vec, freq = 0.0628, alpha_list, beta_list)

gta_12 = map(ground_truth,1)
gta_48 = map(ground_truth,2)




# load data - all 50 trials
out1 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Periodogram/results/out1.RDS") #p=12
out2 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Periodogram/results/out2.RDS") #p=48


r1 = metrics(out1, gta_12)
r2 = metrics(out2, gta_48)



mse = c(r1$av_mse, r2$av_mse)
se = c(r1$se, r2$se)

res = cbind(mse, se)
setwd("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Periodogram")
saveRDS(res, "res_low_rank_periodogram.RDS")
