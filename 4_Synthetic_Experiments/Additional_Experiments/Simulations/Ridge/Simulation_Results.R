
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


# Required functions -------------------------------------------------------

metrics = function(res, gt){
  
  av_mse = mean( unlist(lapply(res, function(x) performance_measures(x, gt$theta, F)$l2)) )
  se = std_err(unlist(lapply(res, function(x) performance_measures(x, gt$theta, F)$l2)))
  
  # F1 score
  pc_list = lapply(res, partial_co)
  F1 = mean(unlist(lapply(pc_list, function(x) performance_measures(x, gt$pc, T)$F1)))
  F1_se = std_err(unlist(lapply(pc_list, function(x) performance_measures(x, gt$pc, T)$F1)))
  
  return(list(av_mse = av_mse, se = se, av_F1 = F1, F1_se = F1_se))
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



results_table_ridge = function(P_vec, freq, alpha_list, beta_list){
  
  # get ground truth
  ground_truth = get_ground_truth_lr(P_vec, freq, alpha_list, beta_list)
  
  gt_12 = map(ground_truth,1)
  gt_48 = map(ground_truth,2)
  gt_96 = map(ground_truth,3)
  
 
  # mse results -------------------------------------------------------------
  
  # load data
  # 10 trials 
  
  out1 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Ridge/results_mse/out1.RDS") #p=12
  out2 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Ridge/results_mse/out2.RDS") #p=48
  out3 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Ridge/results_mse/out3.RDS") #p=96
  # 50 trials
  out4 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Ridge/results_mse/out4.RDS") #p=12
  out5 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Ridge/results_mse/out5.RDS") #p=48
  out6 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Ridge/results_mse/out6.RDS") #p=96
  
  
  r1 = metrics(out1, gt_12)
  r2 = metrics(out2, gt_48)
  r3 = metrics(out3, gt_96)
  
  r4 = metrics(out4, gt_12)
  r5 = metrics(out5, gt_48)
  r6 = metrics(out6, gt_96)
  
  
  mse = c(r1$av_mse, r4$av_mse, r2$av_mse, r5$av_mse, r3$av_mse, r6$av_mse)
  se = c(r1$se, r4$se, r2$se, r5$se, r3$se, r6$se)

  F1 = c(r1$av_F1, r4$av_F1, r2$av_F1, r5$av_F1, r3$av_F1, r6$av_F1)
  F1_se = c(r1$F1_se, r4$F1_se, r2$F1_se, r5$F1_se, r3$F1_se, r6$F1_se)
  return(list(mse=mse, se=se, f1 = F1, f1_se = F1_se)) 
}
# low rank model --------------------------------------------------------------


P_vec = c(12,48,96)

alpha_list <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Parameters/alpha_list.RDS")
beta_list <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Parameters/beta_list.RDS")

res_lr = results_table_ridge(P_vec, 0.0628, alpha_list, beta_list)

res_lr
# save results ------------------------------------------------------------
setwd("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Ridge")
saveRDS(res_lr, "res_lowrank_ridge.RDS")

