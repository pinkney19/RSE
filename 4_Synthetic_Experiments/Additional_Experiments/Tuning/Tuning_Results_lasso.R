
# Results from Tuning Procedure -------------------------------------------
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

# low-rank  -----------------------------------------------------------------

# load alphas and betas
alpha_list <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Parameters/alpha_list.RDS")
beta_list <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Parameters/beta_list.RDS")

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


# 10 Trials

get_tuned_lambdas_low_rank_lasso = function(P_vec, alpha_list, beta_list, freq){

  # load storm results - 10 trials
  res1 <- readRDS("~/luna/RSE/Synthetic_Experiments/Tuning/low_rank/lasso/out1.RDS") #12
  res2 <- readRDS("~/luna/RSE/Synthetic_Experiments/Tuning/low_rank/lasso/out2.RDS") #48
  res3 <- readRDS("~/luna/RSE/Synthetic_Experiments/Tuning/low_rank/lasso/out3.RDS") #96
 
  
  # Recall structure of the files
  
  lambdas = logspace(log10(0.001),log10(10),100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  N_samp = length(data_seq)
  P_vec = c(12,48,96)
  
  # Get ground truth 
  

  ground_truth = get_ground_truth_lr(P_vec,freq, alpha_list, beta_list)
  
  gt_12 = map(ground_truth,1)
  gt_48 = map(ground_truth,2)
  gt_96 = map(ground_truth,3)
  
  n.trials = 10
  
  l12 = lam_func(lambdas, res1, N_samp, gt_12,12, n.trials)
  l48 = lam_func(lambdas, res2, N_samp, gt_48, 48, n.trials)
  l96 = lam_func(lambdas, res3, N_samp, gt_96, 96, n.trials)
  
  ma_lams = rbind(l12, l48, l96)
  ma_lams
  # 50 trials 
  res4 <- readRDS("~/luna/RSE/Synthetic_Experiments/Tuning/low_rank/lasso/out4.RDS") #12
  res5 <- readRDS("~/luna/RSE/Synthetic_Experiments/Tuning/low_rank/lasso/out5.RDS") #48
  res6 <- readRDS("~/luna/RSE/Synthetic_Experiments/Tuning/low_rank/lasso/out6.RDS") #96
  
  n.trials = 50
  l12_50 = lam_func(lambdas, res4, N_samp, gt_12,12, n.trials)
  l48_50 = lam_func(lambdas, res5, N_samp, gt_48, 48, n.trials)
  l96_50 = lam_func(lambdas, res6, N_samp, gt_96, 96, n.trials)
  
  model_A_lambdas = rbind(l12[1:3], l12_50[1:3], l48[1:3], l48_50[1:3], l96[1:3], l96_50[1:3])
  model_A_lambdas = as.data.frame(model_A_lambdas)
  
  return(model_A_lambdas)
}


lams_lr = get_tuned_lambdas_low_rank_lasso(P_vec, alpha_list, beta_list, freq=0.0628)

lams_lr

setwd("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Tuning")
saveRDS(lams_lr, "lasso_lambdas.RDS")
