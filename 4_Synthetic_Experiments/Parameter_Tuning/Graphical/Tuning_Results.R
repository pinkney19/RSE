
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

# Model a -----------------------------------------------------------------

# 10 Trials

get_tuned_lambdas = function(model_type, model_idx){

  res1 <- readRDS(paste0("~/luna/RSE/Synthetic_Experiments/Tuning/Model_",model_type,"/out1.RDS")) #12
  res2 <- readRDS(paste0("~/luna/RSE/Synthetic_Experiments/Tuning/Model_",model_type,"/out2.RDS")) #48
  res3 <- readRDS(paste0("~/luna/RSE/Synthetic_Experiments/Tuning/Model_",model_type,"/out3.RDS")) #96
 
  
  # Recall structure of the files
  
  lambdas = logspace(log10(0.001),log10(10),100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  N_samp = length(data_seq)
  P_vec = c(12,48,96)
  
  # Get ground truth 
  
  ground_truth = Get_ground_truth(P_vec, model_idx, 0.0628)
  
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
  res4 <- readRDS(paste0("~/luna/RSE/Synthetic_Experiments/Tuning/Model_",model_type,"/out4.RDS")) #12
  res5 <- readRDS(paste0("~/luna/RSE/Synthetic_Experiments/Tuning/Model_",model_type,"/out5.RDS")) #48
  #res6 <- readRDS(paste0("~/luna/RSE/Synthetic_Experiments/Tuning/Model_",model_type,"/out6.RDS")) #96
  
  n.trials = 50
  l12_50 = lam_func(lambdas, res4, N_samp, gt_12,12, n.trials)
  l48_50 = lam_func(lambdas, res5, N_samp, gt_48, 48, n.trials)
  #l96_50 = lam_func(lambdas, res6, N_samp, gt_96, 96, n.trials)
  
  model_A_lambdas = rbind(l12[1:3], l12_50[1:3], l48[1:3], l48_50[1:3], l96[1:3])#, l96_50[1:3])
  model_A_lambdas = as.data.frame(model_A_lambdas)
  
  return(model_A_lambdas)
}


lams_A = get_tuned_lambdas("A", 1)
lams_B = get_tuned_lambdas("B", 2)
lams_C =  get_tuned_lambdas("C", 2)




# 
# 
# # Model b -----------------------------------------------------------------
# 
# res4 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/model_b/res1.RDS")
# res5 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/model_b/res2.RDS")
# res6 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/model_b/res3.RDS")
# 
# 
# ground_truth = Get_ground_truth(P_vec, 2, 0.0628)
# 
# gt_12 = map(ground_truth,1)
# gt_48 = map(ground_truth,2)
# gt_96 = map(ground_truth,3)
# 
# l12 = lam_func(lambdas, res4, N_samp, gt_12,12, 10)
# l48 = lam_func(lambdas, res5, N_samp, gt_48, 48, 10)
# l96 = lam_func(lambdas, res6, N_samp, gt_96, 96, 10)
# 
# mb_lams = rbind(l12, l48, l96)
# 
# 
# # Model c -----------------------------------------------------------------
# 
# res7 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/model_c/res1.RDS")
# res8 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/model_c/res2.RDS")
# res9 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/model_c/res3.RDS")
# 
# 
# ground_truth = Get_ground_truth(P_vec, 3, 0.0628)
# 
# gt_12 = map(ground_truth,1)
# gt_48 = map(ground_truth,2)
# gt_96 = map(ground_truth,3)
# 
# l12 = lam_func(lambdas, res7, N_samp, gt_12,12, 10)
# l48 = lam_func(lambdas, res8, N_samp, gt_48, 48, 10)
# l96 = lam_func(lambdas, res9, N_samp, gt_96, 96, 10)
# 
# mc_lams = rbind(l12, l48, l96)
# 
# 
# # Print results -----------------------------------------------------------
# 
# print(ma_lams)
# print(mb_lams)
# print(mc_lams)
