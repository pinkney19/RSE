
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

res1 <- readRDS("~/luna/RSE/Synthetic_Experiments/Tuning/Model_A/out1.RDS") #12
res2 <- readRDS("~/luna/RSE/Synthetic_Experiments/Tuning/Model_A/out2.RDS") #48 
res3 <- readRDS("~/luna/RSE/Synthetic_Experiments/Tuning/Model_A/out3.RDS") #96

# Recall structure of the files

lambdas = logspace(log10(0.001),log10(10),100)
data_seq = seq(1, 10)
grid = expand.grid(sample = data_seq, lams = lambdas)
list_of_pairs <- split(grid, seq(nrow(grid)))
N_samp = length(data_seq)
P_vec = c(12,48,96)

# Get ground truth --------------------------------------------------------

ground_truth = Get_ground_truth(P_vec, 1, 0.0628)

gt_12 = map(ground_truth,1)
gt_48 = map(ground_truth,2)
gt_96 = map(ground_truth,3)

n.trials = 10


# for now -----------------------------------------------------------------


lam_func3 = function(lambdas, out_storm, N_samp, gt, p, n.trials){
  av_mse = NULL; l2 = NULL; l2 = rep(list(l2), length(lambdas));
  F1 = NULL; F1 = rep(list(F1), length(lambdas))
  ebic = list(); ebic = rep(list(ebic), length(lambdas))
  for(i in 1:length(lambdas)){
    b = N_samp*i
    if(i==1){
      res = out_storm[1:b] #i.e. samples 1-10 for lambda = 0.1
    }
    if(i!=1){
      res = out_storm[((b-N_samp)+1):b]
    }
    
    theta_list = map(res, 1)
    z_list = map(res, 2)
    #S_list = map(res, 3)
    l2[[i]] = unlist(lapply(theta_list, function(x) performance_measures(x, gt$theta, F)$l2))
    av_mse[i] = mean(l2[[i]])
    
    pc_list = lapply(z_list, partial_co)
    F1[[i]] = lapply(pc_list, function(x) performance_measures(x, gt$pc, T)$F1)
    
    # for(j in 1:length(theta_list)){
    #   ebic[[i]][j] = eBIC_check(theta_list[[j]], S_list[[j]], z_list[[j]], 0.5, p, n.trials)
    # }
    
  }
  
  lam1 = lambdas[which.min(av_mse)]
  idx = NULL;idx2 = NULL; idx3 = NULL;
  e = list(); e = rep(list(e), N_samp)
  for(j in 1:N_samp){
    l = unlist(map(l2,j))
    idx[j]= which.min(l)
    
   # e[[j]] = unlist(map(ebic,j))
  #  idx2[j] = which.min(Re(e[[j]]))
    
    f = unlist(map(F1,j))
    idx3[j] = which.max(f)
  }
  lam_mse = mean(lambdas[idx])
  #lam_ebic = mean(lambdas[idx2])
  lam_f1 = mean(lambdas[idx3])
  
  return(list(lam_mse = lam_mse, lam_f1 =  lam_f1))#, #l_ebic = lam_ebic, ebic= ebic))
}

l12 = lam_func3(lambdas, res1, N_samp, gt_12,12, n.trials)
l48 = lam_func3(lambdas, res2, N_samp, gt_48, 48, n.trials)
l96 = lam_func3(lambdas, res3, N_samp, gt_96, 96, n.trials)

ma_lams = rbind(l12, l48, l96)

# 50 trials 
res4 <- readRDS("~/luna/RSE/Synthetic_Experiments/Tuning/Model_A/out4.RDS") #12
res5 <- readRDS("~/luna/RSE/Synthetic_Experiments/Tuning/Model_A/out5.RDS") #48 

n.trials = 50
l12 = lam_func3(lambdas, res4, N_samp, gt_12,12, n.trials)
l48 = lam_func3(lambdas, res5, N_samp, gt_48, 48, n.trials)

rbind(l12, l48)
# model b -----------------------------------------------------------------

# Model b -----------------------------------------------------------------

res4 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/model_b/res1.RDS")
res5 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/model_b/res2.RDS")
res6 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/model_b/res3.RDS")


ground_truth = Get_ground_truth(P_vec, 2, 0.0628)

gt_12 = map(ground_truth,1)
gt_48 = map(ground_truth,2)
gt_96 = map(ground_truth,3)

l12 = lam_func(lambdas, res4, N_samp, gt_12,12, 10)
l48 = lam_func(lambdas, res5, N_samp, gt_48, 48, 10)
l96 = lam_func(lambdas, res6, N_samp, gt_96, 96, 10)

mb_lams = rbind(l12, l48, l96)


# Model c -----------------------------------------------------------------

res7 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/model_c/res1.RDS")
res8 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/model_c/res2.RDS")
res9 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/model_c/res3.RDS")


ground_truth = Get_ground_truth(P_vec, 3, 0.0628)

gt_12 = map(ground_truth,1)
gt_48 = map(ground_truth,2)
gt_96 = map(ground_truth,3)

l12 = lam_func(lambdas, res7, N_samp, gt_12,12, 10)
l48 = lam_func(lambdas, res8, N_samp, gt_48, 48, 10)
l96 = lam_func(lambdas, res9, N_samp, gt_96, 96, 10)

mc_lams = rbind(l12, l48, l96)


# Print results -----------------------------------------------------------

print(ma_lams)
print(mb_lams)
print(mc_lams)
