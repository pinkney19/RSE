
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


# Require functions -------------------------------------------------------

metrics = function(res, gt){
  
  theta_list = map(res, 1)
  z_list = map(res, 2)
  r = map(res, 3)
  av_mse = mean( unlist(lapply(theta_list, function(x) performance_measures(x, gt$theta, F)$l2)) )
  
  pc_list = lapply(z_list, partial_co)
  F1 = mean(unlist(lapply(pc_list, function(x) performance_measures(x, gt$pc, T)$F1)))
  
  # vectorise matrices for AUROC
  true_pc = as.vector(gt$pc)
  est_pc_vec = lapply(pc_list, as.vector)
  
  # Check if the response has more than two levels
  if (length(unique(true_pc)) > 2) {
    
    roc_obj = lapply(est_pc_vec, function(x){multiclass.roc(true_pc, x, direction="<")})
    auc_list = lapply(roc_obj, auc)
  } else {
    # Use roc for binary classification
    roc_obj = lapply(est_pc_vec, function(x){roc(true_pc, x, direction="<")})
    auc_list = lapply(roc_obj, auc)
  }
  
  auc_obj = mean(unlist(auc_list))
  
  return(list(F1 = F1, av_mse = av_mse, av_AUC = auc_obj))
}

# Model a -----------------------------------------------------------------

# Get ground truth
P_vec = c(12,48,96)
ground_truth = Get_ground_truth(P_vec, 1, 0.0628)

gt_12 = map(ground_truth,1)
gt_48 = map(ground_truth,2)
gt_96 = map(ground_truth,3)

# eBIC results ------------------------------------------------------------

# load data
# 10 trials 
out1 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/eBIC/out1.RDS") #p=12
out2 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/eBIC/out2.RDS") #p=48
out3 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/eBIC/out3.RDS") #p=96
# 50 trials
out4 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/eBIC/out4.RDS") #p=12
out5 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/eBIC/out5.RDS") #p=48
out6 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/eBIC/out6.RDS") #p=96

r1 = metrics(out1, gt_12)
r2 = metrics(out2, gt_48)
r3 = metrics(out3, gt_96)

r4 = metrics(out4, gt_12)
r5 = metrics(out5, gt_48)
r6 = metrics(out6, gt_96)


mses_ebic = c(r1$av_mse, r4$av_mse, r2$av_mse, r5$av_mse, r3$av_mse, r6$av_mse)
f1s_ebic = c(r1$F1, r4$F1, r2$F1, r5$F1, r3$F1, r6$F1)
auroc_ebic = c(r1$av_AUC, r4$av_AUC, r2$av_AUC, r5$av_AUC, r3$av_AUC, r6$av_AUC)

# mse results -------------------------------------------------------------

# load data
# 10 trials 
out1 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/mse/out1.RDS") #p=12
out2 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/mse/out2.RDS") #p=48
out3 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/mse/out3.RDS") #p=96
# 50 trials
out4 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/mse/out4.RDS") #p=12
out5 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/mse/out5.RDS") #p=48
out6 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/mse/out6.RDS") #p=96


r1 = metrics(out1, gt_12)
r2 = metrics(out2, gt_48)
r3 = metrics(out3, gt_96)

r4 = metrics(out4, gt_12)
r5 = metrics(out5, gt_48)
r6 = metrics(out6, gt_96)


mses_mse = c(r1$av_mse, r4$av_mse, r2$av_mse, r5$av_mse, r3$av_mse, r6$av_mse)
f1s_mse = c(r1$F1, r4$F1, r2$F1, r5$F1, r3$F1, r6$F1)
auroc_mse = c(r1$av_AUC, r4$av_AUC, r2$av_AUC, r5$av_AUC, r3$av_AUC, r6$av_AUC)


# f1 results --------------------------------------------------------------

# load data
# 10 trials 
out1 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/f1/out1.RDS") #p=12
out2 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/f1/out2.RDS") #p=48
out3 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/f1/out3.RDS") #p=96
# 50 trials
out4 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/f1/out4.RDS") #p=12
out5 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/f1/out5.RDS") #p=48
out6 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_A/Results/f1/out6.RDS") #p=96


r1 = metrics(out1, gt_12)
r2 = metrics(out2, gt_48)
r3 = metrics(out3, gt_96)

r4 = metrics(out4, gt_12)
r5 = metrics(out5, gt_48)
r6 = metrics(out6, gt_96)


mses_11 = c(r1$av_mse, r4$av_mse, r2$av_mse, r5$av_mse, r3$av_mse, r6$av_mse)
f1s_f1 = c(r1$F1, r4$F1, r2$F1, r5$F1, r3$F1, r6$F1)
auroc_f1 = c(r1$av_AUC, r4$av_AUC, r2$av_AUC, r5$av_AUC, r3$av_AUC, r6$av_AUC)


# Model a results table ---------------------------------------------------

tab_mse = cbind(mses_mse, mses_11, mses_ebic)
tab_f1 = cbind(f1s_mse, f1s_f1, f1s_ebic)
tab_auroc = cbind(auroc_mse, auroc_f1, auroc_ebic)

# 
# 
# # model b
# 
# ground_truth = Get_ground_truth(P_vec, 2, 0.0628)
# 
# gt_12 = map(ground_truth,1)
# gt_48 = map(ground_truth,2)
# gt_96 = map(ground_truth,3)
# 
# 
# metrics(res4, gt_12)
# metrics(res5, gt_48)
# metrics(res6, gt_96)
# 
# # model c
# ground_truth = Get_ground_truth(P_vec, 3, 0.0628)
# 
# gt_12 = map(ground_truth,1)
# gt_48 = map(ground_truth,2)
# gt_96 = map(ground_truth,3)
# 
# 
# metrics(res7, gt_12)
# metrics(res8, gt_48)
# metrics(res9, gt_96)
# 
# 
# # 50 trials ---------------------------------------------------------------
# 
# res1 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res1.RDS")
# res2 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res2.RDS")
# res3 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res3.RDS")
# res4 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res4.RDS")
# res5 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res5.RDS")
# res6 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res6.RDS")
# res7 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res7.RDS")
# res8 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res8.RDS")
# res9 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res9.RDS")
# 
# # Model a -----------------------------------------------------------------
# 
# # get ground truth
# P_vec = c(12,48,96)
# ground_truth = Get_ground_truth(P_vec, 1, 0.0628)
# gt_12 = map(ground_truth,1)
# gt_48 = map(ground_truth,2)
# gt_96 = map(ground_truth,3)
# metrics(res1, gt_12)
# metrics(res2, gt_48)
# metrics(res3, gt_96)
# 
# 
# # model b -----------------------------------------------------------------
# 
# ground_truth = Get_ground_truth(P_vec, 2, 0.0628)
# gt_12 = map(ground_truth,1)
# gt_48 = map(ground_truth,2)
# gt_96 = map(ground_truth,3)
# metrics(res4, gt_12)
# metrics(res5, gt_48)
# metrics(res6, gt_96)
# 
# 
# # model c -----------------------------------------------------------------
# 
# 
# ground_truth = Get_ground_truth(P_vec, 3, 0.0628)
# gt_12 = map(ground_truth,1)
# gt_48 = map(ground_truth,2)
# gt_96 = map(ground_truth,3)
# metrics(res7, gt_12)
# metrics(res8, gt_48)
# metrics(res9, gt_96)
# 
# 
# # AUROC -------------------------------------------------------------------
# 
# 
