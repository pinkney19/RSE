
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


# Load results ------------------------------------------------------------


res1 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/10_trials/res1.RDS")
res2 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/10_trials/res2.RDS")
res3 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/10_trials/res3.RDS")
res4 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/10_trials/res4.RDS")
res5 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/10_trials/res5.RDS")
res6 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/10_trials/res6.RDS")
res7 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/10_trials/res7.RDS")
res8 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/10_trials/res8.RDS")
res9 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/10_trials/res9.RDS")


# Model a -----------------------------------------------------------------

# get ground truth
P_vec = c(12,48,96)
ground_truth = Get_ground_truth(P_vec, 1, 0.0628)

gt_12 = map(ground_truth,1)
gt_48 = map(ground_truth,2)
gt_96 = map(ground_truth,3)

library(pROC)

# auroc <- function(true_matrix, estimated_matrix) {
#   # vectorise matrices
#   true_values <- as.vector(true_matrix)
#   estimated_values <- as.vector(estimated_matrix)
#   
#   # Calculate AUROC
#   roc_obj <- roc(true_values, estimated_values)
#   auc(roc_obj)
# }

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

metrics(res1, gt_12)
metrics(res2, gt_48)
metrics(res3, gt_96)

# model b

ground_truth = Get_ground_truth(P_vec, 2, 0.0628)

gt_12 = map(ground_truth,1)
gt_48 = map(ground_truth,2)
gt_96 = map(ground_truth,3)


metrics(res4, gt_12)
metrics(res5, gt_48)
metrics(res6, gt_96)

# model c
ground_truth = Get_ground_truth(P_vec, 3, 0.0628)

gt_12 = map(ground_truth,1)
gt_48 = map(ground_truth,2)
gt_96 = map(ground_truth,3)


metrics(res7, gt_12)
metrics(res8, gt_48)
metrics(res9, gt_96)


# 50 trials ---------------------------------------------------------------

res1 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res1.RDS")
res2 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res2.RDS")
res3 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res3.RDS")
res4 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res4.RDS")
res5 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res5.RDS")
res6 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res6.RDS")
res7 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res7.RDS")
res8 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res8.RDS")
res9 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Graphical/Simulations/50_trials/res9.RDS")

# Model a -----------------------------------------------------------------

# get ground truth
P_vec = c(12,48,96)
ground_truth = Get_ground_truth(P_vec, 1, 0.0628)
gt_12 = map(ground_truth,1)
gt_48 = map(ground_truth,2)
gt_96 = map(ground_truth,3)
metrics(res1, gt_12)
metrics(res2, gt_48)
metrics(res3, gt_96)


# model b -----------------------------------------------------------------

ground_truth = Get_ground_truth(P_vec, 2, 0.0628)
gt_12 = map(ground_truth,1)
gt_48 = map(ground_truth,2)
gt_96 = map(ground_truth,3)
metrics(res4, gt_12)
metrics(res5, gt_48)
metrics(res6, gt_96)


# model c -----------------------------------------------------------------


ground_truth = Get_ground_truth(P_vec, 3, 0.0628)
gt_12 = map(ground_truth,1)
gt_48 = map(ground_truth,2)
gt_96 = map(ground_truth,3)
metrics(res7, gt_12)
metrics(res8, gt_48)
metrics(res9, gt_96)


# AUROC -------------------------------------------------------------------


