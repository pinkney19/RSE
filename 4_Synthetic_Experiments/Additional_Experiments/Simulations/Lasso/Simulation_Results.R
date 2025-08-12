
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

# Hawkes parameters -------------------------------------------------------

P_vec = c(12,48,96)
alpha_list <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Parameters/alpha_list.RDS")
beta_list <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Parameters/beta_list.RDS")

# Required functions -------------------------------------------------------

metrics = function(res, gt){
  
  theta_list = map(res, 1)
  z_list = map(res, 2)
  r = map(res, 3)
  av_mse = mean( unlist(lapply(theta_list, function(x) performance_measures(x, gt$theta, F)$l2)) )
  # standard errors
  se = std_err(unlist(lapply(theta_list, function(x) performance_measures(x, gt$theta, F)$l2)) )
  
  pc_list = lapply(z_list, partial_co)
  F1 = mean(unlist(lapply(pc_list, function(x) performance_measures(x, gt$pc, T)$F1)))
  F1_se = std_err(unlist(lapply(pc_list, function(x) performance_measures(x, gt$pc, T)$F1)))
  
  # vectorise matrices for AUROC
  true_pc = as.vector(gt$pc)
  # since all 1s -> make factor
  est_pc_vec = lapply(pc_list, as.vector)
  
  # make into 0 1 vectors
  true_pc[true_pc!=0]=1
  for(i in 1:length(est_pc_vec)){
    est_pc_vec[[i]][est_pc_vec[[i]]!=0]=1
    
  }
  
  # Check if the response has more than two levels
  # if (length(unique(true_pc)) > 2) {
  #   
  #   roc_obj = lapply(est_pc_vec, function(x){multiclass.roc(true_pc, x, direction="<")})
  #   auc_list = lapply(roc_obj, auc)
  # } else {
  # Use roc for binary classification
  #roc_obj = lapply(est_pc_vec, function(x){roc(true_pc, x, direction="<")})
  #auc_list = lapply(roc_obj, auc)
  
  # doesnt make sense to use AUROC here as a metric -> consider F1 score instead!
  # Note: in non-sparse case it doesnt make sense to calculate AUROC
  #auc_obj = mean(unlist(auc_list))
  
  return(list(F1 = F1, av_mse = av_mse, se = se, f1_se = F1_se))
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

results_table_add = function(P_vec, freq, alpha_list, beta_list){
  
  # get ground truth
  ground_truth = get_ground_truth_lr(P_vec, freq, alpha_list, beta_list)
  
  gt_12 = map(ground_truth,1)
  gt_48 = map(ground_truth,2)
  gt_96 = map(ground_truth,3)
  
  # eBIC results 
  
  # load data
  # 10 trials 
  out1 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Lasso/results_ebic/out1.RDS") #p=12
  out2 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Lasso/results_ebic/out2.RDS") #p=48
  out3 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Lasso/results_ebic/out3.RDS") #p=96
  
  # 50 trials
  out4 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Lasso/results_ebic/out4.RDS") #p=12
  out5 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Lasso/results_ebic/out5.RDS") #p=48
  out6 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Lasso/results_ebic/out6.RDS") #p=96
  
  r1 = metrics(out1, gt_12)
  r2 = metrics(out2, gt_48)
  r3 = metrics(out3, gt_96)
  
  r4 = metrics(out4, gt_12)
  r5 = metrics(out5, gt_48)
  r6 = metrics(out6, gt_96)
  
  
  mses_ebic = c(r1$av_mse, r4$av_mse, r2$av_mse, r5$av_mse, r3$av_mse, r6$av_mse)
  f1s_ebic = c(r1$F1, r4$F1, r2$F1, r5$F1, r3$F1, r6$F1)
  auroc_ebic = c(r1$av_AUC, r4$av_AUC, r2$av_AUC, r5$av_AUC, r3$av_AUC, r6$av_AUC)
  ses_ebic = c(r1$se, r4$se, r2$se, r5$se, r3$se, r6$se)
  f1_ses_ebic = c(r1$f1_se, r4$f1_se, r2$f1_se, r5$f1_se, r3$f1_se, r6$f1_se)
  
  # mse results -------------------------------------------------------------
  
  # load data
  # 10 trials 
  out1 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Lasso/results_mse/out1.RDS") #p=12
  out2 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Lasso/results_mse/out2.RDS") #p=48
  out3 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Lasso/results_mse/out3.RDS") #p=96

  # 50 trials
  out4 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Lasso/results_mse/out4.RDS") #p=12
  out5 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Lasso/results_mse/out5.RDS") #p=48
  out6 <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Lasso/results_mse/out6.RDS") #p=96
  
  
  r1 = metrics(out1, gt_12)
  r2 = metrics(out2, gt_48)
  r3 = metrics(out3, gt_96)
  
  r4 = metrics(out4, gt_12)
  r5 = metrics(out5, gt_48)
  r6 = metrics(out6, gt_96)
  
  
  mses_mse = c(r1$av_mse, r4$av_mse, r2$av_mse, r5$av_mse, r3$av_mse, r6$av_mse)
  f1s_mse = c(r1$F1, r4$F1, r2$F1, r5$F1, r3$F1, r6$F1)
  auroc_mse = c(r1$av_AUC, r4$av_AUC, r2$av_AUC, r5$av_AUC, r3$av_AUC, r6$av_AUC)
  ses_mse = c(r1$se, r4$se, r2$se, r5$se, r3$se, r6$se)
  f1_ses_mse = c(r1$f1_se, r4$f1_se, r2$f1_se, r5$f1_se, r3$f1_se, r6$f1_se)
  
  # results tables
  tab_mse = cbind(mses_mse, mses_ebic)
  tab_f1 = cbind(f1s_mse, f1s_ebic)
  tab_auroc = cbind(auroc_mse, auroc_ebic)
  tab_ses = cbind(ses_mse, ses_ebic)
  tab_f1_ses = cbind(f1_ses_mse, f1_ses_ebic)
  
  return(list(tab_mse = tab_mse, tab_f1 = tab_f1, tab_ses = tab_ses, tab_f1_ses = tab_f1_ses, tab_auroc = tab_auroc)) 
}

# Low rank model  ----------------------------------------------------------------


P_vec = c(12,48,96)

res = results_table_add(P_vec, freq=0.0628, alpha_list, beta_list)

# save results ------------------------------------------------------------
setwd("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Lasso")
saveRDS(res, "res_low_rank_lasso.RDS")
