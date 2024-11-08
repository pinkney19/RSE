
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
  
  return(list(F1 = F1, av_mse = av_mse, av_AUC = auc_obj, se = se, f1_se = F1_se))
}
results_table = function(P_vec, model_idx, model_type){
  
  # get ground truth
  ground_truth = Get_ground_truth(P_vec, model_idx, 0.0628)
  
  gt_12 = map(ground_truth,1)
  gt_48 = map(ground_truth,2)
  gt_96 = map(ground_truth,3)
  
  # eBIC results 
  
  # load data
  # 10 trials 
  out1 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/eBIC/out1.RDS")) #p=12
  out2 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/eBIC/out2.RDS")) #p=48
  out3 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/eBIC/out3.RDS")) #p=96
  # 50 trials
  out4 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/eBIC/out4.RDS")) #p=12
  out5 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/eBIC/out5.RDS")) #p=48
  out6 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/eBIC/out6.RDS")) #p=96
  
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
  out1 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/mse/out1.RDS")) #p=12
  out2 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/mse/out2.RDS")) #p=48
  out3 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/mse/out3.RDS")) #p=96
  # 50 trials
  out4 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/mse/out4.RDS")) #p=12
  out5 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/mse/out5.RDS")) #p=48
  out6 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/mse/out6.RDS")) #p=96
  
  
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
  # f1 results --------------------------------------------------------------
  
  # load data
  # 10 trials 
  out1 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/f1/out1.RDS")) #p=12
  out2 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/f1/out2.RDS")) #p=48
  out3 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/f1/out3.RDS")) #p=96
  # 50 trials
  out4 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/f1/out4.RDS")) #p=12
  out5 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/f1/out5.RDS")) #p=48
  out6 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/Model_", model_type, "/Results/f1/out6.RDS")) #p=96
  
  
  r1 = metrics(out1, gt_12)
  r2 = metrics(out2, gt_48)
  r3 = metrics(out3, gt_96)
  
  r4 = metrics(out4, gt_12)
  r5 = metrics(out5, gt_48)
  r6 = metrics(out6, gt_96)
  
  
  mses_f1 = c(r1$av_mse, r4$av_mse, r2$av_mse, r5$av_mse, r3$av_mse, r6$av_mse)
  f1s_f1 = c(r1$F1, r4$F1, r2$F1, r5$F1, r3$F1, r6$F1)
  auroc_f1 = c(r1$av_AUC, r4$av_AUC, r2$av_AUC, r5$av_AUC, r3$av_AUC, r6$av_AUC)
  ses_f1 = c(r1$se, r4$se, r2$se, r5$se, r3$se, r6$se)
  f1_ses_f1 = c(r1$f1_se, r4$f1_se, r2$f1_se, r5$f1_se, r3$f1_se, r6$f1_se)
  
  # results tables
  tab_mse = cbind(mses_mse, mses_f1, mses_ebic)
  tab_f1 = cbind(f1s_mse, f1s_f1, f1s_ebic)
  tab_auroc = cbind(auroc_mse, auroc_f1, auroc_ebic)
  tab_ses = cbind(ses_mse, ses_f1, ses_ebic)
  tab_f1_ses = cbind(f1_ses_mse, f1_ses_f1, f1_ses_ebic)
  
  return(list(tab_mse = tab_mse, tab_f1 = tab_f1, tab_auroc=tab_auroc, tab_ses = tab_ses, tab_f1_ses = tab_f1_ses)) 
}
# Model a -----------------------------------------------------------------


P_vec = c(12,48,96)

res_a = results_table(P_vec, 1, "A")
res_b = results_table(P_vec,2, "B")
res_c = results_table(P_vec,3, "C")

# save results ------------------------------------------------------------


setwd("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical")
saveRDS(res_a, "res_a.RDS")
saveRDS(res_b, "res_b.RDS")
saveRDS(res_c, "res_c.RDS")
