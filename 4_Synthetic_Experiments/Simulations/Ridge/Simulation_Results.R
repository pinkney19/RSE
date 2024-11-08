
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
  
  return(list(av_mse = av_mse, se = se))
}
results_table_ridge = function(P_vec, model_idx, model_type){
  
  # get ground truth
  ground_truth = Get_ground_truth(P_vec, model_idx, 0.0628)
  
  gt_12 = map(ground_truth,1)
  gt_48 = map(ground_truth,2)
  gt_96 = map(ground_truth,3)
  
 
  # mse results -------------------------------------------------------------
  
  # load data
  # 10 trials 
  out1 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Ridge/Model_", model_type, "/Results/mse/out1.RDS")) #p=12
  out2 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Ridge/Model_", model_type, "/Results/mse/out2.RDS")) #p=48
  out3 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Ridge/Model_", model_type, "/Results/mse/out3.RDS")) #p=96
  # 50 trials
  out4 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Ridge/Model_", model_type, "/Results/mse/out4.RDS")) #p=12
  out5 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Ridge/Model_", model_type, "/Results/mse/out5.RDS")) #p=48
  out6 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Ridge/Model_", model_type, "/Results/mse/out6.RDS")) #p=96
  
  
  r1 = metrics(out1, gt_12)
  r2 = metrics(out2, gt_48)
  r3 = metrics(out3, gt_96)
  
  r4 = metrics(out4, gt_12)
  r5 = metrics(out5, gt_48)
  r6 = metrics(out6, gt_96)
  
  
  mse = c(r1$av_mse, r4$av_mse, r2$av_mse, r5$av_mse, r3$av_mse, r6$av_mse)
  se = c(r1$se, r4$se, r2$se, r5$se, r3$se, r6$se)

  
  return(list(mse=mse, se=se)) 
}
# Model a -----------------------------------------------------------------


P_vec = c(12,48,96)

res_a = results_table_ridge(P_vec, 1, "A")
res_b = results_table_ridge(P_vec,2, "B")
res_c = results_table_ridge(P_vec,3, "C")

# save results ------------------------------------------------------------


setwd("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Ridge")
saveRDS(res_a, "res_a.RDS")
saveRDS(res_b, "res_b.RDS")
saveRDS(res_c, "res_c.RDS")
