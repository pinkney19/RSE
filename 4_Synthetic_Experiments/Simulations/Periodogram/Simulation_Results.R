
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

# dimensions
P_vec = c(12,48,96)

# Required functions -------------------------------------------------------

metrics = function(res, gt){
  
  av_mse = mean( unlist(lapply(res, function(x) performance_measures(x, gt$theta, F)$l2)) )
  se = std_err(unlist(lapply(res, function(x) performance_measures(x, gt$theta, F)$l2)))
  
  return(list(av_mse = av_mse, se = se))
}

# get ground truth
ground_truth_a = Get_ground_truth(P_vec, 1, 0.0628)
ground_truth_b = Get_ground_truth(P_vec, 2, 0.0628)
ground_truth_c = Get_ground_truth(P_vec, 3, 0.0628)

gta_12 = map(ground_truth_a,1)
gta_48 = map(ground_truth_a,2)

gtb_12 = map(ground_truth_b,1)
gtb_48 = map(ground_truth_b,2)

gtc_12 = map(ground_truth_c,1)
gtc_48 = map(ground_truth_c,2)


# load data - all 50 trials
# model a
out1 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Periodogram/Results/out1.RDS")) #p=12
out2 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Periodogram/Results/out2.RDS")) #p=48
# model b
out3 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Periodogram/Results/out3.RDS")) #p=12
out4 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Periodogram/Results/out4.RDS")) #p=48
# model c
out5 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Periodogram/Results/out5.RDS")) #p=12
out6 <- readRDS(paste0("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Periodogram/Results/out6.RDS")) #p=48


r1 = metrics(out1, gta_12)
r2 = metrics(out2, gta_48)

r3 = metrics(out3, gtb_12)
r4 = metrics(out4, gtb_48)

r5 = metrics(out5, gtc_12)
r6 = metrics(out6, gtc_48)


mse = c(r1$av_mse, r2$av_mse, r3$av_mse, r4$av_mse, r5$av_mse, r6$av_mse)
se = c(r1$se, r2$se, r3$se, r4$se, r5$se, r6$se)

saveRDS(mse, "res_periodogram.RDS")
