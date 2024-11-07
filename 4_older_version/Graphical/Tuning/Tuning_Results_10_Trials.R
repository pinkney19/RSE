
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

res1 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/model_a/res1.RDS")
res2 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/model_a/res2.RDS")
res3 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/model_a/res3.RDS")

# Recall structure of the files

lambdas = logspace(-5,1,100)
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


l12 = lam_func(lambdas, res1, N_samp, gt_12,12, 10)
l48 = lam_func(lambdas, res2, N_samp, gt_48, 48, 10)
l96 = lam_func(lambdas, res3, N_samp, gt_96, 96, 10)

ma_lams = rbind(l12, l48, l96)

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
