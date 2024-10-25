
# Results from Tuning Procedure -------------------------------------------
setwd("~/Downloads/RSE/4_Synthetic_Experiments")
source("Functions_Section_4.R")
# Model a -----------------------------------------------------------------

res1 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/res1.RDS")
res2 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/res2.RDS")
res3 <- readRDS("~/luna/Paper_Code/Section 4/Reviews/tuning/res3.RDS")

# Recall structure of the files
library(pracma) #for logspace
lambdas = logspace(-5,1,100)
data_seq = seq(1, 10)
grid = expand.grid(sample = data_seq, lams = lambdas)
list_of_pairs <- split(grid, seq(nrow(grid)))

N_samp = length(data_seq)


# Get ground truth --------------------------------------------------------
P_vec = c(12,48,96)
Get_ground_truth = function(P_vec, model_type, freq){
  params = Get_model_parameters(model_type, P_vec); 
  
  true_theta = list(); true_theta = rep(list(true_theta), length(P_vec));
  true_s = list(); true_s = rep(list(true_s), length(P_vec))
  true_pc = list(); true_pc = rep(list(true_pc), length(P_vec))
  for(i in 1:length(P_vec)){
    HD_A = params$HD_A_list[[i]]; HD_B = params$HD_B_list[[i]]
    nu = params$nu_list[[i]]
    gt = MV_spectra(freq, HD_A, HD_B, nu, P_vec[i])
    true_s[[i]] = gt$spectra[[1]]
    true_pc[[i]] = gt$pc[[1]]
    true_theta[[i]] = gt$inv[[1]]
  }
  return(list(theta=true_theta, s = true_s, pc = true_pc))
}

ground_truth = Get_ground_truth(P_vec, 1, 0.0628)

gt_12 = map(ground_truth,1)
gt_48 = map(ground_truth,2)
gt_96 = map(ground_truth,3)

eBIC_check = function(theta, S, z, gam, n.stream, n.trials){
  #theta -    estimate for theta
  #S -        estimator of SDM -> either TA or pooled TA
  #z -        sparse output from ADMM
  #n.stream   no. data streams/dimensionality.
  
  lik = -log(Det(theta))+tr(S%*%theta)
  u = z[upper.tri(z, diag = F)]
  edges = length(u[u!=0])
  
  op = 2*n.trials*lik +  (edges *log(n.trials)) + 4*edges*gam*log(n.stream)
  
  return(op)
} 

lam_func = function(lambdas, out_storm, N_samp, gt, p, n.trials){
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
    S_list = map(res, 3)
    l2[[i]] = unlist(lapply(theta_list, function(x) performance_measures(x, gt$theta, F)$l2))
    av_mse[i] = mean(l2[[i]])
    
    pc_list = lapply(z_list, partial_co)
    F1[[i]] = lapply(pc_list, function(x) performance_measures(x, gt$pc, T)$F1)
    
    for(j in 1:length(theta_list)){
      ebic[[i]][j] = eBIC_check(theta_list[[j]], S_list[[j]], z_list[[j]], 0.5, p, n.trials)
    }
    
  }
  
  lam1 = lambdas[which.min(av_mse)]
  idx = NULL;idx2 = NULL; idx3 = NULL;
  e = list(); e = rep(list(e), N_samp)
  for(j in 1:N_samp){
    l = unlist(map(l2,j))
    idx[j]= which.min(l)
    
    e[[j]] = unlist(map(ebic,j))
    idx2[j] = which.min(Re(e[[j]]))
    
    f = unlist(map(F1,j))
    idx3[j] = which.max(f)
  }
  lam_mse = mean(lambdas[idx])
  lam_ebic = mean(lambdas[idx2])
  lam_f1 = mean(lambdas[idx3])
  
  return(list(lam_mse = lam_mse, lam_f1 =  lam_f1, l_ebic = lam_ebic))
}

l12 = lam_func(lambdas, res1, N_samp, gt_12,12, 10)
l48 = lam_func(lambdas, res2, N_samp, gt_48, 48, 10)
l96 = lam_func(lambdas, res3, N_samp, gt_96, 96, 10)

rbind(l12, l48, l96)



