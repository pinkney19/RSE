library(Matrix)
library(phonTools) #for sinc function
library(QZ) #for H - conjugate transpose
library(purrr) #for map function
library(matrixStats) #for colQuantiles
library(complexplus) #for deterimnan of complex matrix
library(psych) #for trace
library(pracma) #for logspace
library(hawkes) #for simulating hawkes process via Ogata's method


setwd("~/Downloads/RSE/5_Neuronal_Synchronicity")
source("Functions_Section_5.R")

lambdas = logspace(log10(0.001), log10(10), 100)

# results for laser on condition ------------------------------------------

est1_0 <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Delta_band/Laser_on/est_0.RDS")
est1_10 <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Delta_band/Laser_on/est_10.RDS")
est1_50 <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Delta_band/Laser_on/est_50.RDS")

# results for laser off condition ------------------------------------------

est2_0 <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Delta_band/Laser_off/est_0.RDS")
est2_10 <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Delta_band/Laser_off/est_10.RDS")
est2_50 <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Delta_band/Laser_off/est_50.RDS")


res1 = calc_result(est1_0)
res2 = calc_result(est1_10)
res3 = calc_result(est1_50)

res4 = calc_result(est2_0)
res5 = calc_result(est2_10)
res6 = calc_result(est2_50)


# ebic curves -------------------------------------------------------------

plot_eBIC(list(res1$ebic, res2$ebic, res3$ebic), "On")
plot_eBIC(list(res4$ebic, res5$ebic, res6$ebic), "Off")


# ADMM criteria -----------------------------------------------------------

plot_ADMM(list(res1$r, res2$r, res3$r), "On")
plot_ADMM(list(res4$r, res5$r, res6$r), "Off")


# Partial coherence estimates ---------------------------------------------

# edges 
edges = function(mat){
  u = mat[upper.tri(mat, diag=F)]
  edges = length(u[u!=0])
  return(edges)
}

edges_list_on = c(edges(res1$pc), edges(res2$pc), edges(res3$pc))
edges_list_off = c(edges(res4$pc), edges(res5$pc), edges(res6$pc))


edges_list_on
edges_list_off


title_list = c("","","")

library(igraph)
zero = list(res4$pc, res1$pc)
zero_points = c(("AvgPoints = 874"), expression("AvgPoints = 284"))
plot_UGM_new(zero, title_list, zero_points)



library(igraph)
ten = list(res5$pc, res2$pc)
ten_points = c(("AvgPoints = 778"), expression("AvgPoints = 712"))
plot_UGM_new(ten, title_list ,ten_points)



library(igraph)
fifty = list(res6$pc, res3$pc)
fifty_points = c(("AvgPoints = 799"), expression("AvgPoints = 890"))
plot_UGM_new(fifty, title_list, fifty_points)


# Checks on no laser conditions -------------------------------------------

# check = list(res2$pc, res3$pc)
# check_points =  c(("AvgPoints = 712"), expression("AvgPoints = 890"))
# plot_UGM_new(check, title_list = c(expression(10~mW/mm^2), expression(50~mW/mm^2)), check_points)
# 
# 

