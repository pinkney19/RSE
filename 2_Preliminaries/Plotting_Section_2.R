# Plots for Section 2  ----------------------------------------------------

library(Matrix)
library(phonTools) 
library(QZ) 
library(purrr) 
library(matrixStats)
library(complexplus) 
library(psych) 
library(pracma) 
library(hawkes) 
library(hypergeo)

setwd("~/Downloads/RSE/2_Preliminaries")
source("Functions_Section_2.R")


# Fig1.a Plot -----------------------------------------------------------

R_hat <- readRDS("~/luna/Paper_Code/Section 2/Data/R_hat1.RDS")
m=10
plot_fig1_a(R_hat, m, 2, 0)


res = readRDS("sup_norm.RDS")
res_eig = readRDS("max_eig.RDS")
res_eig_min = readRDS("min_eig.RDS")

# Fig1.b and c Plot -----------------------------------------------------------

plot_fig1_bc(res, res_eig, res_eig_min)


# plots for alternative frequency -----------------------------------------

res2 = readRDS("sup_norm2.RDS")
res_eig2 = readRDS("max_eig2.RDS")
res_eig_min2 = readRDS("min_eig2.RDS")

R_hat2 <- readRDS("R_hat1_alt.RDS")

plot_fig1_a(R_hat2, m, 2, 0)

plot_fig1_bc(res2, res_eig2, res_eig_min2)



