
# Simulation Study for Section 2.3 ----------------------------------------

setwd("~/Downloads/RSE/2_Preliminaries")
source("Functions_Section_2.R")


library(Matrix)
library(phonTools) 
library(QZ) 
library(purrr) 
library(matrixStats)
library(complexplus) 
library(psych) 
library(pracma) 
library(hawkes) 


T_prime = 1000 #time to simulate Poisson process until.
m = 10 #no. trials.
lambda = 1 #rate of homogeneous Poisson process.
freqs = pi/2 #0.0628 #chosen w.l.o.g since we are looking at poisson process.
MC_samples = 1000; #no. Monte Carlo samples
p = 100 #no. of dimensions/data streams

res = res_eig = res_eig_min = list();
s =Sys.time();
set.seed(5)
for(i in 1:p){
  print(i)
  l_inf= max_eig = min_eig=NULL; 
  for(k in 1:MC_samples){
    
    times = pprocess(lambda, T_prime, m,i) #simulate times
    S_hat = periodogram(times, freqs[1], T_prime) #estimate trial averaged periodogram
    
    #get max eig of S_hat
    
    max_eig[k] = max(abs(eigen(S_hat)$val))
    min_eig[k] = min(abs(eigen(S_hat)$val))
    
    #true spectra 
    true_spec = diag(lambda/(2*pi), i, i)
    
    #l_infinity norm 
     d = S_hat - true_spec
     l_inf[k] = max(abs(d)) 
  }
  #store results in a list for each p
  res[[i]] = l_inf
  res_eig[[i]] = max_eig
  res_eig_min[[i]] = min_eig
}
e = Sys.time()
time_diff = e-s

setwd("~/Downloads/RSE/2_Preliminaries")
saveRDS(res, "sup_norm2.RDS")
saveRDS(res_eig, "max_eig2.RDS")
saveRDS(res_eig_min, "min_eig2.RDS")



# Goodman Example ---------------------------------------------------------


p = 7 #no. datastreams
m = 10
R_hat = NULL;
T_prime = 1000; MC_samples = 1000;
lambda=1
freqs = pi/2#0.0628

for(i in 1:MC_samples){
  times = pprocess(lambda, T_prime, m,p)
  S_hat = periodogram(times, freqs[1], T_prime)
  R_hat[i] = Mod(S_hat[1,2])^2/(S_hat[1,1]*S_hat[2,2])
}
R_hat = Re(R_hat)
setwd("~/Downloads/RSE/2_Preliminaries")
saveRDS(R_hat, "R_hat1_alt.RDS")
