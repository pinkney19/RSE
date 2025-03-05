
# libraries ---------------------------------------------------------------

library(Matrix)
library(phonTools) 
library(QZ) 
library(purrr) 
library(matrixStats) 
library(complexplus) 
library(psych) 
library(pracma) 
library(hawkes) 
library(igraph)

setwd("~/Downloads/RSE/4_Synthetic_Experiments")
source("Functions_Section_4.R")


# Low Rank Interaction Matrix ---------------------------------------------


# Set the dimensions of the matrix and rank
p <- 12  # Size of the matrix (12x12)

create_low_rank = function(p, rank){
  
  B <- matrix(runif(p* rank, 0,0.25), nrow = p, ncol = rank)
  C <- matrix(runif(rank * p, 0, 0.25), nrow = rank, ncol = p)
  
  # Compute the low-rank excitation matrix A = B %*% C
  alpha <- B %*% C
  rank = rankMatrix(alpha)
  
  sr = spectralRadius(alpha)
  
  n = sr/0.83 # we want spectral radius to be 0.83 to align with other experimental conditions
  
  beta = matrix(n, nrow = p, ncol = p)
  
  spectralRadius(alpha/beta) # ensures spectral radius is approx 0.83
  
  return(list(alpha = alpha, beta = beta, rank_alph = rank))
}

set.seed(123)  # For reproducibility
p_vec = c(12,48,96)
alpha = list(); beta = list(); rank = NULL;
for(i in 1:length(p_vec)){
  res = create_low_rank(p_vec[i], rank = 3)
  alpha[[i]] = res$alpha
  beta[[i]] = res$beta
  rank[i] = res$rank_alph[1]
}

par(mfrow = c(1,3))
lapply(alpha, image)


# save alpha and beta matrices --------------------------------------------

setwd("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Parameters")
saveRDS(alpha, "alpha_list.RDS")
saveRDS(beta, "beta_list.RDS")

# check simulate times ----------------------------------------------------

check = simulate_times(10, 96,200, alpha[[3]], beta[[3]], rep(0.2,96))$times
