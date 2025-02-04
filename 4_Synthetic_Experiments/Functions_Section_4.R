
# 1. Simulate times -------------------------------------------------------

simulate_times = function(n.trials, n.stream, Big.T, A, B, nu){
  # Usage - simulate trial times for each stream/neuron
  #
  # Input Parameters:
  #
  #   n.trials  - no. of trials 
  #   n.stream  - no. of data streams/neurons
  #   Big.T     - time horizon for each trial
  #   A         - excitation matrix of dimension n.stream x n.stream
  #   B         - decay matrix of dimension n.stream x n.stream
  #   nu        - baseline intensity of process (vector of dimension 1 x n.stream) 
  #
  # Output:
  #
  #   times - nested list of times for each process/neurons
  #   times[[i]][[j]] - contains times of process i, trial j
  
  times = list(); times = rep(list(times), n.stream); times = rep(list(times), n.trials)
  
  for(i in 1:length(times)){
    times[[i]] = simulateHawkes(nu, A, B, Big.T) #from hawkes package
  }
  
  #put data into same structure as "grouped_times"
  grouped_times2 = list();
  for(i in 1:n.stream){
    grouped_times2[[i]] = map(times, i)
  }
  
  jumps = list();
  for(j in 1:n.stream){
    jumps[[j]] = lengths(grouped_times2[[j]]) 
  }
  
  return(list(times = grouped_times2, jumps = jumps))
}




# 2. Simulating model times -----------------------------------------------


simulate_trial_times = function(model_type, P_vec, Big_T, MC_rep, n.trials){ 
  # Usage - simulate trial times for each model considered in the simulation study
  #
  # Input Parameters:
  #
  #   model_type  - 1,2,3 refers to models (a), (b) and (c) discussed in paper
  #   P_vec       - refers to no. of data streams i.e. c(12,48,96)
  #   Big_T       - time horizon for each trial
  #   MC_rep      - no. of Monte Carlo samples
  #   n.trials    - no. of experimental trials
  #
  # Output:
  #
  #   times - nested list of trials times for specific model
  #         - t12 t48 and t96 give trial times for each (12,48,96).
  
  params = Get_model_parameters(model_type, P_vec)
  HD_A_list = params$HD_A_list
  HD_B_list = params$HD_B_list
  nu_list = params$nu_list
  times_list_12 = times_list_48 = times_list_96 = list();
  for(i in 1:MC_rep){
    times_list_12[[i]] = simulate_times(n.trials, P_vec[1], Big_T, HD_A_list[[1]], 
                                        HD_B_list[[1]], nu_list[[1]])$times
    times_list_48[[i]] = simulate_times(n.trials, P_vec[2], Big_T, HD_A_list[[2]], 
                                        HD_B_list[[2]], nu_list[[2]])$times
    times_list_96[[i]] = simulate_times(n.trials, P_vec[3], Big_T, HD_A_list[[3]], 
                                        HD_B_list[[3]], nu_list[[3]])$times
  }
  
  return(list(t12 = times_list_12, t48 = times_list_48, t96 = times_list_96))
}


# 3. Model Parameters -----------------------------------------------------
Get_model_parameters = function(model, P_vec){
  # Usage - obtain model parameters for specified model and dimension
  #
  # Input Parameters:
  #
  #   model  - 1,2,3 refers to models (a), (b) and (c) discussed in paper
  #   P_vec  - refers to no. of data streams i.e. c(12,48,96)
  #
  # Output:
  #
  #   HD_A_list - list of pxp excitation matrices where p=12,48 and 96
  #             - t12 t48 and t96 give trial times for each (12,48,96).
  HD_A_list = HD_B_list = nu_list = list();
  
  if(model==1){
    gamma = 0.4
    alpha = rep(1,3)
    A = get_A(gamma, 1)
    HD_params = get_HD_params(A, 0.86, 12)
    HD_A = HD_params$A
    HD_B = HD_params$B
    nu = rep(0.2, 12)
    for(i in 1:length(P_vec)){
      # print(i)
      HD_params = get_HD_params(HD_A, HD_B, n.stream=P_vec[i])
      HD_A_list[[i]] = HD_params$A
      HD_B_list[[i]] = HD_params$B
      nu_list[[i]] = rep(0.2, P_vec[i])
      
    }
  }
  
  if(model == 2){
    
    n.stream = 3 
    alpha = diag(0.2, n.stream, n.stream)
    alpha[upper.tri(alpha)] = seq(0.1,0.4,length.out=3)
    alpha[lower.tri(alpha)] = t(alpha)[lower.tri(alpha)]
    beta = matrix(0.86, ncol=n.stream, nrow=n.stream)
    nu = rep(0.2,3)
    
    for(i in 1:length(P_vec)){
      #print(i)
      HD_params = get_HD_params(alpha, beta, n.stream=P_vec[i])
      HD_A_list[[i]] = HD_params$A
      HD_B_list[[i]] = HD_params$B
      nu_list[[i]] = rep(0.2, P_vec[i])
    }
    
    
  }
  
  if(model == 3){
    
    n.stream = 12
    alpha = matrix(0, n.stream, n.stream)
    alpha[1,3] = 0.6; alpha[2,10]=0.5; alpha[3,4]=0.8
    alpha[lower.tri(alpha)] = t(alpha)[lower.tri(alpha)] #make symmetric
    alpha12 = alpha
    beta = matrix(1.2, n.stream, n.stream)
    beta12 = beta
    #spectralRadius(alpha/beta) #same as other settings - approx 0.45
    nu = rep(0.2,n.stream)
    
    for(i in 1:length(P_vec)){
      #print(i)
      HD_params = get_HD_params(alpha12, beta12, n.stream=P_vec[i])
      HD_A_list[[i]] = HD_params$A
      HD_B_list[[i]] = HD_params$B
      nu_list[[i]] = rep(0.2, P_vec[i])
    }
    
  }
  
  
  return(list(HD_A_list = HD_A_list, HD_B_list = HD_B_list, nu_list = nu_list))
  
}


# 4. Parameters for Hawkes Process ----------------------------------------

get_HD_params = function(A,beta_val, n.stream){
  # Usage - Populate pxp alpha and beta matrices
  #
  # Input Parameters:
  #
  #   A - Excitation matrix
  #   beta_val - decay matrix
  #   n.stream - no. of data streams/neurons 
  #
  # Output:
  #
  #   A_block - "excitation" matrix -> "alpha" in Hawkes paper
  #   B - decay matrix -> "beta" in Hawkes paper
  A1 = rep(list(A), n.stream/dim(A)[1])
  A_block = as.matrix(bdiag(A1))
  
  B = matrix(rep(beta_val, n.stream), nrow=n.stream, ncol = n.stream)
  
  return(list(B=B, A = A_block))
}

# 5. A Matrix Function -------------------------------------------------------

get_A = function(gamma, alpha){
  
  # Usage - Populate 3x3 "A" matrix for given alpha/gamma values
  #
  # Input Parameters:
  #
  #   gamma - \gamma \in (0,1) - for stability conditions
  #   alpha - set constant at 0.5 [why?]
  #
  # Output:
  #
  #   A - "excitation" matrix -> "alpha" in Hawkes paper
  A = diag(gamma*alpha, nrow=3)
  A[1,1]=0
  A[1,2]=(1-gamma)*alpha
  return(A)
}


# 6. Periodogram Function -------------------------------------------------

periodogram = function(data, omega, Big_T){
  # Usage - obtain trial averaged periodogram for specified frequency
  #
  # Input Parameters:
  #
  #   data  - simulated hawkes process data (nested list of trial times)
  #   omega - specified frequency
  #   Big.T - time horizon for each trial
  #
  # Output:
  #
  #   p1    - pxp periodogram estimate
  
  img=sqrt(as.complex(-1)) #imaginary number i^2=-1
  H_omega = (Big_T/sqrt(Big_T)) * exp((-img*omega*Big_T)/2) * sinc((omega*Big_T)/2) #FT of Taper
  p = length(data) #dimensionality 
  m = length(data[[1]]) #trials
  
  N_t = NULL;
  N_t = rep(list(N_t), m); bar_d = N_t; I = list();
  
  for(j in 1:m){
    for(i in 1:p){
      N_t[[j]][i] = length(data[[i]][[j]])
      bar_d[[j]][i] = (1/sqrt(Big_T) * sum(exp(-img*omega*data[[i]][[j]])) ) - ((N_t[[j]][i]/Big_T) * H_omega )
    }
    
    I[[j]] = (outer((bar_d[[j]]), Conj(bar_d[[j]])))/(2*pi)
  }
  
  p1 = Reduce("+", I)/length(I)
  
  return(p1)
  
}
# 6. Ridge Estimator ---------------------------------------------------------

Ridge = function( n.stream, n.trials, Big_T, freq, lambda, times){
  
  
  #get trial averaged periodogram estiamte
  S_hat = periodogram(times, freq, Big_T)
  
  
  #get ridge estimate
  S_hat_ridge = S_hat + lambda*diag(1, n.stream, n.stream)
  
  #get theta estimate
  
  theta = solve(S_hat_ridge)
  
  pc = partial_co(theta)
  
  return(list(pc=pc, theta = theta))
  
  
  
  
}

# 7. Partial Coherence ---------------------------------------------------

partial_co = function(theta){
  # Usage - Function to obtain parital coherence from inverse SDM (either estimated or theoretical)
  #
  # Input Parameters:
  #
  # theta - estimate of inverse SDM obtained via ADMM
  #
  # Output:
  #
  #   co - partial coherence matrix
  #
  
  #get indices to use - so it generalizes to higher dimensions
  indexes = which(upper.tri(theta), arr.ind = T) #alpha instead of theta?
  index_list = list();
  
  for(i in 1:(length(indexes)/2)){
    index_list[[i]] = indexes[i,]
  }
  co = diag(1, nrow=dim(theta)[1], ncol=dim(theta)[2]);
  for(i in 1:length(index_list)){
    k = as.numeric(index_list[[i]][1])
    j = as.numeric(index_list[[i]][2])
    co[k,j] = Mod(-theta[k,j]/sqrt(theta[k,k]*theta[j,j]))^2
  }
  co[lower.tri(co)] = t(co)[lower.tri(co)] #populate lower triangle
  
  return(co)
}

# 8. Glasso Estimate ------------------------------------------------------

glasso = function(n.stream,  chosen_lambda, S_hat, Max_iter){
  # Usage - Function to obtain glasso estimate for a given scenario and lambda value
  #
  # Input Parameters:
  #
  # n.stream      - no. of data streams/"neuron"
  # chosen_lambda - regularization parameter 
  # S_hat         - periodogram estimate
  # Max_iter      - Max no. of iterations for the ADMM algorithm
  #
  # Output:
  #
  #   theta       - theta estimate from ADMM
  #   z           - z estimate from ADMM
  #   r_tol       - ADMM stopping criteria
  
  
  theta_warm = diag(1, nrow=n.stream, ncol=n.stream)
  res = ADMM(S_hat, Max_iter, chosen_lambda, rho=1, theta_warm)
  theta = res$theta
  r = res$r
  z = res$z
  
  
  
  return(list(theta = theta, z=z, r=r))
}

# 9. ADMM Function --------------------------------------------------------

ADMM = function(S_hat, Max_iter,lambda,rho,theta_warm){
  
  # Usage - ADMM Algorithm for Glasso estimator
  # 
  # Input parameters:
  # 
  #   S_hat     : Estimator for SDM at a given frequency
  #   Max_iter  : Maximum number of iterations for ADMM method
  #   lambda    : regularization parameter
  #   rho       : augmented Lagrangian parameter
  #   theta_warm: inital value for theta matrix
  #   
  # Output:
  #   
  #   theta             : Inverse SDM at chosen frequency at end of ADMM algorithm
  #   r/s               : Stopping Criteria
  #   z                : Z at end of ADMM algorithm 
  
  
  #Initialization 
  
  theta = z = u = RHS_optim_cond = Q = Q_H = D = eigen_D = W = D_tilde = list();
  
  dim = dim(S_hat)[1] #How many processes are in total?
  
  #z = D_tilde = rep(list(matrix(0,nrow=dim,ncol=dim)), Max_iter)
  
  r = s = NULL; #r2=s2=NULL;
  
  theta[[1]] = theta_warm; #previous solution to ADMM
  
  
  z[[1]] = u[[1]]= matrix(0,nrow=dim,ncol=dim)
  
  
  e_tol = 1e-4
  
  for(k in 2:Max_iter){
    
    
    ###############################
    ####Theta minimization step####
    ###############################
    
    RHS_optim_cond[[k]] = rho*(z[[k-1]]-u[[k-1]])-S_hat #RHS of optimality condition 
    
    Q[[k]] = eigen(RHS_optim_cond[[k]])$vectors #eigen vectors of above matrix
    
    Q_H[[k]] = H(Q[[k]])
    
    D[[k]] = diag(eigen(RHS_optim_cond[[k]])$values) #D-matrix - eigenvalues of RHS optim cond on diagonal
    
    #populate D-tilde matrix - does same as below loop
    D_tilde[[k]] = matrix(0,nrow=dim,ncol=dim)
    diag(D_tilde[[k]]) = (diag(D[[k]]) + sqrt(diag(D[[k]])^2+4*rho))/(2*rho) #populate D-tilde matrix - does same as below loop
    
    theta[[k]] = Q[[k]] %*% D_tilde[[k]] %*% Q_H[[k]]
    
    ###############################
    ##### Z-minimization step######
    ###############################
    
    kappa = lambda/rho
    
    W[[k]] = theta[[k]] + u[[k-1]] #W[[k]] - symmetric matrix?
    
    z[[k]] = pmax((1-kappa/Mod(W[[k]])),0) #checks if entries in 1-kappa/Mod(W[[k]]) < 0 -> if so replaces with a zero
    
    z[[k]] = z[[k]] * W[[k]] #want entry wise multiplication NOT matrix multiplication
    
    
    
    ###############################
    ###########U-update############
    ###############################
    
    u[[k]] = u[[k-1]]+theta[[k]]-z[[k]]
    
    ###############################
    ####### ADMM Residuals ########
    ###############################
    
    #Need to check
    
    d1 = theta[[k]] - z[[k]]
    d2 = z[[k]] - z[[k-1]]
    
    r[k] = sum(abs(d1))
    s[k] = sum(abs(d2))
    
    
    if(min(r[k], s[k])<e_tol & min(s, na.rm = T)!=0){break}
    
  }
  
  return(list(theta = theta[[k]], z = z[[k]], theta_all = theta, z_all = z, r = r))
  
}

# 10. Spectra for MV Hawkes Process ---------------------------------------
MV_spectra = function(omega, alpha, beta,nu, n.stream){
  
  # Usage     - Function to obtain SDM for a multivariate Hawkes process with 
  #             Exponential decays
  #
  # Input Parameters:
  #
  # omega     - Chosen Frequency 
  # alpha     - P x P Excitation Matrix    
  # beta      - P x P Decay Matrix
  # nu        - 1  x P vector of background intensities      
  # n.stream  - Number of data streams          
  #
  # Output:
  #
  # spectra   - SDM at specified frequency omega
  # inv_SDM   - Inverse SDM at specified frequency
  # D         - Diagonal matrix of rates of each process
  # pc        - Partial coherence matrix at specified frequency
  
  img=sqrt(as.complex(-1))
  I = diag(1,n.stream, n.stream)
  M = (alpha)/beta
  D = solve(I-M)
  D = D*nu
  D = diag(D)
  D =diag(D, n.stream,n.stream)
  spectra = list();
  for(i in 1:length(omega)){
    gamma = alpha/(beta+(img*omega[i]))
    Id = diag(1, n.stream, n.stream)
    t1 = solve(Id - gamma)
    t2 = solve(Id - H(gamma)) 
    f =  t1 %*% D %*% t2
    spectra[[i]] = f/(2*pi)
  }
  
  inv_sdm = lapply(spectra, solve)
  
  pc = lapply(inv_sdm, partial_co)
  return(list(spectra=spectra, inv = inv_sdm, D=D, pc = pc))
}



# 11. Performance Metrics -------------------------------------------------
performance_measures = function(estim, truth, ind){
  
  # Usage - Function to obtain performance measures for Monte carlo estimates
  #        
  #
  # Input Parameters:
  #
  # estim - p x p matrix containing ISDM estimates
  # truth - true ISDM matrix (Hawkes)
  # ind   - indicator for PC or theta matrix. If true => PC matrix so get TPR and FPR
  #
  # Output:
  # TPR   - True positive rate
  # FPR   - False positive rate
  # l2    - l2 norm
  
  
  l1=l2=l_inf= NULL; d= list()
  
  d = estim - truth
  
  d = d[upper.tri(d)] #consider only upper diagonal entries
  
  l2 = (sum(abs(d)^2))/length(d)
   
  if(ind==T){
    
    #Biniarise truth and estimated ISDM - i.e. set nonzero and non-diagonal elements to 1 so can compare to ground truth
    truth[truth!=1 &truth!=0] = 1
    estim[estim!=1 &estim!=0] = 1
    
    n.stream = dim(truth)[1]
      
    TN = sum(truth==0)/2 #true number of missing edges 
    TP = (sum(truth==1)-n.stream)/2 #no. true edges 
      
      
    TPR = FPR = FP = FN =NULL;
    
    
    diff = estim - truth
      
    FP = sum(diff>0)/2 #false positives 
    FN = sum(diff<0)/2 #false negatives
      
    #TPR and FPR
    FPR = FP/(FP+TN)
    TPR= 1-(FN/TP)  
    
    #F1 - score
    F1 = TP/(TP+0.5*(FP+FN))
  }
  else{TPR=FPR=F1=NA}
    
  return(list(TPR = TPR, FPR = FPR, l2=l2, F1=F1))
  
}



# 12. Formatting Results Function -----------------------------------------

Format_function = function(res, model_type, n.stream, ridge){
  # Usage - Function to format results from simulation study
  #        
  #
  # Input Parameters:
  #
  # res         - .RDS file from simulation study
  # model_type  - 1,2,3 indicating model (a), (b) or (c) in paper
  # n.stream    - no of data streams - i.e. p=12, 48 or 96
  # ridge       - T/F indicating whether we are reporting ridge or glasso results
  #
  # Output:
  # r1          - results averaged across the MC samples
  # r2          - standard errors across MC samples
  
  if(n.stream == 12){k=1}
  if(n.stream == 48){k=2}
  if(n.stream == 96){k=3}
  
  if(ridge==T){pc = map(res,1)}
  
  if(ridge==F){
    z_list=map(res, 3) #3 gives z and 2 gives theta
    pc = lapply(z_list, partial_co)
    } 
  
  theta_list = map(res, 2)
  
  P_vec = c(12,48,96);
  params = Get_model_parameters(model_type, P_vec)
  HD_A = params$HD_A_list; HD_B = params$HD_B_list; nu = params$nu_list
  truth = MV_spectra(omega=0.0628, HD_A[[k]], HD_B[[k]], nu[[k]], P_vec[k])
  true_pc = truth$pc[[1]]
  true_s = truth$spectra[[1]]
  true_theta = truth$inv[[1]]
  
  
  pc_errs = lapply(pc, function(x) performance_measures(x, true_pc, T))
  
  TPR = (unlist(map(pc_errs, 1))) #TPR
  FPR = (unlist(map(pc_errs, 2))) #FPR
  F1 = (unlist(map(pc_errs, 4))) #F1-sscore
  
  E_pc = (unlist(map(pc_errs, 3))) #squared error on pc matrix
  
  theta_errs = lapply(theta_list, function(x) performance_measures(x, true_theta, F))
  
  E_theta = (unlist(map(theta_errs, 3))) #squared error
  
  res2 = list(TPR = TPR, FPR = FPR, E_pc = E_pc, E_theta = E_theta, F1 = F1)
  r1 = lapply(res2, mean) #average across monte carlo samples
  r2 = lapply(res2, function(x) sd(x)/length(x)) #standard errors across MC samples
  
  return(list(r1=r1, r2=r2))
}


# 13. Formatting Periodogram Results --------------------------------------

results_periodogram = function(res, model_type, n.stream){
  
  # Usage - Function to format results from simulation study for periodogram estimates
  #        
  #
  # Input Parameters:
  #
  # res         - .RDS file from simulation study
  # model_type  - 1,2,3 indicating model (a), (b) or (c) in paper
  # n.stream    - no of data streams - i.e. p=12, 48 or 96
  #
  # Output:
  # r1          - results averaged across the MC samples
  # r2          - standard errors across MC samples
  
  if(n.stream == 12){k=1}
  if(n.stream == 48){k=2}
  if(n.stream == 96){k=3}
  
  P_vec = c(12,48,96);
  params = Get_model_parameters(model_type, P_vec)
  HD_A = params$HD_A_list; HD_B = params$HD_B_list; nu = params$nu_list
  truth = MV_spectra(omega=0.0628, HD_A[[k]], HD_B[[k]], nu[[k]], P_vec[k])
  true_ISDM = truth$inv
  
  metrics = lapply(res, function(x) performance_measures(x, true_ISDM[[1]], F))
  theta_errs = map(metrics, 3)
  r1 = mean(unlist(theta_errs))
  r2 = sd(unlist(theta_errs))/length(unlist(theta_errs))
  
  return(list(r1 = r1, r2 = r2))
}


# 14. Ground truth --------------------------------------------------------

Get_ground_truth = function(P_vec, model_type, freq){
  # Function to get ground truth for each model and each dimension at a particular frequency of interest.
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


# 15. eBIC function -------------------------------------------------------

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


# 16. Function to select regularsiation parameter (GLASSO) -------------------------

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
  
  return(list(lam_mse = lam_mse, lam_f1 =  lam_f1, l_ebic = lam_ebic, ebic= ebic))
}


# 17. Function to select regularisation parameter (ridge) -----------------

lam_func_ridge = function(lambdas, out_storm, N_samp, gt, p, n.trials){
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
    S_list = map(res, 2)
    l2[[i]] = unlist(lapply(theta_list, function(x) performance_measures(x, gt$theta, F)$l2))
    av_mse[i] = mean(l2[[i]])
    
    
    for(j in 1:length(theta_list)){
      ebic[[i]][j] = eBIC_check(theta_list[[j]], S_list[[j]], theta_list[[j]], 0.5, p, n.trials)
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
    
    
  }
  lam_mse = mean(lambdas[idx])
  lam_ebic = mean(lambdas[idx2])
  
  
  return(list(lam_mse = lam_mse, l_ebic = lam_ebic, ebic= ebic))
}
