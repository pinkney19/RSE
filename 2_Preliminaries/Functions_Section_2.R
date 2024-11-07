

# 1. Simulate Poisson Process ---------------------------------------------

pprocess = function(lambda, T_prime, m, p){
  # Function to simulate poisson process times for m trials of dimension p
  # Inputs 
  # lambda - rate 
  # T_prime - no. of events
  # m - no. of trials
  # p - no. of dimensions
  # Output
  # times - nested list of process times for each trial 
  # times[[j]][[i]] gives trial times of dimension j for trial i  
  inter = list(); times = rep(list(inter), p)
  for(j in 1:p){
    for(i in 1:m){  
      inter[[i]] =rexp(T_prime, lambda)
      times[[j]][[i]] = cumsum(inter[[i]])
      times[[j]][[i]] = times[[j]][[i]][times[[j]][[i]]<T_prime] #only want times up to and including T=1000
    }
  }
  return(times)
}


# 2. Periodogram Function -------------------------------------------------

periodogram = function(data, omega, Big_T){
  
  
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


# 3. Density function for Goodman Distribution ----------------------------


Goodman_eq = function(m,p,R.squared,x){
  #Function to evaluate equation (4) in the main paper
  #Inputs
  # m         - no. of trials
  # p         - no. of data streams
  # R.squared - true coherence
  # x         - points at which to evaluate the density 
  # Output
  # Output to function
  t1 = gamma(m) / (gamma(p-1)*gamma(m-p+1))
  t2 = (1-R.squared)
  t3 = (x)^(p-2)
  t4 = (1-x)^(m-p)
  t5 = hypergeo(m,m,(p-1), (R.squared*x)) #??
  return(t1*t2*t3*t4*t5)
}


# 4. Plotting Figure 1a  ----------------------------------------------


plot_fig1_a =function(R_hat, m,p,R.squared){
  # Function to plot empirical and theoretical Goodman Distribution
  # Inputs
  # R_hat     - estimated coherence
  # m         - no. of trials
  # p         - no. of data streams
  # R.squared - true coherence
  # x         - points at which to evaluate the density 
  # Output
  #           - plot for paper (Fig 1)
  xs=seq(0,1,0.001) #points at which to evaluate Goodman
  t=Goodman_eq(m,p,R.squared,xs)
  #par(mar = c(5,4,4,2)+1, mgp=c(3,1,0))
  par(mar = c(5,4,4,2) + 2.5, mgp=c(4,1,0))
 
  hist(R_hat,freq=F, xlim=c(0,1), main="", breaks=30, col="tomato",
       xlab = expression(R[12](omega)),cex.lab=2,cex.axis=2, las=1, family ="serif", ylim=c(0,9))
  lines(xs,t,type='l', col='black')
}


# 5. Plotting Figure 1b and 1c --------------------------------------------------
plot_fig1_bc = function(res, res_eig, res_eig_min){
  
  mean_res = unlist(lapply(res, mean))
  mean_lower = unlist(lapply(res, quantile, probs=0.025))
  mean_upper = unlist(lapply(res, quantile, probs = 0.975))
  
  mean_res_eig = unlist(lapply(res_eig, mean))
  eig_lower = unlist(lapply(res_eig, quantile, probs=0.025))
  eig_upper = unlist(lapply(res_eig, quantile, probs = 0.975))
  
  
  mean_res_eig_min = unlist(lapply(res_eig_min, mean))
  eig_min_lower = unlist(lapply(res_eig_min, quantile, probs=0.025))
  eig_min_upper = unlist(lapply(res_eig_min, quantile, probs = 0.975))
  
  #fig 1b
  par(mar = c(5,4,4,2) + 2.5, mgp=c(4,1,0))
  plot(seq(1,100), (mean_res), type='l', xlab = "p", ylab = expression(paste("||", hat(S)(omega)-S(omega),"||", infinity)), 
       cex.axis = 2, cex.lab =2, ylim = c(min(mean_lower),max(mean_upper)), las=1, family="serif", axes = F, frame.plot = T)
  #axis(2, at = round(mean_res, 2), labels = round(mean_res, 2) , cex.axis = 2, las =1)
  labs = seq(0, 0.35, length.out=8)
  axis(2, at = round(seq(0, 0.35, length.out=8),2), labels = sprintf("%.2f", labs) , cex.axis = 2, las =1)
  lines(seq(1,100), (mean_lower), col="brown2", lwd=1)
  lines(seq(1,100), (mean_upper), col="brown2", lwd=1)
  abline(v=10, lty=2, lwd=1)
  
  #fig 1c
  par(mar = c(5,4,4,2) + 2.5, mgp=c(4,1,0))
  #par(mar = c(5,4,4,2) + 1, mgp=c(3.5,1,0))
  plot(seq(1,10), (mean_res_eig/mean_res_eig_min)[1:10], type='l',xlab = "p", 
       ylab = expression(phi["max"]/phi["min"]) ,xlim=c(0,10),
       cex.axis = 2, cex.lab=2, lwd=1, las = 1, cex.axis =2, family = "serif")
  lines(seq(1,10), (eig_lower/eig_min_lower)[1:10], col="brown2", lwd=1)
  lines(seq(1,10), (eig_upper/eig_min_upper)[1:10], col="brown2", lwd=1)
  
  
}


# 5. Periodogram without mean correction ----------------------------------

raw_periodogram = function(data, omega, Big_T){
  
  
  img=sqrt(as.complex(-1)) #imaginary number i^2=-1
  H_omega = (Big_T/sqrt(Big_T)) * exp((-img*omega*Big_T)/2) * sinc((omega*Big_T)/2) #FT of Taper
  p = length(data) #dimensionality 
  m = length(data[[1]]) #trials
  
  N_t = NULL;
  N_t = rep(list(N_t), m); bar_d = N_t; I = list();
  
  for(j in 1:m){
    for(i in 1:p){
      #N_t[[j]][i] = length(data[[i]][[j]])
      bar_d[[j]][i] = sum(exp(-img*omega*data[[i]][[j]]))  #- ((N_t[[j]][i]/Big_T) * H_omega )
    }
    
    I[[j]] = (outer((bar_d[[j]]), Conj(bar_d[[j]])))/(2*pi)
  }
  
  p1 = Reduce("+", I)/length(I)
  
  return(p1)
  
}




