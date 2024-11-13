
# 1. Pre-Processing  ------------------------------------------------------
pre_processing_function <- function(data){
  #'  Function to restructure Bolding and Franks Data
  #'
  #' @param data (rds) rds file containing raw data
  #' @export  
  #' Outputs a nested list of trial spike times for each neuron (same structure as simulation study)

  h=vector(mode="list", length=dim(data)[1])
  
  trial_names = NULL;
  for(i in 1:dim(data)[1]){
    trial_names[i] = paste0("trial ", i)
  }
  names(h) = trial_names 
  data_output = rep(list(h), dim(data)[2]-1)
  
  neuron_names = NULL;
  for(i in 1:dim(data)[2]-1){
    neuron_names[i] = paste0("Neuron ", i)
  }
  
  names(data_output) = neuron_names
  
  
  for(i in 2:dim(data)[2]){ # remove first column as it is aggregate of the rest
    for(j in 1:dim(data)[1]){
      data_output[[i-1]][[j]] = (data[,i][[j]][1,])
    }
  }
  
  return(data_output)
}



# 2. Firing rate plots ----------------------------------------------------

spike_density = function(data, bin_size, a, b, title, c, d){
  #'  Function to plot firing rate plots from Bolding and Franks Data
  #'
  #' @param data pre-processed data in same format as simulation study
  #' @param bin_size time bins for data
  #' @param a,b xlimits for plots
  #' @param title title of plot
  #' @param c,d ylimits for plots 
  #' @export  
  #' Outputs a nested list of trial spike times for each neuron (same structure as simulation study)
  xs = seq(-5,10, bin_size)
  check = lapply(data, function(x) cut(x, breaks = xs))
  check2 = lapply(check, table)
  counts = NULL;
  for(i in 1:(length(xs)-1)){
    counts[i] = mean(unlist(map(check2, i)))
  }
  plot(seq(-5,(10-bin_size),bin_size), counts/bin_size, type='l', 
       xlab = "Time (s)", ylab = "Firing Rate (Hz)", main = title, xlim = c(a,b), ylim = c(c,d),
       cex.main = 2, cex.axis = 2, cex.lab = 2, family = "serif", las =1)
  return(counts)
}



# 2. Raster Plots ---------------------------------------------------------

Raster_plots = function(data, title, a, b){
  #Function to plot rasters
  plot((data[[1]]), rep(1, length(data[[1]])), ylab = "Trial", xlab = "Time (s)", 
       ylim = c(1,10), main = title, pch=1, cex=0.2, xlim = c(a,b), cex.axis  =2 , cex.lab =2, cex.main =2)
  for(i in 2:length(data)){
    points((data[[i]]), rep(i, length(data[[i]])), pch=1, cex=0.2)
  }
  rect(xleft=0, xright=1, ybottom = par("usr")[3], ytop = par("usr")[4], border = NA,col=adjustcolor("coral2", alpha=0.3))
  rect(xleft=9, xright=10, ybottom = par("usr")[3], ytop = par("usr")[4], border = NA,col=adjustcolor("coral2", alpha=0.3))
}
# 3. Plotting Power Spectra -----------------------------------------------

plot_power_spec = function(S_hat, freqs, plot_title, im_ind){
  #Function to plot power spectra as shown in Figure 3
  #Inputs -
  #       S_hat       - Periodogram Estimate
  #       freqs       - considered frequencies of interest (omega)
  #       data_type   - string for plot_title
  #       n.stream    - no. data streams/neurons
  #       im_ind      - indicator var. If true do image plot, if false do line graph  
  #Outputs - 
  #                   - Plot of power spectra
  
  power = lapply(S_hat, diag); pspec=list();
  power = lapply(power, Re) #get rid of +0i's
  for(i in 1:dim(S_hat[[1]])[1]){ #i in 1:n.stream
    pspec[[i]] = unlist(map(power, i))
  }
  if(im_ind==F){
    f = freqs/(2*pi)
    plot(f, pspec[[1]], type='l',  col="black", main=plot_title, 
         ylab ="Power", xlab = "Frequency (Hz)" , 
         ylim = c(0,19) , cex.main = 2, cex.axis = 2, cex.lab = 2, family = "serif", las =1)
    for(i in 2:length(pspec)){
      lines(f, pspec[[i]], col="black")
    }
  }
  if(im_ind==T){
    print("yes")
    mat = matrix(NA, ncol = length(freqs), nrow = dim(S_hat[[1]])[1])
    for(i in 1:nrow(mat)){
      mat[i,] = pspec[[i]]
    }
    #mat = t(mat)
    # n.stream = dim(S_hat[[1]])[1]
    # if(n.stream==24){
    #   mat = add_zeroes2(mat)
    # }
    # if(n.stream==26){
    #   mat = re_arrange(mat)
    # }
    plot_image(mat, plot_title)
    
  }
}



# 4. Periodogram function --------------------------------------------

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
# 5. Plot Image -----------------------------------------------------------
plot_image = function(m, type){
  library(ggplot2)
  x = seq(1,dim(m)[2],1)
  y = seq(1,dim(m)[1],1)
  data = expand.grid(X=x,Y=y)
  data$Z <- as.vector(Re(t(m)))
  data$Z[data$Z==0] = NA #change zero entries to NAs
  
  
  
  plt = ggplot(data, aes(X,Y, fill=Z)) + geom_tile()  + scale_fill_distiller(palette = "Reds", direction=+1, 
                                                                             na.value = "white", limits =c(0,20)) + #limits = c(0,max(data$Z)))  +
   
    labs(title = type, fill = "" , x = "Frequency (Hz)", y="Neuron")+ scale_y_reverse()+
    theme(text = element_text(size=20, family = "serif"))  #breaks = seq(1,dim(m)[1])) + scale_x_continuous(breaks = seq(1,dim(m)[2]))#+
  # theme(plot.title = element_text(size=10), axis.text = element_text(size=15),
  #      legend.title = element_text(size=10)) 
  #labs(title = paste("", f), fill = "Coherence" , x = "Neuron", y="Neuron") + scale_y_reverse()
  
  return(plt)
}

# 6. Trial-Frequency Smoothed Periodogram Estimate ------------------------

Get_pooled_Estimator = function(om, data_type, Big_T){
  S_hat = lapply(om, function(x) periodogram(data_type, x, Big_T)) #Get periodogram estimates for each frequecy of interest
  bands = list(seq(1,4), seq(4,8), seq(8,12), seq(12,30), seq(30,50))
  pooled_s = list();
  for(i in 1:length(bands)){
    p = S_hat[bands[[i]]]
    pooled_s[[i]] = Reduce("+", p)/length(p)
  }
  return(list(pooled_s = pooled_s, f = lengths(bands)))
}

# 7. Glasso for Pooled Estimator -----------------------------------------

glasso_pooled = function(n.stream,  chosen_lambda, S_hat, Max_iter){
  # Usage - Function to obtain partial coherence for a given scenario and lambda value
  #
  # Input Parameters:
  #
  # n.trials      - no. of trials
  # n.stream      - no. of data streams/"neuron"
  # Big.T         - time to simualte trial data to
  # A             - p x p excitation matrix -> A_list[[i]]
  # B             - p x p decay matrix
  # nu            - 1 x p baseline intensity vector
  # chosen_lambda - regularisation parameter for ADMM
  # freq          - chosen frequency we want to obtain pc estimate at
  #
  # Output:
  #
  #   pc          - partial coherence estimate
  #   r_tol       - cut-off value -> tolerance from ADMM
  
  
  theta_warm = diag(1, nrow=n.stream, ncol=n.stream)
  res = ADMM(S_hat, Max_iter, chosen_lambda, rho=1, theta_warm)
  theta = res$theta
  r_tol = tail(res$r,1)
  
  z = res$z
  
  
  
  return(list(r_tol=r_tol, theta = theta, z=z, r=res$r))
}

# 8. ADMM Function --------------------------------------------------------

ADMM = function(S_hat, Max_iter,lambda,rho,theta_warm){
  
  # Usage
  # 
  # Input parameters:
  # 
  #   S_hat     : Estimator for SDM at a given frequency
  #   Max_iter  : Maximum number of iterations for ADMM method
  #   lambda    : regularization parameter
  #   rho       : augmented Lagrangian parameter
  #   freq      : frequency for which we want to obtain the inverse SDM matrix
  #   theta     : output from previous ADMM based on a specific lamdba value/can just be identity matrix
  #   
  # Output:
  #   
  #   theta[[Max_iter]] : Inverse SDM at chosen frequency at end of ADMM algorithm
  #   r/s               : Stopping Criteria
  #   z[[Max_iter]]     : Z at end of ADMM algorithm 
  
  
  #Initialization 
  
  theta = z = u = RHS_optim_cond = Q = Q_H = D = eigen_D = W = D_tilde = list();
  
  dim = dim(S_hat)[1] #How many processes are in total?
  
  #z = D_tilde = rep(list(matrix(0,nrow=dim,ncol=dim)), Max_iter)
  
  r = s = NULL; #r2=s2=NULL;
  
  theta[[1]] = theta_warm; #previous solution to ADMM
  
  #zs = 
  
  z[[1]] = u[[1]]= matrix(0,nrow=dim,ncol=dim)
  
  #just for purpose of initialization 
  # r[1] = 1#sum(abs(Mod(theta[[1]]-z[[1]]))) #l1 norm of Mod(theta-z)
  # s[1] = 2#sum(abs(Mod(rho*(z[[1]]-z[[1]]))))
  
  #checks
  #k=2
  e_tol = 1e-4
  
  for(k in 2:Max_iter){
    
    #Stopping criteria
    
    # if(min(r[k-1], s[k-1])<e_tol){
    #   Max_iter = k-1
    # }
    # 
    # else{
    
    #while(min(r[k-1], s[k-1])>e_tol){
    
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
    
    
    # r[k] = sum((Mod(theta[[k]]-z[[k]]))^2)
    # s[k] = sum((Mod(rho*(z[[k]]-z[[k-1]])))^2)
    
    #e_pri[k] = sqrt(n.stream^2)*e_tol + e_tol*8.370021
    
    if(min(r[k], s[k])<e_tol & min(s, na.rm = T)!=0){break}
    
  }
  
  return(list(theta = theta[[k]], z = z[[k]], theta_all = theta, z_all = z, r = r,s=s, u=u))
  
}
# 9. eBIC ----------------------------------------------------------------

eBIC = function(theta, S, z, gam, n.stream, f, n.trials){
  #theta -    estimate for theta
  #S -        estimator of SDM -> either TA or pooled TA
  #z -        sparse output from ADMM
  #n.stream   no. data streams/dimensionality.
  
  lik = -log(Det(theta))+tr(S%*%theta)
  u = z[upper.tri(z, diag = F)]
  edges = length(u[u!=0])
  
  op = n.trials*f*lik +  (edges *log(f*n.trials)) + 4*edges*gam*log(n.stream)
  
  return(op)
} 

# 10. Partial Coherence ---------------------------------------------------

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
  
  #put diagonals to 0 just so can look at maximum connections
  #diag(co)=0
  
  #connections = which(co > sort(co[lower.tri(co)], TRUE)[4], arr.ind = TRUE)
  #diag(co)=1
  
  return(co)
}


# 11. iGraph Plots --------------------------------------------------------

plot_UGM <- function(true_pc, estim_pc, truth){
  
  #Params
  # true_pc = theoretical partial coherence/Control
  # estim_pc = estimated partial coherence/Treatment
  # truth = T/F where T indicates "true" and F indicates "estimate"
  
  
  #set what graph you want to produce at end
  if(truth == T){pc = true_pc}
  if(truth == F){pc = estim_pc}
  
  #get edges of undirected graph - inculdes diagonals (self-excitation)
  upper_pc_row = t(pc)[lower.tri(pc, diag = T)]
  g1 = graph_from_adjacency_matrix(pc, mode="undirected", weighted=T)
  #par(mar = c(5,6,4,1))
  edges = upper_pc_row[upper_pc_row!=0]
  #create data frame with edges and colours (different colour for cross-excitation)
  df = data.frame(edges)
  df$col = 1
  
  for(i in 1:length(df$edges)){
    if(df$edges[i]!=1){df$col[i]="coral2"}  
    if(df$edges[i]==1){df$col[i]="black"}
  }
  
  pc_control = true_pc
  pc_treat = estim_pc
  
  n.stream = dim(pc_control)[1]
  
  #Biniarise pc entries
  bin_pc_treat = bin_pc_control = matrix(0, n.stream, n.stream)
  bin_pc_control[pc_control!=0]=1
  bin_pc_treat[pc_treat!=0]=1
  
  
  #get indices of common edges
  conns_treat = t(bin_pc_treat)[lower.tri(bin_pc_treat, diag=T)]
  conns_contr = t(bin_pc_control)[lower.tri(bin_pc_control, diag=T)]
  
  idx_control = which(conns_contr==1)
  idx_treat = which(conns_treat ==1)
  
  idx_control %in% idx_treat
  
  if(truth==F){
    same_idx = which(idx_treat %in% idx_control)
    
    
    for(i in 1:length(df$col[same_idx])){
      if(df$col[same_idx][i]!="black"){df$col[same_idx][i]="navy"}
    }
  }
  
  if(truth==T){
    same_idx = which(idx_control %in% idx_treat)
    
    for(i in 1:length(df$col[same_idx])){
      if(df$col[same_idx][i]!="black"){df$col[same_idx][i]="navy"}
    }
  }
  
  for(i in 1:length(df$edges)){
    if(df$edges[i]==1){df$col[i] = "white"}
  }
  p = plot(g1, layout=layout.circle, edge.color=df$col, 
           vertex.color="white", vertex.label.color = "black")#, #edge.width = df$edges*1000)
  if(truth==T){title = "Control"} #"Theoretical"
  else{title = "Treatment"} #"Estimated"
  
  title(main = title, adj=0.5, cex.main=1, line=-2)
  return(list(p=p, pc=pc))
}


# 12. Formatting Pooled Estimator Tuning Results --------------------------
format_res = function(res, Pooled_S){
  #Function to format output from tuning pooled estimator
  #Inputs -
  #     res       - RDS output from tuning script
  #     Pooled_S  - Trial-Frequency Smoothed Periodogram Estimate
  #Outputs - 
  #     pc        - partial coherence matrix - infers neural connectivity
  #     lam       - selected regularisation parameter from eBIC
  #     r         - ADMM Stopping Criteria
  
  eBIC = unlist(map(res, 1))
  lambdas = logspace(-3,1,100)
  
  idx = which.min(Re(eBIC))
  chosen_lam = lambdas[idx]
  out = glasso_pooled(26, chosen_lam, Pooled_S, Max_iter = 1000)
  pc = partial_co(out$z)
  r = out$r
  
  return(list(pc=pc, lam=chosen_lam, r=r, eBIC=eBIC))
}




# 13. Formatting Single Frequency Estimator Tuning Results ----------------

format_res2 = function(out){
  #Function to format output from tuning single frequency estimator
  #Inputs -
  #     out       - RDS output from tuning script
  #Outputs - 
  #     eBICs     - list of eBIC output (one for each frequency)
  #     lams      - vector of selected regularization parameters (one for each frequency)
  #     thetas    - list of ISDMs (one for each frequency)
  #     zs        - list of sparse ISDMs (one for each frequency)
  #     rs        - list of ADMM stopping criteria (one for each frequency)
  
  #Get eBIC output
  BIC_out = map(out, 1) #nested list of length 100 - each of length 4.
  BICs = list(); lams = NULL;
  Z_out = map(out, 3); r_out = map(out, 4); theta_out = map(out, 2)
  Zs = rs = thetas = list();
  lambdas = logspace(-3,1,100)
  for(i in 1:50){
    BICs[[i]] = unlist(map(BIC_out, i))
    lams[i] = lambdas[which.min(BICs[[i]])]
    Zs[[i]] = unlist(map(Z_out, i))
    rs[[i]] = unlist(map(r_out, i))
    thetas[[i]] = unlist(map(theta_out, i))
  }
  
  return(list(eBICs = BICs, lams = lams, thetas = thetas, zs = Zs, rs = rs))
}


# 14. Glasso for Single Frequency Estimator -------------------------------

glasso_single = function(n.stream, lams, S_hat, Max_iter){
  # Usage - Function to obtain Glasso results for single frequency estimator at all frequencies 
  #
  # Input Parameters:
  #
  # n.stream      - no. of data streams/"neuron"
  # lams          - vector of lambda values for each frequency
  # S_hat         - list of TA SDM estimates (one for each freq)
  # Max_iter      - No. max iterations for ADMM algorithm
  #
  # Output:
  #
  #   res         - list of output results for each frequency - including ADMM
  #               - residuals, ISDM estimates and sparse ISDM estimates
  #   r_tol       - cut-off value -> tolerance from ADMM
  res=list();
  for(i in 1:50){
    print(i)
    res[[i]] = glasso_pooled(n.stream, lams[i], S_hat[[i]], Max_iter)
  }
  return(res)
}



# 15. Weighted iGraph Plots -----------------------------------------------

plot_UGM_weighted <- function(true_pc, estim_pc, truth){
  
  #Params
  # true_pc = theoretical partial coherence/Control
  # estim_pc = estimated partial coherence/Treatment
  # truth = T/F where T indicates "true" and F indicates "estimate"
  
  
  #set what graph you want to produce at end
  if(truth == T){pc = true_pc}
  if(truth == F){pc = estim_pc}
  
  #get edges of undirected graph - inculdes diagonals (self-excitation)
  upper_pc_row = t(pc)[lower.tri(pc, diag = T)]
  g1 = graph_from_adjacency_matrix(pc, mode="undirected", weighted = T)
  #par(mar = c(5,6,4,1))
  edges = upper_pc_row[upper_pc_row!=0]
  #create data frame with edges and colours (different colour for cross-excitation)
  df = data.frame(edges)
  df$col = 1
  for(i in 1:length(df$edges)){
    if(df$edges[i]!=1){df$col[i]="coral2"}  
    if(df$edges[i]==1){df$col[i]="black"}
  }
  
  pc_control = true_pc
  pc_treat = estim_pc
  
  n.stream = dim(pc_control)[1]
  
  #Biniarise pc entries
  bin_pc_treat = bin_pc_control = matrix(0, n.stream, n.stream)
  bin_pc_control[pc_control!=0]=1
  bin_pc_treat[pc_treat!=0]=1
  
  #get indices of common edges
  conns_treat = t(bin_pc_treat)[lower.tri(bin_pc_treat, diag=T)]
  conns_contr = t(bin_pc_control)[lower.tri(bin_pc_control, diag=T)]
  
  idx_control = which(conns_contr==1)
  idx_treat = which(conns_treat ==1)
  
  idx_control %in% idx_treat
  
  if(truth==F){
    same_idx = which(idx_treat %in% idx_control)
    
    
    for(i in 1:length(df$col[same_idx])){
      if(df$col[same_idx][i]!="black"){df$col[same_idx][i]="navy"}
    }
  }
  
  if(truth==T){
    same_idx = which(idx_control %in% idx_treat)
    
    for(i in 1:length(df$col[same_idx])){
      if(df$col[same_idx][i]!="black"){df$col[same_idx][i]="navy"}
    }
  }
  
  for(i in 1:length(df$edges)){
    if(df$edges[i]==1){df$col[i] = "white"}
  }
  p = plot(g1, layout=layout.circle, edge.color=df$col, 
           vertex.color="white", vertex.label.color = "black", edge.width = df$edges*10)
  if(truth==T){title = "Control"} #"Theoretical"
  else{title = "Treatment"} #"Estimated"
  
  title(main = title, adj=0.5, cex.main=1, line=-5)
  return(list(p=p, pc=pc))
}

# 16. iGraph plots for stim -----------------------------------------------
plot_UGM2 <- function(true_pc, estim_pc, truth){
  
  #Params
  # true_pc = theoretical partial coherence/Control
  # estim_pc = estimated partial coherence/Treatment
  # truth = T/F where T indicates "true" and F indicates "estimate"
  
  
  #set what graph you want to produce at end
  if(truth == T){pc = true_pc}
  if(truth == F){pc = estim_pc}
  
  #get edges of undirected graph - inculdes diagonals (self-excitation)
  upper_pc_row = t(pc)[lower.tri(pc, diag = T)]
  g1 = graph_from_adjacency_matrix(pc, mode="undirected", weighted=T)
  #par(mar = c(5,6,4,1))
  edges = upper_pc_row[upper_pc_row!=0]
  #create data frame with edges and colours (different colour for cross-excitation)
  df = data.frame(edges)
  df$col = 1
  
  for(i in 1:length(df$edges)){
    if(df$edges[i]!=1){df$col[i]="coral2"}  
    if(df$edges[i]==1){df$col[i]="black"}
  }
  
  pc_control = true_pc
  pc_treat = estim_pc
  
  n.stream = dim(pc_control)[1]
  
  #Biniarise pc entries
  bin_pc_treat = bin_pc_control = matrix(0, n.stream, n.stream)
  bin_pc_control[pc_control!=0]=1
  bin_pc_treat[pc_treat!=0]=1
  
  
  #get indices of common edges
  conns_treat = t(bin_pc_treat)[lower.tri(bin_pc_treat, diag=T)]
  conns_contr = t(bin_pc_control)[lower.tri(bin_pc_control, diag=T)]
  
  idx_control = which(conns_contr==1)
  idx_treat = which(conns_treat ==1)
  
  idx_control %in% idx_treat
  
  if(truth==F){
    same_idx = which(idx_treat %in% idx_control)
    
    
    for(i in 1:length(df$col[same_idx])){
      if(df$col[same_idx][i]!="black"){df$col[same_idx][i]="navy"}
    }
  }
  
  if(truth==T){
    same_idx = which(idx_control %in% idx_treat)
    
    for(i in 1:length(df$col[same_idx])){
      if(df$col[same_idx][i]!="black"){df$col[same_idx][i]="navy"}
    }
  }
  
  for(i in 1:length(df$edges)){
    if(df$edges[i]==1){df$col[i] = "white"}
  }
  p = plot(g1, layout=layout.circle, edge.color=df$col, 
           vertex.color="white", vertex.label.color = "black")#, #edge.width = df$edges*1000)
  if(truth==T){title = "No Stimulus"} #"Theoretical/Control"
  else{title = "Stimulus"} #"Estimated/treatment"
  
  title(main = title, adj=0.5, cex.main=1, line=-2)
  return(list(p=p, pc=pc))
}


# 17. iGraph plots for pre and post ---------------------------------------

plot_UGM3 <- function(true_pc, estim_pc, truth){
  
  #Params
  # true_pc = theoretical partial coherence/Control
  # estim_pc = estimated partial coherence/Treatment
  # truth = T/F where T indicates "true" and F indicates "estimate"
  
  
  #set what graph you want to produce at end
  if(truth == T){pc = true_pc}
  if(truth == F){pc = estim_pc}
  
  #get edges of undirected graph - inculdes diagonals (self-excitation)
  upper_pc_row = t(pc)[lower.tri(pc, diag = T)]
  g1 = graph_from_adjacency_matrix(pc, mode="undirected", weighted=T)
  #par(mar = c(5,6,4,1))
  edges = upper_pc_row[upper_pc_row!=0]
  #create data frame with edges and colours (different colour for cross-excitation)
  df = data.frame(edges)
  df$col = 1
  
  for(i in 1:length(df$edges)){
    if(df$edges[i]!=1){df$col[i]="coral2"}  
    if(df$edges[i]==1){df$col[i]="black"}
  }
  
  pc_control = true_pc
  pc_treat = estim_pc
  
  n.stream = dim(pc_control)[1]
  
  #Biniarise pc entries
  bin_pc_treat = bin_pc_control = matrix(0, n.stream, n.stream)
  bin_pc_control[pc_control!=0]=1
  bin_pc_treat[pc_treat!=0]=1
  
  
  #get indices of common edges
  conns_treat = t(bin_pc_treat)[lower.tri(bin_pc_treat, diag=T)]
  conns_contr = t(bin_pc_control)[lower.tri(bin_pc_control, diag=T)]
  
  idx_control = which(conns_contr==1)
  idx_treat = which(conns_treat ==1)
  
  idx_control %in% idx_treat
  
  if(truth==F){
    same_idx = which(idx_treat %in% idx_control)
    
    
    for(i in 1:length(df$col[same_idx])){
      if(df$col[same_idx][i]!="black"){df$col[same_idx][i]="navy"}
    }
  }
  
  if(truth==T){
    same_idx = which(idx_control %in% idx_treat)
    
    for(i in 1:length(df$col[same_idx])){
      if(df$col[same_idx][i]!="black"){df$col[same_idx][i]="navy"}
    }
  }
  
  for(i in 1:length(df$edges)){
    if(df$edges[i]==1){df$col[i] = "white"}
  }
  p = plot(g1, layout=layout.circle, edge.color=df$col, 
           vertex.color="white", vertex.label.color = "black")#, #edge.width = df$edges*1000)
  if(truth==T){title = "Pre-stimulus"} #"Theoretical/Control"
  else{title = "Post-stimulus"} #"Estimated/treatment"
  
  title(main = title, adj=0.5, cex.main=1, line=-2)
  return(list(p=p, pc=pc))
}

# 18. Function to count number of edges in igraph -------------------------

edges = function(mat){
  u = mat[upper.tri(mat, diag=F)]
  edges = length(u[u!=0])
  return(edges)
}






# 21. New iGraph plots ----------------------------------------------------

plot_UGM_new <- function(pc_list, title_list, points_list){
  
  upper = lapply(pc_list, function(x) {t(x)[lower.tri(x, diag=T)]})
  
  g1 = lapply(pc_list, function(x) graph_from_adjacency_matrix(x, mode="undirected", weighted=T))
  
  edges = lapply(upper, function(x){x[x!=0]})
  
  
  df = lapply(edges, function(x) data.frame(x))
  
  df = lapply(upper, function(x) data.frame(x))
  
  c = lapply(upper, function(x) which(x!=0)) #indices of non zero entries
  common = Reduce(intersect, c) #indices of common edges
  
  #check for commonalities among laser settings
  u2 = upper[-1] #get rid of list containing no laser estimates
  
  c2 = lapply(u2, function(x) which(x!=0)) #indices of non zero entries
  common2 = Reduce(intersect, c2)
  
  for(i in 1:length(pc_list)){
    df[[i]]$col = "white"
      
    for(j in 1:length(df[[i]]$x)){
      if(df[[i]]$x[j]!=1 & df[[i]]$x[j]!=0){df[[i]]$col[j] = "darkgrey"}
    }
    
    
    for(k in 1:length(df[[i]]$col[common])){
      if(df[[i]]$col[common][k]!="white"){df[[i]]$col[common][k] = "red"}
    }
    
  }
  
  for(i in 2:length(pc_list)){
    for(k in 1:length(df[[i]]$col[common2])){
      if(df[[i]]$col[common2][k]!="white"&df[[i]]$col[common2][k]!="red"){df[[i]]$col[common2][k] = "blue"}
    }
  }
  library(dplyr)
  df1 = lapply(df, function(x) filter(x, x>0))
  #curves <- autocurve.edges(g1[[1]])
  par(mfrow=c(1,1), mar=c(1,1,1,1), font.main = 1)
  for(i in 1:length(pc_list)){
    plot(g1[[i]], layout=layout.circle, edge.color=df1[[i]]$col, 
         vertex.color="white", vertex.label.color = "black", main ="", cex=2,vertex.size=20, 
         vertex.label.size=10, vertex.label.cex=1.3, family ="serif")
    title(title_list[i], cex.main = 2,  adj =0, line=-1, family="serif")
    title(points_list[i], cex.main = 2, adj =0, line = -24, family="serif")
  }
  
  
}

# 22. Heatmap plots -------------------------------------------------------
plot_heat_map = function(coherence_matrix, N.neuron, type){
  library(ggplot2)
  x = seq(1,N.neuron,1)
  y = seq(1,N.neuron,1)
  data = expand.grid(X=x,Y=y)
  data$Z <- as.vector(coherence_matrix)
  data$Z[data$Z==0] = NA #change zero entries to NAs
  
  #check if matrix is diagonal
  # g = na.omit(data$Z)[1]
  # if(g==1){
  #   data$Z = factor(data$Z)
  #   col.plot = c("white", "mistyrose")
  #   plt = ggplot(data, aes(X,Y)) + geom_tile(aes(fill = Z))  + scale_fill_manual(values = col.plot) +
  #     labs(title = type, fill = "Partial Coherence" , x = "", y="") + scale_y_reverse(breaks = seq(1,N.neuron+2, 3)) + scale_x_continuous(breaks = seq(1,N.neuron+2, 3))+
  #     theme(plot.title = element_text(size=14)) 
  # }
  
  plt = ggplot(data, aes(X,Y, fill=(Z))) + geom_tile()  + scale_fill_distiller(palette = "Reds", direction=+1, na.value = "white", limits = c(0,1))  +
    labs(title = type, fill = "Partial 
Coherence" , x = "", y="") + scale_y_reverse(breaks = seq(1,N.neuron+2, 3)) + scale_x_continuous(breaks = seq(1,N.neuron+2, 3))+
    theme(plot.title = element_text(size=15), axis.text = element_text(size=15),
          legend.title = element_text(size=10)) 
  #labs(title = paste("", f), fill = "Coherence" , x = "Neuron", y="Neuron") + scale_y_reverse()
  
  return(plt)
}


# 23. Processing Tuning Output --------------------------------------------

Processing_output = function(Pooled_s_list, res_list, band){
  
  out = list(); out = rep(list(out), length(Pooled_s_list))
  for(i in 1:length(Pooled_s_list)){
    out[[i]] <- format_res(res_list[[i]], Pooled_s_list[[i]])
  }
  
  pc_list = map(out, 1) #partial coherence for each laser setting
  lams = unlist(map(out, 2)) #lambdas for each laser setting
  tol = map(out, 3) #ADMM tolerances achieved at each setting
  eBIC = map(out, 4) #eBIC output for each laser setting
  
  lams
  no_edges = unlist(lapply(pc_list, edges))
  
  return(list(lams = lams, edges = no_edges, pc_list = pc_list, eBIC=eBIC, tol=tol))
}


# 24. Formatting tuning results -------------------------------------------

results = function(laser_on, b, band){
  
  #laser_on - T/F indicating whether laser on or not
  #b - index of frequency band - c(1,2,3,4) = (delta, theta, alpha...)
  #band - string indicating frequency band
  
  if(laser_on==T){
    Pooled_S_0  <- readRDS("~/luna/Paper_code/Section 5/Data/Laser On/(0,1)/S_OB_0.RDS")
    Pooled_S_10 <-readRDS("~/luna/Paper_code/Section 5/Data/Laser On/(0,1)/S_OB_10.RDS")
    Pooled_S_50 <-readRDS("~/luna/Paper_code/Section 5/Data/Laser On/(0,1)/S_OB_50.RDS")
    
    res_0 <- readRDS(paste0("~/luna/Paper_code/Section 5/Tuning/Results/Laser On/0/eBIC_",band,"_band.RDS"))
    res_10 <- readRDS(paste0("~/luna/Paper_code/Section 5/Tuning/Results/Laser On/10/eBIC_",band,"_band.RDS"))
    res_50 <- readRDS(paste0("~/luna/Paper_code/Section 5/Tuning/Results/Laser On/50/eBIC_",band,"_band.RDS"))
  }
  
  if(laser_on==F){
    Pooled_S_0  <-readRDS("~/luna/Paper_Code/Section 5/Data/Laser Off/S_OB_0.RDS")
    Pooled_S_10 <-readRDS("~/luna/Paper_Code/Section 5/Data/Laser Off/S_OB_10.RDS")
    Pooled_S_50 <- readRDS("~/luna/Paper_Code/Section 5/Data/Laser Off/S_OB_50.RDS")
    
    res_0 <- readRDS(paste0("~/luna/Paper_Code/Section 5/Tuning/Results/Laser Off/0/eBIC_",band,"_band.RDS"))
    res_10 <- readRDS(paste0("~/luna/Paper_Code/Section 5/Tuning/Results/Laser Off/10/eBIC_",band,"_band.RDS"))
    res_50 <- readRDS(paste0("~/luna/Paper_Code/Section 5/Tuning/Results/Laser Off/50/eBIC_",band,"_band.RDS"))
    
  }
  
  Pooled_s_list <- list(Pooled_S_0[[b]],  Pooled_S_10[[b]], Pooled_S_50[[b]])
  res_list <- list(res_0, res_10, res_50)
  
  # Main Results
  out = Processing_output(Pooled_s_list, res_list, band)
  
  return(out)
  
}




# 25. STEM BRITAIN --------------------------------------------------------
plot_UGM_new2 <- function(pc_list, title_list, points_list){
  
  upper = lapply(pc_list, function(x) {t(x)[lower.tri(x, diag=T)]})
  
  g1 = lapply(pc_list, function(x) graph_from_adjacency_matrix(x, mode="undirected", weighted=T))
  
  edges = lapply(upper, function(x){x[x!=0]})
  
  
  df = lapply(edges, function(x) data.frame(x))
  
  df = lapply(upper, function(x) data.frame(x))
  
  c = lapply(upper, function(x) which(x!=0)) #indices of non zero entries
  common = Reduce(intersect, c) #indices of common edges
  
  #check for commonalities among laser settings
  u2 = upper[-1] #get rid of list containing no laser estimates
  
  c2 = lapply(u2, function(x) which(x!=0)) #indices of non zero entries
  common2 = Reduce(intersect, c2)
  
  for(i in 1:length(pc_list)){
    df[[i]]$col = "white"
      
    for(j in 1:length(df[[i]]$x)){
      if(df[[i]]$x[j]!=1 & df[[i]]$x[j]!=0){df[[i]]$col[j] = "darkgrey"}
    }
    
    
    for(k in 1:length(df[[i]]$col[common])){
      if(df[[i]]$col[common][k]!="white"){df[[i]]$col[common][k] = "coral2"}
    }
    
  }
  
  for(i in 2:length(pc_list)){
    for(k in 1:length(df[[i]]$col[common2])){
      if(df[[i]]$col[common2][k]!="white"&df[[i]]$col[common2][k]!="coral2"){df[[i]]$col[common2][k] = "navy"}
    }
  }
  library(dplyr)
  df1 = lapply(df, function(x) filter(x, x>0))
  #curves <- autocurve.edges(g1[[1]])
  par(mfrow=c(1,3), mar=c(1,1,1,1), font.main = 1)
  for(i in 1:length(pc_list)){
    plot(g1[[i]], layout=layout.circle, edge.color=df1[[i]]$col, 
         vertex.color="white", vertex.label.color = "black", main ="", cex=2,vertex.size=20, vertex.label.size=10, vertex.label.cex=1.3,)#edge.curved = seq(-0.5,0.5,length(g1[[i]])))
    title(title_list[i], cex.main = 1.3,  adj =0, line=-1)
    title(points_list[i], cex.main = 1.3, adj =0, line = -18)
  }
  
  
}



# 26. Plotting Spike Density ----------------------------------------------





# 27. Function to plot ADMM Stopping Criteria -----------------------------

plot_ADMM = function(tol, ind){
  plot(seq(1, length(tol[[1]])-1), tol[[1]][-1], type='l', xlab = "No. Iterations", ylab = "Stopping Criteria", 
       main = paste0("Laser ", ind), family="serif", cex =1 , cex.lab =1, cex.axis =1, las =1, cex.main =1)
  cols = c("black", "red", "blue")
  for(i in 2:3){
    lines(seq(1, length(tol[[i]])-1), tol[[i]][-1], col=cols[i])
  }
}


# 28. Function to plot eBIC curves ----------------------------------------

plot_eBIC = function(eBIC, ind){
  lambdas = logspace(-3,1,100)
  par(mfrow=c(1,1), mar = c(5.1,4.1,4.1,2.1), font.main = 1)
  eBIC = lapply(eBIC, Re)
  a = min(unlist(lapply(eBIC, min))); b = max(unlist(lapply(eBIC, max)))
  plot(lambdas, eBIC[[1]], xlab = expression(lambda), ylab = "eBIC", type="l", main = paste0("Laser ",ind), 
       ylim = c(a, b), family="serif", cex =1 , cex.lab =1, cex.axis =1, cex.main =1,  las =1)
  cols = c("black", "red", "blue")
  for(i in 2:3){
    lines(lambdas, eBIC[[i]], col=cols[i])
  }
}


# 29. Function to plot regularised Estimate -----------------------------------

plot_reg_power = function(res, intensity){
  
  #res - output from glasso estimator
  #intensity - 0,10 or 50 mW/mm^2
  
  x = map(res,2) #get thetas
  reg_s = lapply(x, solve)
  om = seq(1,50)*2*pi
  print(plot_power_spec(reg_s, om, intensity , T))
  return(reg_s)
}


# 30. Function to plot individual spectral --------------------------------

plot_individual_spec = function(S_hat, reg_S, freqs, plot_title, neuron){
  #Function to plot power spectra as shown in Figure 3
  #Inputs -
  #       S_hat       - Periodogram Estimate
  #       freqs       - considered frequencies of interest (omega)
  #       plot_title  - title of plot
  #       neuron      - specified data neuron
  #Outputs - 
  #                   - Plot of power spectra
  
  power = lapply(S_hat, diag); pspec=list();
  power = lapply(power, Re) #get rid of +0i's
  
  power2 = lapply(reg_S, diag);
  power2 = lapply(power2, Re);
  
    pspec = unlist(map(power, neuron))
    
    pspec2 = unlist(map(power2, neuron))
  
  
    f = freqs/(2*pi)
    plot(f, pspec, type='l',  col="black", main=plot_title, 
         ylab =expression(hat(S)(omega)), xlab = "Frequency (Hz)" , 
         ylim = c(0,19) , cex.main = 1, cex.axis = 1, cex.lab = 1, family = "serif", las =1)
    
    lines(f, pspec2, col="red")
 
    
  }



