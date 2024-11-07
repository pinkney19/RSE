
# zero-frequency investigation --------------------------------------------

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
freqs = 0.0628 #chosen w.l.o.g since we are looking at poisson process.
MC_samples = 1000; #no. Monte Carlo samples
p = 20 #no. of dimensions/data streams


times = pprocess(lambda, T_prime, m,p) #simulate times
freqs = seq(from= -0.1,to= 0.1,length.out = 100)

S_hat = lapply(freqs, function(x) periodogram(times, x, T_prime))
# no mean correction
S_hat_no_corr = lapply(freqs, function(x) periodogram_no_mean_corr(times, x, T_prime))
par(mfrow=c(1,2), mar = c(5.1,4.5,4.1,2.1))

plot_power = function(S_hat, a,b, title){

  
  # Example with unit 1 
  check = map(S_hat, 1)
  plot(freqs, unlist(check), ylim=c(a,b), xlab=expression(omega), ylab = expression(hat(S)[11](omega)), type='l')
  abline(h = lambda/(2*pi), col="blue", lty="dashed")
  abline(v = 0.0628, col="red", lty = "dashed")
  legend("topleft",c("Estimated Spectum", "True spectrum", expression(omega=0.0628)),  col=c("black","blue", "red"), lty = c(1, 2,2), cex = 0.8)
  title(main = title)
}

plot_power(S_hat, -10,150, "Mean Corrected")
plot_power(S_hat_no_corr, -10,150, "No Mean Correction")


#S_hat_paper = lapply(freqs, function(x) periodogram_up(times, x, T_prime))
#plot_power(S_hat_paper, -10, 1000, "Paper")


