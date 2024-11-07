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

#freqs = seq(from= -0.05,to= 0.1,length.out = 100)

S_hat = lapply(freqs, function(x) periodogram(times, x, T_prime))
# no mean correction
S_hat_raw = lapply(freqs, function(x) raw_periodogram(times, x, T_prime))


# Example with unit 1 
par(mfrow=c(1,2), mar = c(5.1,4.7,4.1,2.1))
S1 = map(S_hat, 1)
S1_raw = map(S_hat_raw,1)
plot(freqs, unlist(S1_raw), col="red", xlab=expression(omega), ylab = expression(hat(S)[11](omega)), type='l',cex.axis = 1.5, cex.lab=1, lwd=1, las = 1, cex.axis =1, family = "serif")
lines(freqs, unlist(S1), xlab=expression(omega), ylab = expression(hat(S)[11](omega)), type='l')
abline(h = lambda/(2*pi), col="blue", lty="dashed", lwd = 2)
abline(v = 0.0628, col="darkgreen", lty = "dashed", lwd = 2)
legend("topleft",c("Mean Corrrected", "Raw Periodogram", "True spectrum", expression(omega=0.0628)),  col=c("black","red", "blue", "darkgreen"), lty = c(1,1, 2,2), cex = 0.8)
title(main = "")

# zoomed in 

plot(freqs, unlist(S1_raw), col="red", xlab=expression(omega), ylab = expression(hat(S)[11](omega)), type='l', ylim = c(0,4000), cex.axis = 1.5, cex.lab=1, lwd=1, las = 1, cex.axis =1, family = "serif")
lines(freqs, unlist(S1), xlab=expression(omega), ylab = expression(hat(S)[11](omega)), type='l')
abline(h = lambda/(2*pi), col="blue", lty="dashed", lwd = 2)
abline(v = 0.0628, col="darkgreen", lty = "dashed", lwd = 2)
legend("topleft",c("Mean Corrrected", "Raw Periodogram", "True spectrum", expression(omega=0.0628)),  col=c("black","red", "blue", "darkgreen"), lty = c(1,1, 2,2), cex = 0.8)

par(mfrow=c(1,1))
plot(freqs, unlist(S1), xlab=expression(omega), ylab = expression(hat(S)[11](omega)), type='l',cex.axis = 1.5, cex.lab=1, lwd=1, las = 1, cex.axis =1, family = "serif")
abline(h = lambda/(2*pi), col="blue", lty="dashed", lwd = 2)
abline(v = 0.0628, col="darkgreen", lty = "dashed", lwd = 2)
