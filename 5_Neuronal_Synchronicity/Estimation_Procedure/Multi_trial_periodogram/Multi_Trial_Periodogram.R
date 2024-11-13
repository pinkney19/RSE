# Periodogram Estimates ---------------------------------------------------


# Load libraries ----------------------------------------------------------

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

setwd("~/Downloads/RSE/5_Neuronal_Synchronicity")
source("Functions_Section_5.R")


# Load Data ---------------------------------------------------------------
# stimulus
OB_0_stim <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Processed_Data/Laser_on/OB_0.RDS")
OB_10_stim <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Processed_Data/Laser_on/OB_10.RDS")
OB_50_stim <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Processed_Data/Laser_on/OB_50.RDS")

# no stimulus
OB_0_no_stim <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Processed_Data/Laser_off/OB_0.RDS")
OB_10_no_stim <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Processed_Data/Laser_off/OB_10.RDS")
OB_50_no_stim <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Processed_Data/Laser_off/OB_50.RDS")

# Considered frequencies  -------------------------------------------------

om= seq(1,50) *2*pi

# Trial-frequency smoothed periodogram estimates --------------------------

# stimulus
S1 = Trial_freq_Estimator(om, OB_0_stim, 1)
S3 = Trial_freq_Estimator(om, OB_10_stim, 1)
S4 = Trial_freq_Estimator(om, OB_50_stim, 1)


setwd("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Multi_trial_periodogram/Laser_on_estimates")
saveRDS(S1$pooled_s, "S_OB_0.RDS")
saveRDS(S3$pooled_s, "S_OB_10.RDS")
saveRDS(S4$pooled_s, "S_OB_50.RDS")

# no stimulus
S1 = Trial_freq_Estimator(om, OB_0_no_stim, 3)
S3 = Trial_freq_Estimator(om, OB_10_no_stim, 3)
S4 = Trial_freq_Estimator(om, OB_50_no_stim, 3)

setwd("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Multi_trial_periodogram/Laser_off_estimates")
saveRDS(S1$pooled_s, "S_OB_0.RDS")
saveRDS(S3$pooled_s, "S_OB_10.RDS")
saveRDS(S4$pooled_s, "S_OB_50.RDS")
setwd("~/Downloads/RSE/5_Neuronal_Synchronicity/Estimation_Procedure/Multi_trial_periodogram")
saveRDS(S4$f, "freqs.RDS")
