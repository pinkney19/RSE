
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

OB_0 <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Pre_processing/OB_0.RDS")
OB_1 <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Pre_processing/OB_1.RDS")
OB_5 <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Pre_processing/OB_5.RDS")
OB_10 <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Pre_processing/OB_10.RDS")
OB_20 <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Pre_processing/OB_20.RDS")
OB_30 <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Pre_processing/OB_30.RDS")
OB_40 <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Pre_processing/OB_40.RDS")
OB_50 <- readRDS("~/Downloads/RSE/5_Neuronal_Synchronicity/Pre_processing/OB_50.RDS")




# Re-sructure Data --------------------------------------------------------

OB_0 <- pre_processing_function(OB_0)
OB_1 <- pre_processing_function(OB_1)
OB_5 <- pre_processing_function(OB_5)
OB_10 <- pre_processing_function(OB_10)
OB_20 <- pre_processing_function(OB_20)
OB_30 <- pre_processing_function(OB_30)
OB_40 <- pre_processing_function(OB_40)
OB_50 <- pre_processing_function(OB_50)


# Spike Density Plots -----------------------------------------------------

par(mfrow=c(2,4),mar=c(6,7,4,2)+1.3, mgp=c(5.5,1.5,0))
spike_density(OB_0[[5]], 0.01, -5, 10, expression(0~mW/mm^2), 0, 300)
spike_density(OB_1[[5]], 0.01, -5, 10, expression(1~mW/mm^2), 0, 300)
spike_density(OB_5[[5]], 0.01, -5, 10, expression(5~mW/mm^2), 0, 300)
spike_density(OB_10[[5]], 0.01, -5, 10, expression(10~mW/mm^2), 0, 300)
spike_density(OB_20[[5]], 0.01, -5, 10, expression(20~mW/mm^2), 0, 300)
spike_density(OB_30[[5]], 0.01, -5, 10, expression(30~mW/mm^2), 0, 300)
spike_density(OB_40[[5]], 0.01, -5, 10, expression(40~mW/mm^2), 0, 300)
spike_density(OB_50[[5]], 0.01, -5, 10,  expression(50~mW/mm^2), 0, 300)



# Extract data for laser on/off scenarios ---------------------------------



Extract_data = function(data){
  stim1 = stim2 = bet_stim = list();
  for(i in 1:10){ #i in 1:10
    stim1[[i]] = data[[i]][data[[i]]>0 & data[[i]]<=1]#data on interval (0,1)
    stim2[[i]] = data[[i]][data[[i]]>9 & data[[i]]<=10] #data on interval (9,10)
    bet_stim[[i]] = data[[i]][data[[i]]>1 & data[[i]]<=4] #data on interval (1,4)
  }
  return(list(stim1 = stim1, stim2 = stim2, bet_stim = bet_stim))
}


Get_stim_data = function(data){
  res = lapply(data, Extract_data)
  stim1 = map(res,1) #data for (0,1)  interval stimulus
  stim2 = map(res,2) #data for  (9,10) interval - i.e. after stimulus
  bet_stim = map(res, 3) #data for (1,9) interval - i.e. before stimulus
  
  return(list(x = stim1, y = stim2, z = bet_stim))
}

OB_0 <- Get_stim_data(OB_0)
OB_0_stim <- OB_0$x

OB_10 <- Get_stim_data(OB_10)
OB_10_stim <- OB_10$x#, OB_10$y) 

OB_50 <- Get_stim_data(OB_50)
OB_50_stim <- OB_50$x #, OB_50$y) 

# function to check if numeric(0) is in the list
# is_empty <- function(lst) {
#   any(sapply(lst, function(x) is.numeric(x) && length(x) == 0))
# }
# 
# # Check if numeric(0) is in the nested list
# which(sapply(OB_0_stim, is_empty))
# which(sapply(OB_10_stim, is_empty))
# which(sapply(OB_50_stim, is_empty))



# Count Points ------------------------------------------------------------
count_points = function(data){
  count = NULL;
  for(i in 1:26){
    count[i] = mean(unlist(lapply(data[[i]], length)))
  }
  return(count)
}



var = rbind(count_points(OB_0_stim), count_points(OB_10_stim), count_points(OB_50_stim))

counts_tab = cbind(count_points(OB_0_stim),  count_points(OB_10_stim), count_points(OB_50_stim))
library(xtable)
xtable(counts_tab)


par(mfrow=c(1,1))

barplot(var, col = c("deepskyblue", "lightblue", "cyan2"), main = "Counts in (0,1)", xlab = "Neuron", 
        ylab = "Avergae No. of Events", names.arg = seq(1,26),beside = T)

legend("topright", c("0 mW/mm","10 mW/mm", "50 mW/mm"), fill = c("deepskyblue", "lightblue", "cyan2"), cex =0.8)




# Interval (1,94) ----------------------------------------------------------

OB_0_no_stim <- OB_0$z
OB_10_no_stim <- OB_10$z
OB_50_no_stim <- OB_50$z

var2 = rbind(count_points(OB_0_no_stim), count_points(OB_10_no_stim), count_points(OB_50_no_stim))

counts_tab = cbind(count_points(OB_0_no_stim), count_points(OB_10_no_stim), count_points(OB_50_no_stim))
library(xtable)
xtable(counts_tab)


par(mfrow=c(1,1))
barplot(var2, col = c("deepskyblue", "lightblue", "cyan2"), main = "Counts in (1,4)", xlab = "Neuron", 
        ylab = "Avergae No. of Events", names.arg = seq(1,26),beside = T)

legend("topright", c("0 mW/mm","10 mW/mm", "50 mW/mm"), fill = c("deepskyblue", "lightblue", "cyan2"), cex =0.8)



# Average No. of points ---------------------------------------------------


count_points2 = function(data1){
  trials = counts = sums = points = list();
  for(i in 1:10){
    trials[[i]] = map(data1, i)
    counts[[i]] = lapply(trials[[i]], length) #count no. of points in each data stream for trial i
    points[[i]] = sum(unlist(counts[[i]])) #total sum across dimensions 
  }
  av = mean(unlist(points))
  
  return(round(av))
}


count_points2(OB_0_stim)
count_points2(OB_10_stim)
count_points2(OB_50_stim)

count_points2(OB_0_no_stim)
count_points2(OB_10_no_stim)
count_points2(OB_50_no_stim)


# Save Data ---------------------------------------------------------------

setwd("~/Downloads/RSE/5_Neuronal_Synchronicity/Processed_Data/Laser_on")
saveRDS(OB_0_stim, "OB_0.RDS")
saveRDS(OB_10_stim, "OB_10.RDS")
saveRDS(OB_50_stim, "OB_50.RDS")

setwd("~/Downloads/RSE/5_Neuronal_Synchronicity/Processed_Data/Laser_off")
saveRDS(OB_0_no_stim, "OB_0.RDS")
saveRDS(OB_10_no_stim, "OB_10.RDS")
saveRDS(OB_50_no_stim, "OB_50.RDS")