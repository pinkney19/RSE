
# load data ---------------------------------------------------------------

# Graphical
g_a <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/res_a.RDS")
g_b <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/res_b.RDS")
g_c <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Graphical/res_c.RDS")

# Ridge 
r_a <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Ridge/res_a.RDS")
r_b<- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Ridge/res_b.RDS")
r_c <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Ridge/res_c.RDS")


# Mean-squared Errors -----------------------------------------------------

# ridge estimates
Ridge = c(r_a, r_b, r_c)

# lasso 1 estimates - mse
Lasso1 = c(g_a$tab_mse[,1] , g_b$tab_mse[,1] , g_c$tab_mse[,1])

# lasso 2 estimates - f1
Lasso2 = c(g_a$tab_mse[,2] , g_b$tab_mse[,2] , g_c$tab_mse[,2])

# lasso 3 estimates - ebic
Lasso3 = c(g_a$tab_mse[,3] , g_b$tab_mse[,3] , g_c$tab_mse[,3])

MSE_table = cbind(Ridge, Lasso1, Lasso2, Lasso3)
MSE_table = as.data.frame(MSE_table)

round(MSE_table, 2)

# F1 scores ---------------------------------------------------------------

# lasso 1 estimates - mse
Lasso1 = c(g_a$tab_f1[,1] , g_b$tab_f1[,1] , g_c$tab_f1[,1])

# lasso 2 estimates - f1
Lasso2 = c(g_a$tab_f1[,2] , g_b$tab_f1[,2] , g_c$tab_f1[,2])

# lasso 3 estimates - ebic
Lasso3 = c(g_a$tab_f1[,3] , g_b$tab_f1[,3] , g_c$tab_f1[,3])

f1_table = cbind(Lasso1, Lasso2, Lasso3)
f1_table = as.data.frame(f1_table)

round(f1_table,2)


# Periodogram -------------------------------------------------------------

res_periodogram <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Simulations/Periodogram/res_periodogram.RDS")

data_per = c(0, res_periodogram[1], 0, res_periodogram[2], 0, 0, 0, res_periodogram[3], 0, res_periodogram[4],0, 0, 0, res_periodogram[5], 0, res_periodogram[6], 0, 0)

library(dplyr)
MSE_table <- MSE_table %>% mutate(Periodogram = data_per)
# reorder 
MSE_table <- MSE_table %>%
  select(Periodogram, everything())


# add trial info
m=c(10,50)
m = rep(m,9)
p = c(12,12,48,48,96,96)
p = rep(p, 3)
round(MSE_table,2)

MSE_table <- MSE_table %>% mutate(m = m)
# reorder 
MSE_table <- MSE_table %>% select(m, everything())

MSE_table <- MSE_table %>% mutate(p = p)
# reorder 
MSE_table <- MSE_table %>% select(p, everything())

# add f1 scores

new_tab = cbind(MSE_table, f1_table)


# to latex
library(xtable)
xtable(new_tab)



# Table for AUROC and F1 scores -------------------------------------------


# lasso 1 estimates - mse
Lasso1 = c(g_a$tab_f1[,1] , g_b$tab_f1[,1] , g_c$tab_f1[,1])

# lasso 2 estimates - f1
Lasso2 = c(g_a$tab_f1[,2] , g_b$tab_f1[,2] , g_c$tab_f1[,2])

# lasso 3 estimates - ebic
Lasso3 = c(g_a$tab_f1[,3] , g_b$tab_f1[,3] , g_c$tab_f1[,3])

f1_table = cbind(Lasso1, Lasso2, Lasso3)
f1_table = as.data.frame(f1_table)

round(f1_table,2)

#AUROC
# lasso 1 estimates - mse
lasso1 = c(g_a$tab_auroc[,1] , g_b$tab_auroc[,1] , g_c$tab_auroc[,1])

# lasso 2 estimates - f1
lasso2 = c(g_a$tab_auroc[,2] , g_b$tab_auroc[,2] , g_c$tab_auroc[,2])

# lasso 3 estimates - ebic
lasso3 = c(g_a$tab_auroc[,3] , g_b$tab_auroc[,3] , g_c$tab_auroc[,3])

auroc_table = cbind(lasso1, lasso2, lasso3)
auroc_table =  as.data.frame(auroc_table)

final_table = cbind(MSE_table, auroc_table)
xtable(final_table)


model_selection = (cbind(f1_table, auroc_table))
model_selection <- model_selection %>% mutate(m = m)
# reorder 
model_selection <- model_selection %>% select(m, everything())

model_selection <- model_selection %>% mutate(p = p)
# reorder 
model_selection <- model_selection %>% select(p, everything())
model_selection
xtable(model_selection)
