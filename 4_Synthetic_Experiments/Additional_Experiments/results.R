res_low_rank_lasso <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Lasso/res_low_rank_lasso.RDS")
round(res_low_rank_lasso$tab_mse, 2)
res_lowrank_ridge <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Ridge/res_lowrank_ridge.RDS")

round(res_lowrank_ridge$mse, 2)
res_low_rank_periodogram <- readRDS("~/Downloads/RSE/4_Synthetic_Experiments/Additional_Experiments/Simulations/Periodogram/res_low_rank_periodogram.RDS")
round(res_low_rank_periodogram,2)
