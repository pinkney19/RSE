#!/bin/bash

#SBATCH -J AE_tuning_glasso
#SBATCH -c 20
#SBATCH --array=1-6%4
#SBATCH -e t%.e
#SBATCH -o t%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/RSE/4_Synthetic_exp/AE/tuning/lasso/Tuning_AE_lasso.R $x
