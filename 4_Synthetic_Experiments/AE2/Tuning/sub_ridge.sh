#!/bin/bash

#SBATCH -J AE_tuning_ridge
#SBATCH -c 21
#SBATCH --array=1-6%4
#SBATCH -e t%.e
#SBATCH -o t%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/RSE/4_Synthetic_exp/AE2/tuning/ridge/Tuning_AE_ridge.R $x
