#!/bin/bash

#SBATCH -J low_rank_tuning_glasso
#SBATCH -c 21
#SBATCH --array=1-6%4
#SBATCH -e t%.e
#SBATCH -o t%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/RSE/4_Synthetic_exp/low_rank/tuning/glasso/Tuning_low_rank_lasso.R $x
