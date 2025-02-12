#!/bin/bash

#SBATCH -J tunc
#SBATCH -c 31
#SBATCH --array=1-7%3
#SBATCH -e t%.e
#SBATCH -o t%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/RSE/4_Synthetic_exp/tuning/graphical/Tuning_model_c.R $x
