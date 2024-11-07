#!/bin/bash

#SBATCH -J tuning_ridge_10
#SBATCH -c 31
#SBATCH --array=1-9%3
#SBATCH -e r%.e
#SBATCH -o r%.out
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/reviews/Ridge/tuning/Tuning_ridge_10.R $x
