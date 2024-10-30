#!/bin/bash

#SBATCH -J 50_tuning
#SBATCH -c 31
#SBATCH --array=1-9%3
#SBATCH -e m50%.e
#SBATCH -o m50%.out
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/reviews/50_trials/Tuning.R $x
