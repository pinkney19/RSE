#!/bin/bash

#SBATCH -J large_p
#SBATCH -c 45
#SBATCH --array=1-2%2
#SBATCH -e ml%.e
#SBATCH -o ml%.out
#SBATCH --mem-per-cpu=4G 
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/reviews/50_trials/Tuning_larger.R $x
