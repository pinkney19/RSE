#!/bin/bash

#SBATCH -J tuning_model_3
#SBATCH -c 31
#SBATCH --array=1-3%3
#SBATCH -e mc%.e
#SBATCH -o mc%.out
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/reviews/model_c/10_trials/Tuning.R $x
