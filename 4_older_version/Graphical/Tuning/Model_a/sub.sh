#!/bin/bash

#SBATCH -J tuning_model_1
#SBATCH -c 31
#SBATCH --array=1-3%3
#SBATCH -e ma%.e
#SBATCH -o ma%.out
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/reviews/model_a/Tuning.R $x
