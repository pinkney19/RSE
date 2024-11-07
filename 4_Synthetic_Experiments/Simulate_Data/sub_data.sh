#!/bin/bash

#SBATCH -J sims_data
#SBATCH -c 31
#SBATCH --array=1-6%3
#SBATCH -e t%.e
#SBATCH -o t%.out
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/RSE/4_Synthetic_exp/Sim_Data/Simulate_Data.R $x
