#!/bin/bash

#SBATCH -J low_rank_sim_data
#SBATCH -c 20
#SBATCH --array=1-2%2
#SBATCH -e t%.e
#SBATCH -o t%.out
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/RSE/4_Synthetic_exp/low_rank/Sim_Data/sims/Simulating_Data.R $x
