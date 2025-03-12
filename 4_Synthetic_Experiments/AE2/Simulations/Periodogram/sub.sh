#!/bin/bash

#SBATCH -J AE2_periodogram
#SBATCH -c 10
#SBATCH --array=1-2%2
#SBATCH -e s%.e
#SBATCH -o s%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/RSE/4_Synthetic_exp/AE2/sims/periodogram/sims.R $x
