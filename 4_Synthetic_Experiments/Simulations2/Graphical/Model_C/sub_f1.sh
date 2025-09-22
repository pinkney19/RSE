#!/bin/bash

#SBATCH -J mc_sims_f1
#SBATCH -c 31
#SBATCH --array=1-6%3
#SBATCH -e s%.e
#SBATCH -o s%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/RSE/4_Synthetic_exp/sims/graphical/sims_F1.R $x
