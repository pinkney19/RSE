#!/bin/bash

#SBATCH -J sims_ebic_10
#SBATCH -c 31
#SBATCH --array=1-9%3
#SBATCH -e s%.e
#SBATCH -o s%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/reviews/sims/graphical/10/Simulations_eBIC.R $x
