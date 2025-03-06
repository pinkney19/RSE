#!/bin/bash

#SBATCH -J low_rank_lasso_ebic_sims
#SBATCH -c 20
#SBATCH --array=1-6%4
#SBATCH -e s%.e
#SBATCH -o s%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/RSE/4_Synthetic_exp/low_rank/sims/lasso/ebic_sims/sims_ebic.R $x
