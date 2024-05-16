#!/bin/bash
#SBATCH --job-name=BOL7004Job
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --partition=qbiol
#SBATCH --mem=2GB              # memory (MB)
#SBATCH --time=0-1:00          # time (D-HH:MM)
#SBATCH -o BOL7004Job.%N.%j.out     # STDOUT
#SBATCH -e BOL7004Job.%N.%j.err     # STDERR



echo "Start time: "; date

module load applications/R/4.0.3

Rscript --vanilla ~/For_HPC_4.R 

echo "End time: "; date