#!/bin/bash
#SBATCH -J flare_ler
#SBATCH --account=flare
#SBATCH --partition=normal_q
#SBATCH --nodes=1 --ntasks-per-node=1 --cpus-per-task=8 # this requests 1 node, 1 core.
#SBATCH --time=0-48:00:00 # 10 minutes

module load containers/singularity
singularity exec --bind=/home/melofton/FCR-GLM-metrics:/home/rstudio/FCR-GLM-metrics /projects/rqthomas_lab/eco4cast_containers/rocker-neon4cast_4.2.1.sif /bin/bash run_r.sh
