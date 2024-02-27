#!/bin/bash
#SBATCH -J eco-KGML-model-runs
#SBATCH --account=flare
#SBATCH --partition=normal_q
#SBATCH --nodes=1 --ntasks-per-node=1 --cpus-per-task=25 # this requests 1 node, 1 core.
#SBATCH --time=0-48:00:00 # 48 hr

module load containers/singularity
singularity exec --bind=/home/melofton/FCR-GLM-metrics:/home/rstudio/FCR-GLM-metrics /projects/rqthomas_lab/eco4cast_containers/rocker-flare_4.3.0.sif /bin/bash run_r.sh
