#!/bin/sh 
### General options 
### -- specify queue -- 
#BSUB -q hpc
### -- set the job Name -- 
#BSUB -J bioreader.R
### -- ask for 1 core -- 
#BSUB -n 5
### -- specify that we need 2GB of memory per core/slot -- 
#BSUB -R "rusage[mem=40GB]"
### -- specify that we want the job to get killed if it exceeds 3 GB per core/slot -- 
#BSUB -M 50GB
### -- set walltime limit: hh:mm -- 
#BSUB -W 24:00 
### -- set the email address -- 
# please uncomment the following line and put in your e-mail address,
# if you want to receive e-mail notifications on a non-default address
#BSUB -u s193518@dtu.dk
### -- send notification at start -- 
#BSUB -B 
### -- send notification at completion -- 
#BSUB -N 
### -- Specify the output and error file. %J is the job-id -- 
### -- -o and -e mean append, -oo and -eo mean overwrite -- 
#BSUB -o Output_bioreader_%J.out 
#BSUB -e Output_bioreader_%J.err

module load R/4.2.1-mkl2022update1
#export TMPDIR=Path_to_your_scratch_directory
export R_BATCH_OPTIONS="--no-save"
# -- commands you want to execute -- # 
R CMD BATCH main_test.R
