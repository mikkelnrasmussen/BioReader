#!/bin/sh 
### General options 
### -- specify queue -- 
#BSUB -q hpc
### -- set the job Name -- 
#BSUB -J bioreader_class_upsamling
### -- ask for 1 core -- 
#BSUB -n 32
### -- specify that the cores must be on the same host -- 
#BSUB -R "span[hosts=1]"
### -- specify that we need 2GB of memory per core/slot -- 
#BSUB -R "rusage[mem=15GB]"
### -- specify that we want the job to get killed if it exceeds 3 GB per core/slot -- 
#BSUB -M 15.5GB
### -- set walltime limit: hh:mm -- 
#BSUB -W 50:00 
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
#BSUB -o bioreader_%J.out 
#BSUB -e bioreader_%J.err

module load R/4.3.1-mkl2023update1

#export TMPDIR=Path_to_your_scratch_directory
export R_BATCH_OPTIONS="--no-save"
mkdir -p time/
# -- commands you want to execute -- # 
/usr/bin/time -v -o time/bioreader.time Rscript src/R/minimal_example.R -a class -p --hpc -d _with_upsamling
