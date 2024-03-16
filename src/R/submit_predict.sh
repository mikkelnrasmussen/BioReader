#!/bin/sh
### General options
### -- specify queue -- 
#BSUB -q hpc
### -- set the job Name --
#BSUB -J bioreader_predict
### -- ask for number of cores -- 
#BSUB -n 1
### -- specify that the cores must be on the same host -- 
#BSUB -R "span[hosts=1]"
### -- specify that we need XGB of memory per core/slot -- 
#BSUB -R "rusage[mem=50GB]"
### -- specify that we want the job to get killed if it exceeds X GB per core/slot -- 
#BSUB -M 52GB
### -- set walltime limit: hh:mm -- 
#BSUB -W 03:00
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
#BSUB -e log/bioreader_%J.err
#BSUB -o log/bioreader_%J.out
module load R/4.3.1-mkl2023update1
export R_BATCH_OPTIONS="--no-save"
Rscript src/R/model_prediction.R -i data/test_articles_to_classify.csv
