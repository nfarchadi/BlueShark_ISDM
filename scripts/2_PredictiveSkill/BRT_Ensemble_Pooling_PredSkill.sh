#!/bin/bash -l

# setting name of job
#SBATCH --job-name=BRTpredskill

# setting home directory
#SBATCH -D /home/njfarcha/bsh_ISDM

# setting standard error output
#SBATCH -e /home/njfarcha/bsh_ISDM/slurm_log/sterror_BRTpredskill_%j.txt

# setting standard output
#SBATCH -o /home/njfarcha/bsh_ISDM/slurm_log/stdoutput_BRTpredskill_%j.txt

# setting medium priority
#SBATCH -p med2

# setting the max time
#SBATCH -t 10:00:00

# mail alerts at the begining and end of job
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END

# send mail here
#SBATCH --mail-user=nfarchadi@sdsu.edu

# now we'll print out the contents of the R script to the output file
cat scripts/2_PredictiveSkill/BRT_Ensemble_Pooling_PredSkill.r
echo "ok now for the actual standard output"


# now running the actual script!

# load R
module load R

srun Rscript scripts/2_PredictiveSkill/BRT_Ensemble_Pooling_PredSkill.r