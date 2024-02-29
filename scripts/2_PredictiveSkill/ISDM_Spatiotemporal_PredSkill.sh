#!/bin/bash -l

# setting name of job
#SBATCH --job-name=ISDM_spatiotemporal_skill

# setting home directory
#SBATCH -D /home/njfarcha/bsh_ISDM

# setting standard error output
#SBATCH -e /home/njfarcha/bsh_ISDM/slurm_log/sterror_ISDMSpatiotemporal_%j.txt

# setting standard output
#SBATCH -o /home/njfarcha/bsh_ISDM/slurm_log/stdoutput_ISDMSpatiotemporal_%j.txt

# setting medium priority
#SBATCH -p med2

# setting the number of CPUs (I want to request 20 CPUs and 20GB of RAM from 1 node)
#SBATCH --cpus-per-task=20

# setting the number of nodes
#SBATCH --nodes=1

# setting the memory -- 20GB seems to work well for these models. before I was using --mem (mem per node) but going to try --mem-per-cpu
#SBATCH --mem-per-cpu=2G

# setting the number of tasks
#SBATCH --ntasks=1

# setting the max time
#SBATCH -t 168:00:00

# mail alerts at the begining and end of job
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

# send mail here
#SBATCH --mail-user=nfarchadi@sdsu.edu

# now we'll print out the contents of the R script to the output file
cat scripts/2_PredictiveSkill/ISDM_Spatiotemporal_PredSkill.r
echo "ok now for the actual standard output"


# now running the actual script!

# load R
module load R

srun Rscript scripts/2_PredictiveSkill/ISDM_Spatiotemporal_PredSkill.r