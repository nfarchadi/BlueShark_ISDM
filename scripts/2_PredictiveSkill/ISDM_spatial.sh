#!/bin/bash -l

# setting name of job
#SBATCH --job-name=ISDMspatial_practice

# setting home directory
#SBATCH -D /home/njfarcha/bsh_ISDM

# setting standard error output
#SBATCH -e /home/njfarcha/bsh_ISDM/slurm_log/sterror_ISDMSpatial_%j.txt

# setting standard output
#SBATCH -o /home/njfarcha/bsh_ISDM/slurm_log/stdoutput_ISDMSpatial_%j.txt

# setting medium priority
#SBATCH -p med2

# setting the number of CPUs (I was to request 8 CPUs and 8GB of RAM from 1 node)
#SBATCH --cpus-per-task=20

# setting the number of nodes
#SBATCH --nodes=1

# setting the memory -- not sure what this should be honestly (copying the CSE wiki) but I think it is plenty
#SBATCH --mem=20G

# setting the number of tasks
#SBATCH --ntasks=1

# setting the max time
#SBATCH -t 10:00:00

# mail alerts at the begining and end of job
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

# send mail here
#SBATCH --mail-user=nfarchadi@sdsu.edu

# now we'll print out the contents of the R script to the output file
cat scripts/2_PredictiveSkill/ISDM_spatial.r
echo "ok now for the actual standard output"


# now running the actual script!

# load R
module load R

srun Rscript scripts/2_PredictiveSkill/ISDM_spatial.r