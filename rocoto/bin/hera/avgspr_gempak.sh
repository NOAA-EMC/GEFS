#!/bin/ksh
#

# EXPORT list here
set -x
export IOBUF_PARAMS=


ulimit -s unlimited
ulimit -a

export MP_SHARED_MEMORY=no
export MEMORY_AFFINITY=core:6

#export NODES=$SLURM_JOB_NUM_NODES
#export total_tasks=$SLURM_NTASKS
#export OMP_NUM_THREADS=1
#export taskspernode=$SLURM_CPUS_ON_NODE

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_AVGSPR_GEMPAK

