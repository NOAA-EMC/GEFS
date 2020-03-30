#!/bin/ksh

set -x
export IOBUF_PARAMS=*:size=64M:count=4:verbose

ulimit -s unlimited
ulimit -a

export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:4

#export NODES=$SLURM_JOB_NUM_NODES
#export total_tasks=$SLURM_NTASKS
#export OMP_NUM_THREADS=4
#export taskspernode=$SLURM_CPUS_ON_NODE

export DO_LOW_RES=

# export for development runs only begin
export RERUN=NO

# CALL executable job script here
. $SOURCEDIR/jobs/JGEFS_PRDGEN
