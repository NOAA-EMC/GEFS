#!/bin/ksh

# EXPORT list here
set -x
export NODES=1
export IOBUF_PARAMS=


ulimit -s unlimited
ulimit -a




export OMP_NUM_THREADS=4

#export OMP_NUM_THREADS=4

export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:4

export total_tasks=1
export OMP_NUM_THREADS=4
export taskspernode=1

#export total_tasks=1
#export PDY=20160415
#export cyc=00
#export cyc_fcst=00
#export job=Aa2016041500200
#export RUNMEM=gec00
export FORECAST_SEGMENT=hr

# export for development runs only begin
export APRUNC="mpirun"
export aprun_gec00="mpirun -np 1"
export NTHREADS_SIGCHGRS=6

cd $SOURCEDIR/control

#. $SOURCEDIR/parm/gefs.parm

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_SIGCHGRS

