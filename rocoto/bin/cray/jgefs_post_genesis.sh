#!/bin/ksh

# EXPORT list here
set -x
export NODES=3
export IOBUF_PARAMS=*:size=32M:count=4:verbose
export FORT_BUFFERED=TRUE
export MKL_CBWR=AVX
ulimit -s unlimited
ulimit -a

export ATP_ENABLED=0
export MALLOC_MMAP_MAX_=0
export MALLOC_TRIM_THRESHOLD_=134217728

export MPICH_ABORT_ON_ERROR=1
export MPICH_ENV_DISPLAY=1
export MPICH_VERSION_DISPLAY=1
export MPICH_CPUMASK_DISPLAY=1

export KMP_STACKSIZE=1024m
export OMP_NUM_THREADS=1
export KMP_AFFINITY=disabled

#export OMP_NUM_THREADS=1
export KMP_AFFINITY=disabled

export MP_EUIDEVICE=sn_all
export MP_EUILIB=us
export MP_SHARED_MEMORY=no
export MEMORY_AFFINITY=core:1

export total_tasks=21
export OMP_NUM_THREADS=1
export taskspernode=7

#Date and Cycle
#export cyc=00
#export cyc_fcst=00
#export job=Aa2016041500397
#export RUNMEM=gec00
export FORECAST_SEGMENT=lr

#export gefsmpexec_mpmd=mpirun.lsf

# export for development runs only begin
export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}
export gefsmachine=cray
export gefsmpexec=" aprun -b -j1 -n21 -N7 -d1 -cc depth "
export gefsmpexec_mpmd="  aprun -b -j1 -n21 -N7 -d1 -cc depth  cfp mpmd_cmdfile"
export APRUNC="aprun"
export aprun_gec00="aprun -b -j1 -n1 -N1 -d24 -cc depth"
export NTHREADS_SIGCHGRS=6


cd $SOURCEDIR/control
. $SOURCEDIR/control/setbase
. $SOURCEDIR/parm/gefs.parm

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_POST_GENESIS
