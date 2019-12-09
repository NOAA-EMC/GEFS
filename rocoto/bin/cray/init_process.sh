#!/bin/ksh

# EXPORT list here
set -x
#export IOBUF_PARAMS=
#export FORT_BUFFERED=TRUE
#export MKL_CBWR=AVX
ulimit -s unlimited
ulimit -a

#export ATP_ENABLED=0
#export MALLOC_MMAP_MAX_=0
#export MALLOC_TRIM_THRESHOLD_=134217728

#export MPICH_ABORT_ON_ERROR=1
#export MPICH_ENV_DISPLAY=1
#export MPICH_VERSION_DISPLAY=1
#export MPICH_CPUMASK_DISPLAY=1

#export KMP_STACKSIZE=1024m
#export KMP_AFFINITY=disabled

#export MP_EUIDEVICE=sn_all
#export MP_EUILIB=us
#export MP_SHARED_MEMORY=no
#export MEMORY_AFFINITY=core:6

#export total_tasks=20
#export OMP_NUM_THREADS=6
#export taskspernode=4

# export for development runs only begin
#export envir=${envir:-dev}
#export RUN_ENVIR=${RUN_ENVIR:-dev}

#export gefsmpexec=" aprun -b -j1 -n32 -N4 -d6 -cc depth "
#export APRUN_CHGRES="aprun -j 1 -n 1 -N 1 -d 12 -cc depth"
#export APRUN_RECENT="aprun -j 1 -n $total_tasks -N $taskspernode -d $OMP_NUM_THREADS -cc depth"


export NTHREADS_SIGCHGRS=6

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_INIT_PROCESS
