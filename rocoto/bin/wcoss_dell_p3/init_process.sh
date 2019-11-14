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
#export OMP_NUM_THREADS=6
#export KMP_AFFINITY=disabled

export KMP_AFFINITY=disabled

#export MP_EUIDEVICE=sn_all
#export MP_EUILIB=us
#export MP_SHARED_MEMORY=no
#export MEMORY_AFFINITY=core:6

export total_tasks=32
export OMP_NUM_THREADS=6
export taskspernode=4
export NODES=7

# export for development runs only begin
export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}

export gefsmpexec=" mpirun -n $total_tasks "
export NTHREADS_SIGCHGRS=6

export APRUN=${gefsmpexec:-mpirun}
export APRUN_CHGRES=${APRUN_CHGRES:-"mpirun -n 1 "}
export APRUN_RECENT=${APRUN_RECENT:-"mpirun -n $total_tasks "}

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_INIT_PROCESS
