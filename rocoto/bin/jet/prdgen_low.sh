#!/bin/ksh
#

# EXPORT list here
set -x
export IOBUF_PARAMS=*:size=64M:count=4:verbose
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
export OMP_NUM_THREADS=4
export KMP_AFFINITY=disabled

#export OMP_NUM_THREADS=4
export KMP_AFFINITY=disabled

export MP_EUIDEVICE=sn_all
export MP_EUILIB=us
export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:4

export NODES=1
export total_tasks=5
export OMP_NUM_THREADS=4
export taskspernode=5

export FORECAST_SEGMENT=lr
export DO_LOW_RES=

export gefsmpexec_mpmd=mpirun.lsf

# export for development runs only begin
export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}
#export gefsmpexec_mpmd="mpirun -np $total_tasks /lfs3/projects/hwrf-vd/soft/mpiserial-3.0rc1/exec/mpiserial"
export gefsmpexec_mpmd="mpirun -np $total_tasks /mnt/lfs3/projects/hfv3gfs/Bing.Fu/soft/mpiserial-3.0rc1/impi/exec/mpiserial"

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_PRDGEN
