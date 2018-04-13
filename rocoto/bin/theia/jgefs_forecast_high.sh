#!/bin/ksh
#

# EXPORT list here
set -x
export IOBUF_PARAMS=
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

export MP_EUIDEVICE=sn_all
export MP_EUILIB=us
export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:2

export NTHREADS_SIGCHGRS=2

export FORECAST_SEGMENT=hr

export memdir_template='$ROTDIR/enkf.$CDUMP.$PDY/$cyc'

export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}
<<<<<<< HEAD
export gefsmpexec="mpirun -np $PBS_NP"
=======
export gefsmpexec="mpirun -np $total_tasks"
>>>>>>> 24e6f308dda50ebb1ff37693c9a6e193d44c7b6e

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_FORECAST_FV3

