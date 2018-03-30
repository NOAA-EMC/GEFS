#!/bin/ksh

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
export KMP_STACKSIZE=1024m
export OMP_NUM_THREADS=1
export KMP_AFFINITY=disabled

export MP_EUIDEVICE=sn_all
export MP_EUILIB=us
export MP_SHARED_MEMORY=no
export MEMORY_AFFINITY=core:1

export gefsmpexec_mpmd="  aprun -b -j1 -n40 -N20 -d1 -cc depth  cfp mpmd_cmdfile"

# export for development runs only begin
export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}
export expid=${EXPID}
export SOURCEDIR=${SOURCEDIR}
export WORKDIR=${WORKDIR}

# CALL executable job script here
. $GEFS_ROCOTO/parm/setbase
. $GEFS_ROCOTO/parm/gefs_config
. $GEFS_ROCOTO/parm/gefs_dev.parm

$SOURCEDIR/jobs/JGEFS_ENKF_TRACK
