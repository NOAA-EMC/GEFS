#!/bin/ksh

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
export KMP_AFFINITY=disabled

export MP_EUIDEVICE=sn_all
export MP_EUILIB=us
export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:4

export NODES=$PBS_NUM_NODES
export total_tasks=$PBS_NP
export OMP_NUM_THREADS=4
export taskspernode=$PBS_NUM_PPN

export DO_LOW_RES=

# export for development runs only begin
export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}
export gefsmpexec_mpmd="mpirun -np $total_tasks /scratch3/NCEPDEV/nwprod/util/exec/mpiserial"

export RERUN=NO

# CALL executable job script here
. $SOURCEDIR/jobs/JGEFS_PRDGEN
