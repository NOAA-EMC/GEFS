#!/bin/ksh

# EXPORT list here
set -x

#export IOBUF_PARAMS=cfi*:size=64M:count=4:verbose
ulimit -s unlimited
ulimit -a

#export ATP_ENABLED=0
#export MALLOC_MMAP_MAX_=0
#export MALLOC_TRIM_THRESHOLD_=134217728

#export MPICH_ABORT_ON_ERROR=1
#export MPICH_ENV_DISPLAY=1
#export MPICH_VERSION_DISPLAY=1
#export MPICH_CPUMASK_DISPLAY=1
#
#export KMP_AFFINITY=disabled

#export MP_EUIDEVICE=sn_all
#export MP_EUILIB=us
#export MP_SHARED_MEMORY=no
#export MEMORY_AFFINITY=core:4

#export OMP_NUM_THREADS=${GEFS_TPP:-4}

# export gefsmpexec_mpmd="mpirun -n $total_tasks cfp mpmd_cmdfile"

# CALL executable job script here
. $SOURCEDIR/jobs/JGEFS_ENSSTAT
