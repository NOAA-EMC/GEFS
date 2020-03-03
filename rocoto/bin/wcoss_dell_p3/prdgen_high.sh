#!/bin/ksh
#

# EXPORT list here
set -x

#export IOBUF_PARAMS=*:size=64M:count=4:verbose
ulimit -s unlimited
ulimit -a

#export OMP_NUM_THREADS=${GEFS_TPP:-4}

#export DO_LOW_RES=

# export gefsmpexec_mpmd="  mpirun -n $total_tasks cfp mpmd_cmdfile"

#export RERUN=NO

# CALL executable job script here
. $SOURCEDIR/jobs/JGEFS_PRDGEN
