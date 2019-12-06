#!/bin/ksh
#

# EXPORT list here
set -x
#export NODES=${GEFS_NODES:-5}

ulimit -s unlimited
ulimit -a

#export KMP_AFFINITY=disabled

#export OMP_NUM_THREADS=${GEFS_TPP:-6}

export FORECAST_SEGMENT=hr

# export gefsmpexec_mpmd="  mpirun -n $total_tasks cfp mpmd_cmdfile"
# export gefsmpexec=mpirun 

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ENSAVG_NEMSIO
