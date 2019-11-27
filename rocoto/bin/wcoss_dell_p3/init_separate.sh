#!/bin/ksh

# EXPORT list here
set -x
ulimit -s unlimited
ulimit -a

# export MPI_LABELIO=YES
# export MP_STDOUTMODE=ORDERED
export KMP_STACKSIZE=2048M # Overrides common.sh
export KMP_AFFINITY=scatter

# export envir=${envir:-dev}
# export RUN_ENVIR=${RUN_ENVIR:-dev}

# export NODES=10
# export total_tasks=40
export OMP_NUM_THREADS=6
# export taskspernode=4

# export gefsmpexec_mpmd="  mpirun -n $total_tasks cfp mpmd_cmdfile"
# export aprun_gec00="mpirun -n 1"
# export APRUNC="mpirun"

# export for development runs only begin

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_INIT_SEPARATE
