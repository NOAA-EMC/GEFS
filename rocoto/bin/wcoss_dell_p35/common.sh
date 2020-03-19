#!/bin/ksh

export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}

#export NTASKS=${GEFS_NTASKS}
export total_tasks=${GEFS_NTASKS}
export OMP_NUM_THREADS=${GEFS_TPP}
export taskspernode=${GEFS_PPN}

# Calculate the number of tasks based on the task geometry
(( NTASKS=$(echo $LSB_PJL_TASK_GEOMETRY | grep -Po "\d+" | sort -n | tail -1) + 1 ))
export NTASKS

export gefsmpexec="mpirun -n $NTASKS"
export gefsmpexec_mpmd="mpirun -n $NTASKS cfp mpmd_cmdfile"
export wavempexec="mpirun -n"
export wave_mpmd="cfp"

#export APRUNC="$gefsmpexec"
#export APRUN_RECENT="$gefsmpexec"
export APRUN_CHGRES="mpirun -n 1"
#export aprun_gec00="mpirun -n 1"
export APRUN_CALCINC="mpirun -n 1"

. $GEFS_ROCOTO/parm/setbase
. $GEFS_ROCOTO/parm/gefs_config
. $GEFS_ROCOTO/parm/gefs_dev.parm
