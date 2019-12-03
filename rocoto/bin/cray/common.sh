#!/bin/ksh

#export MPI_LABELIO=YES
#export MP_STDOUTMODE=ORDERED
#export KMP_STACKSIZE=1024m

#export FORT_BUFFERED=TRUE
#export MKL_CBWR=AVX

export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}

export NTASKS=${GEFS_NTASKS}
export total_tasks=${GEFS_NTASKS}
export OMP_NUM_THREADS=${GEFS_TPP}
export taskspernode=${GEFS_PPN}

export gefsmpexec="  aprun -b -j1 -n$total_tasks -N${taskspernode} -d${OMP_NUM_THREADS} -cc depth"
export gefsmpexec_mpmd=" aprun -b -j1 -n${total_tasks} -N${taskspernode} -d${OMP_NUM_THREADS} -cc depth cfp mpmd_cmdfile"

export wavempexec="aprun -b -j1 -cc depth -n"
export wave_mpmd="cfp"

export APRUNC="$gefsmpexec"
export APRUN_CHGRES="aprun -j1 -n$total_tasks -N$taskspernode -d$OMP_NUM_THREADS -cc depth"
export APRUN_RECENT="$APRUN_CHGRES"
export aprun_gec00="$gefsmpexec"
export APRUN_CALCINC="$gefsmpexec"

