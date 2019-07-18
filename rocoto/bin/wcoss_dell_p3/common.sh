#!/bin/ksh

export MPI_LABELIO=YES
export MP_STDOUTMODE=ORDERED
export KMP_STACKSIZE=1024m

export FORT_BUFFERED=TRUE
export MKL_CBWR=AVX

export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}

# Calculate the number of tasks based on the task geometry
(( NTASKS=$(echo $LSB_PJL_TASK_GEOMETRY | grep -Po "\d+" | sort -n | tail -1) + 1 ))
export NTASKS

export gefsmpexec="mpirun -n $NTASKS"
export gefsmpexec_mpmd="mpirun -n $NTASKS cfp mpmd_cmdfile"
export wavempexec="mpirun -n"
export wave_mpmd="cfp"

export APRUNC="$gefsmpexec"
export APRUN_RECENT="gefsmpexec"
export APRUN_CHGRES="mpirun -n 1"
export aprun_gec00="mpirun -n 1"
export APRUN_CALCINC="mpirun -n 1"

export errchk="eval if [[ \$err != 0 ]]; then exit \$err; fi"
