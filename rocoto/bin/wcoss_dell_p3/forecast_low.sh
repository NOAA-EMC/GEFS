#!/bin/ksh
#

# EXPORT list here
set -x
#export IOBUF_PARAMS=
#export FORT_BUFFERED=TRUE
#export MKL_CBWR=AVX
ulimit -s unlimited
ulimit -a

#export ATP_ENABLED=0
#export MALLOC_MMAP_MAX_=0
#export MALLOC_TRIM_THRESHOLD_=134217728

#export MPICH_ABORT_ON_ERROR=1
#export MPICH_ENV_DISPLAY=1
#export MPICH_VERSION_DISPLAY=1
#export MPICH_CPUMASK_DISPLAY=1

#export KMP_STACKSIZE=1024m
#export KMP_AFFINITY=disabled

#export OMP_NUM_THREADS=4
#export KMP_AFFINITY=disabled

#export MP_EUIDEVICE=sn_all
#export MP_EUILIB=us
#export MP_SHARED_MEMORY=yes
#export MEMORY_AFFINITY=core:2

export MPI_LABELIO=YES
export MP_STDOUTMODE=ORDERED
export KMP_STACKSIZE=2048M
export KMP_AFFINITY=scatter

export FORECAST_SEGMENT=lr

# export for development runs only begin
export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}

export gefsmpexec=" mpirun -n 456 "
#export NTHREADS_SIGCHGRS=2

# For Restart
export RERUN=RESTART  #(the J-job script has default value "RERUN"
export FHINI=03    #(forecast lead hour at which your want to restart the fcst)
export RFNDATE=YES  #(If there is prefix in the restart file names, use YES)
#export warm_start=.true.
#export restart_run=.true.
#export output_1st_tstep_rst=.true.
#export restart_hour=3
export fhrestart=48

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_FORECAST_FV3

