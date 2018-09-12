#!/bin/ksh
#

# EXPORT list here
set -x
export NODES=7
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
#export OMP_NUM_THREADS=2
export KMP_AFFINITY=disabled

#export OMP_NUM_THREADS=4
#export KMP_AFFINITY=disabled

#export MP_EUIDEVICE=sn_all
#export MP_EUILIB=us
#export MP_SHARED_MEMORY=yes
#export MEMORY_AFFINITY=core:2

#. /opt/modules/default/init/ksh
#module load NetCDF-intel-haswell/4.2
#module load nco-gnu-sandybridge/4.4.4

export total_tasks=20
export OMP_NUM_THREADS=7
export taskspernode=4


#export total_tasks=756

#Date and Cycle
#export cyc=00
#export PDY=20160415
#export cyc_fcst=00
#export job=Aa2016041500100
export FORECAST_SEGMENT=hr

# export for development runs only begin
export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}
export gefsmachine=wcoss_dell_p3
export gefsmpexec_mpmd="  mpirun -n 756 cfp mpmd_cmdfile"
export APRUNC="mpirun"
export APRUN_RECENT="mpirun -n $total_tasks"
export APRUN_CHGRES="mpirun -n 1"

export aprun_gec00="mpirun -n 1 "
export NTHREADS_SIGCHGRS=6


# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_INIT_RECENTER
