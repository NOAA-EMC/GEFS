#!/bin/ksh
#

# EXPORT list here
set -x
export NODES=5
export IOBUF_PARAMS=
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
export OMP_NUM_THREADS=2
export KMP_AFFINITY=disabled

#export OMP_NUM_THREADS=4
export KMP_AFFINITY=disabled

export MP_EUIDEVICE=sn_all
export MP_EUILIB=us
export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:2

. /opt/modules/default/init/ksh
module load NetCDF-intel-haswell/4.2
module load nco-gnu-sandybridge/4.4.4

export total_tasks=30
export OMP_NUM_THREADS=6
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

#export gefsmpexec=" aprun -b -j1 -n1 -N12 -d2 -cc depth "
export gefsmpexec_mpmd="  aprun -b -j1 -n756 -N6 -d4 -cc depth  cfp mpmd_cmdfile"
export APRUNC="aprun"
export APRUN_RECENT="aprun -j 1 -n $total_tasks -N $taskspernode -d $OMP_NUM_THREADS -cc depth"
export APRUN_CHGRES="aprun -j 1 -n 1 -N 1 -d 12 -cc depth"

export aprun_gec00="aprun -b -j1 -n1 -N1 -d24 -cc depth"
export NTHREADS_SIGCHGRS=6

. $GEFS_ROCOTO/parm/setbase
. $GEFS_ROCOTO/parm/gefs_config
. $GEFS_ROCOTO/parm/gefs_dev.parm


# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_INIT_RECENTER

