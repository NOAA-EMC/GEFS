#!/bin/ksh
#

# EXPORT list here
set -x
export IOBUF_PARAMS=


ulimit -s unlimited
ulimit -a




export MP_SHARED_MEMORY=no
export MEMORY_AFFINITY=core:6

export total_tasks=32
export OMP_NUM_THREADS=6
export taskspernode=4

#. /opt/modules/default/init/ksh
#module load NetCDF-intel-haswell/4.2
#module load nco-gnu-sandybridge/4.4.4

export total_tasks=32
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



#export APRUNC="aprun"
export APRUNC="mpirun -np 1"

#export APRUN_RECENT="aprun -j 1 -n $total_tasks -N $taskspernode -d $OMP_NUM_THREADS -cc depth"
export APRUN_RECENT="mpirun -np $total_tasks"
#export APRUN_RECENT="mpirun -np 1"

#export APRUN_CHGRES="aprun -j 1 -n 1 -N 1 -d 12 -cc depth"
export APRUN_CHGRES="mpirun -np 1"

#export aprun_gec00="aprun -b -j1 -n1 -N1 -d24 -cc depth"
export aprun_gec00="mpirun -np 1"

export APRUN_CALCINC="mpirun -np 1"

export NTHREADS_SIGCHGRS=6

. $GEFS_ROCOTO/parm/setbase
. $GEFS_ROCOTO/parm/gefs_config
. $GEFS_ROCOTO/parm/gefs_dev.parm


# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_INIT_RECENTER

