#!/bin/ksh
#

# EXPORT list here
set -x
export NODES=4
export IOBUF_PARAMS=


ulimit -s unlimited
ulimit -a




export OMP_NUM_THREADS=2

#export OMP_NUM_THREADS=4

export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:2

. /opt/modules/default/init/ksh
module load NetCDF-intel-haswell/4.2
module load nco-gnu-sandybridge/4.4.4

export total_tasks=48
export OMP_NUM_THREADS=2
export taskspernode=12

#export total_tasks=756

#Date and Cycle
#export cyc=00
#export PDY=20160415
#export cyc_fcst=00
#export job=Aa2016041500100
export FORECAST_SEGMENT=hr

# export for development runs only begin
export gefsmachine=theia
export APRUNC="mpirun"
export aprun_gec00="mpirun -np 1"
export NTHREADS_SIGCHGRS=6

cd $SOURCEDIR/control

#. $SOURCEDIR/parm/gefs.parm

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_FV3REMAP

