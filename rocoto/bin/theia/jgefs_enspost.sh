#!/bin/ksh

# EXPORT list here
set -x
export NODES=1
export IOBUF_PARAMS=*pgrb2*:size=64M:count=4:verbose,*enspost_grb2*:size=64M:count=4:verbose,*ensstat_grb2*:size=64M:count=4:verbose,*pq?f*:size=64M:count=4:verbose
export FORT_BUFFERED=TRUE

#Date and Cycle
#export cyc=00
#export cyc_fcst=00
#export job=Aa2016041500395
#export RUNMEM=gec00
export FORECAST_SEGMENT=lr

#export gefsmpexec_mpmd=mpirun.lsf

# export for development runs only begin
export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}
export gefsmachine=theia
export gefsmpexec="mpirun -np $total_tasks"
export gefsmpexec_mpmd="mpirun -np $total_tasks /scratch3/NCEPDEV/nwprod/util/exec/mpiserial"
export APRUNC="mpirun"
export aprun_gec00="mpirun -np 1"
export NTHREADS_SIGCHGRS=6

cd $SOURCEDIR/control

#. $SOURCEDIR/parm/gefs.parm

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ENSPOST

