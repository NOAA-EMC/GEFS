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
export gefsmachine=cray
export gefsmpexec=" aprun -b -j1 -n1 -N1 -d24 -cc depth "
export gefsmpexec_mpmd="  aprun -b -j1 -n1 -N1 -d24 -cc depth  cfp mpmd_cmdfile"
export APRUNC="aprun"
export aprun_gec00="aprun -b -j1 -n1 -N1 -d24 -cc depth"
export NTHREADS_SIGCHGRS=6

cd $SOURCEDIR/control
. $SOURCEDIR/control/setbase
#. $SOURCEDIR/parm/gefs.parm

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ENSPOST

