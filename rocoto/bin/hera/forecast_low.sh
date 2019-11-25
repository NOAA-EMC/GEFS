#!/bin/ksh
#

# EXPORT list here
set -x
export IOBUF_PARAMS=


ulimit -s unlimited
ulimit -a

export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:2

export NTHREADS_SIGCHGRS=2

#export FORECAST_SEGMENT=lr

export memdir_template='$ROTDIR/enkf.$CDUMP.$PDY/$cyc'


# For Restart
export RERUN=RESTART  #(the J-job script has default value "RERUN"
export FHINI=03    #(forecast lead hour at which your want to restart the fcst)
export RFNDATE=YES  #(If there is prefix in the restart file names, use YES)
#export warm_start=.true.
#export restart_run=.true.
#export output_1st_tstep=.true.
#export restart_hour=3
export fhrestart=48

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_FORECAST_FV3

