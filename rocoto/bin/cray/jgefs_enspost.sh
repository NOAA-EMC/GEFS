#!/bin/ksh

# EXPORT list here
set -x
export IOBUF_PARAMS=*pgrb2*:size=64M:count=4:verbose,*enspost_grb2*:size=64M:count=4:verbose,*ensstat_grb2*:size=64M:count=4:verbose,*pq?f*:size=64M:count=4:verbose
export FORT_BUFFERED=TRUE

# export for development runs only begin
export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}
export expid=${EXPID}

# CALL executable job script here
. $GEFS_ROCOTO/parm/setbase
. $GEFS_ROCOTO/parm/gefs_config
. $GEFS_ROCOTO/parm/gefs_dev.parm

$SOURCEDIR/jobs/JGEFS_ENSPOST
