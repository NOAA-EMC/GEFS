#!/bin/ksh
#

# EXPORT list here
set -x

ulimit -s unlimited
ulimit -a

# export for development runs only begin
export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}
export gefsmachine=cray

. $GEFS_ROCOTO/parm/setbase
. $GEFS_ROCOTO/parm/gefs_config
. $GEFS_ROCOTO/parm/gefs_dev.parm


# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_GETCFSSST

